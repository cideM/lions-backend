// Package blackbox runs the Hurl files in test/hurl against a real server.
//
// Every *.hurl file gets its own freshly started server and its own copy of
// the fixture database (migrations plus fixture/seed.sql), so files are
// independent and run in parallel. Optional sidecar files next to a Hurl file:
//
//	NAME.pre.sql  executed against the database before the server starts
//	NAME.vars     key=value lines passed to Hurl as variables
//	NAME.env      KEY=value lines added to the server's environment
//
// The server under test is chosen with environment variables:
//
//	LIONS_TEST_CMD    a command run on the host. It gets the LIONS_* variables
//	                  plus LIONS_PORT and must serve HTTP on that port.
//	LIONS_TEST_IMAGE  otherwise, a Docker image whose /opt/app/run.sh serves
//	                  port 3000 (default lions-test, the Haskell app).
//
// Run with `go test ./test/` from the repository root; hurl must be in PATH.
package blackbox

import (
	"bytes"
	"database/sql"
	"fmt"
	"net"
	"net/http"
	"os"
	"os/exec"
	"path/filepath"
	"sort"
	"strings"
	"sync"
	"syscall"
	"testing"
	"time"

	_ "modernc.org/sqlite"
)

// Keys matching the Firebase scrypt test vector used in fixture/seed.sql.
var serverEnv = map[string]string{
	"LIONS_SCRYPT_SIGNER_KEY": "jxspr8Ki0RYycVU8zykbdLGjFQ3McFUH0uiiTvC8pVMXAn210wjLNmdZJzxUECKbm0QsEmYUSDzZvpjeJ9WmXA==",
	"LIONS_SCRYPT_SALT_SEP":   "Bw==",
	"LIONS_LOG_LEVEL":         "info",
	"LIONS_ENV":               "test",
	"LIONS_EMAIL_MODE":        "log",
}

func TestHurl(t *testing.T) {
	hurl, err := exec.LookPath("hurl")
	if err != nil {
		t.Fatal("hurl not found in PATH; run inside the nix dev shell")
	}

	fixture := buildFixture(t)

	files, err := filepath.Glob("hurl/*.hurl")
	if err != nil || len(files) == 0 {
		t.Fatalf("no hurl files found: %v", err)
	}
	for _, file := range files {
		name := strings.TrimSuffix(filepath.Base(file), ".hurl")
		t.Run(name, func(t *testing.T) {
			t.Parallel()
			runFile(t, hurl, fixture, file)
		})
	}
}

// buildFixture creates the template database: all migrations applied, the
// go-migrate bookkeeping table written, and seed.sql loaded.
func buildFixture(t *testing.T) string {
	t.Helper()
	path := filepath.Join(t.TempDir(), "fixture.db")
	db := openDB(t, path)
	defer db.Close()

	migrations, err := filepath.Glob("../backend/migrations/*.up.sql")
	if err != nil || len(migrations) == 0 {
		t.Fatalf("no migrations found: %v", err)
	}
	sort.Strings(migrations)
	version := ""
	for _, m := range migrations {
		execFile(t, db, m)
		version = strings.SplitN(filepath.Base(m), "_", 2)[0]
	}
	execSQL(t, db, fmt.Sprintf(
		"create table if not exists schema_migrations (version uint64, dirty bool);"+
			"insert into schema_migrations (version, dirty) values (%s, 0);",
		strings.TrimLeft(version, "0")))
	execFile(t, db, "fixture/seed.sql")
	return path
}

func runFile(t *testing.T, hurl, fixture, file string) {
	base := strings.TrimSuffix(file, ".hurl")
	work := t.TempDir()
	dbPath := filepath.Join(work, "sqlite.db")
	copyFile(t, fixture, dbPath)

	if pre := base + ".pre.sql"; exists(pre) {
		db := openDB(t, dbPath)
		execFile(t, db, pre)
		db.Close()
	}

	env := map[string]string{}
	for k, v := range serverEnv {
		env[k] = v
	}
	for k, v := range readPairs(t, base+".env") {
		env[k] = v
	}

	port := freePort(t)
	url := fmt.Sprintf("http://localhost:%d", port)
	logs, stop := startServer(t, work, port, env)
	defer stop()
	waitForServer(t, url+"/login", logs)

	args := []string{
		"--test", "--error-format", "long", "--file-root", "hurl/files",
		"--variable", "base=" + url,
	}
	for k, v := range readPairs(t, base+".vars") {
		args = append(args, "--variable", k+"="+v)
	}
	args = append(args, file)

	out, err := exec.Command(hurl, args...).CombinedOutput()
	if err != nil {
		t.Errorf("hurl failed:\n%s\n--- server log ---\n%s", out, tail(logs.String(), 40))
	} else if testing.Verbose() {
		t.Logf("%s", out)
	}
}

// startServer starts the server under test and returns its log buffer and a
// function that stops it.
func startServer(t *testing.T, work string, port int, env map[string]string) (*lockedBuffer, func()) {
	t.Helper()
	logs := &lockedBuffer{}

	if command := os.Getenv("LIONS_TEST_CMD"); command != "" {
		cmd := exec.Command("sh", "-c", command)
		cmd.Env = os.Environ()
		for k, v := range env {
			cmd.Env = append(cmd.Env, k+"="+v)
		}
		cmd.Env = append(cmd.Env,
			"LIONS_SQLITE_PATH="+filepath.Join(work, "sqlite.db"),
			"LIONS_SESSION_KEY_FILE="+filepath.Join(work, "session.aes"),
			fmt.Sprintf("LIONS_PORT=%d", port),
		)
		cmd.Stdout, cmd.Stderr = logs, logs
		cmd.SysProcAttr = &syscall.SysProcAttr{Setpgid: true}
		if err := cmd.Start(); err != nil {
			t.Fatalf("starting server: %v", err)
		}
		return logs, func() {
			_ = syscall.Kill(-cmd.Process.Pid, syscall.SIGTERM)
			_ = cmd.Wait()
		}
	}

	image := os.Getenv("LIONS_TEST_IMAGE")
	if image == "" {
		image = "lions-test"
	}
	name := fmt.Sprintf("lions-test-%d-%d", os.Getpid(), port)
	args := []string{
		"run", "--rm", "--name", name,
		"-p", fmt.Sprintf("127.0.0.1:%d:3000", port),
		"-v", work + ":/data",
		"-e", "LIONS_SQLITE_PATH=/data/sqlite.db",
		"-e", "LIONS_SESSION_KEY_FILE=/data/session.aes",
		"-e", "LIONS_SERVER_LISTEN_ADDR=0.0.0.0",
	}
	for k, v := range env {
		args = append(args, "-e", k+"="+v)
	}
	args = append(args, "--entrypoint", "/opt/app/run.sh", image)
	cmd := exec.Command("docker", args...)
	cmd.Stdout, cmd.Stderr = logs, logs
	if err := cmd.Start(); err != nil {
		t.Fatalf("starting docker: %v", err)
	}
	return logs, func() {
		_ = exec.Command("docker", "stop", "-t", "2", name).Run()
		_ = cmd.Wait()
	}
}

func waitForServer(t *testing.T, url string, logs *lockedBuffer) {
	t.Helper()
	client := &http.Client{Timeout: time.Second, CheckRedirect: func(*http.Request, []*http.Request) error {
		return http.ErrUseLastResponse
	}}
	deadline := time.Now().Add(30 * time.Second)
	for time.Now().Before(deadline) {
		if resp, err := client.Get(url); err == nil {
			resp.Body.Close()
			return
		}
		time.Sleep(100 * time.Millisecond)
	}
	t.Fatalf("server did not come up at %s\n--- server log ---\n%s", url, tail(logs.String(), 40))
}

func openDB(t *testing.T, path string) *sql.DB {
	t.Helper()
	db, err := sql.Open("sqlite", path)
	if err != nil {
		t.Fatalf("opening %s: %v", path, err)
	}
	return db
}

func execFile(t *testing.T, db *sql.DB, path string) {
	t.Helper()
	content, err := os.ReadFile(path)
	if err != nil {
		t.Fatal(err)
	}
	if _, err := db.Exec(string(content)); err != nil {
		t.Fatalf("executing %s: %v", path, err)
	}
}

func execSQL(t *testing.T, db *sql.DB, query string) {
	t.Helper()
	if _, err := db.Exec(query); err != nil {
		t.Fatalf("executing %q: %v", query, err)
	}
}

// readPairs reads key=value lines from an optional file.
func readPairs(t *testing.T, path string) map[string]string {
	t.Helper()
	pairs := map[string]string{}
	content, err := os.ReadFile(path)
	if os.IsNotExist(err) {
		return pairs
	}
	if err != nil {
		t.Fatal(err)
	}
	for _, line := range strings.Split(string(content), "\n") {
		line = strings.TrimSpace(line)
		if line == "" || strings.HasPrefix(line, "#") {
			continue
		}
		k, v, ok := strings.Cut(line, "=")
		if !ok {
			t.Fatalf("%s: expected key=value, got %q", path, line)
		}
		pairs[k] = v
	}
	return pairs
}

func copyFile(t *testing.T, from, to string) {
	t.Helper()
	content, err := os.ReadFile(from)
	if err != nil {
		t.Fatal(err)
	}
	// World writable so that the Docker container, running as root with a
	// bind mount, can write to it too.
	if err := os.WriteFile(to, content, 0o666); err != nil {
		t.Fatal(err)
	}
}

func exists(path string) bool {
	_, err := os.Stat(path)
	return err == nil
}

func freePort(t *testing.T) int {
	t.Helper()
	l, err := net.Listen("tcp", "127.0.0.1:0")
	if err != nil {
		t.Fatal(err)
	}
	defer l.Close()
	return l.Addr().(*net.TCPAddr).Port
}

func tail(s string, n int) string {
	lines := strings.Split(strings.TrimRight(s, "\n"), "\n")
	if len(lines) > n {
		lines = lines[len(lines)-n:]
	}
	return strings.Join(lines, "\n")
}

type lockedBuffer struct {
	mu  sync.Mutex
	buf bytes.Buffer
}

func (b *lockedBuffer) Write(p []byte) (int, error) {
	b.mu.Lock()
	defer b.mu.Unlock()
	return b.buf.Write(p)
}

func (b *lockedBuffer) String() string {
	b.mu.Lock()
	defer b.mu.Unlock()
	return b.buf.String()
}

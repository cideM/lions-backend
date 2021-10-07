# Hi!

## Deploy

Linux: `deploy .`

Darwin Intel: `./scripts/deploy-docker`

Darwin M1: Use CI

## SQLite

There's no more seed data in `.sql` files since the QEMU VM has hardcoded data
and for everything else you can just use Litestream to pull down production.

### Running Migrations

```shell
$ migrate -path migrations -database "sqlite3://$LIONS_SQLITE_PATH" up
1/u users (23.971333ms)
2/u welcome (44.808037ms)
3/u user_roles (66.688333ms)
4/u users_email_index (87.838135ms)
$ migrate create -ext sql -dir migrations/ -seq users_email_index
/home/tifa/lions-backend/migrations/000004_users_email_index.up.sql
/home/tifa/lions-backend/migrations/000004_users_email_index.down.sql
```

### Creating Migrations

```
$ migrate create -ext sql -dir migrations/ -seq events
/home/tifa/lions-backend/migrations/000007_events.up.sql
/home/tifa/lions-backend/migrations/000007_events.down.sql
```

### Litestream

When opening S3 from the Administrator account, keep in mind that the user
doesn't have access to the S3 bucket. You can only access it through the
Litestream user. Use the shell below, together with values from either `.envrc`
or 1Password to check which replicas there are.

There's a `prod` and a `dev-db` prefix.

```shell
$ AWS_ACCESS_KEY_ID= AWS_SECRET_ACCESS_KEY= aws s3 ls lions-achern-litestream-replica-1/
```

Check all generations that exist:

```shell
litestream generations s3://lions-achern-litestream-replica-1/prod/
```

Restore the DB **from** S3 **to** a local file on the server. This process is
pretty dangerous. First of all, files can only be accessed by the dynamic user
created by systemd. You can override this with `sudo` but then you need to
`chown` the files afterwards. Also, be sure to delete all DB files, not just
the DB itself, also the WAL and the `.db-litestream` I guess?

```shell
[admin@lions-server:~]$ sudo rm /var/lib/lions-server/db /var/lib/lions-server/.db-litestream
[admin@lions-server:~]$ sudo LITESTREAM_ACCESS_KEY_ID= LITESTREAM_SECRET_ACCESS_KEY= litestream restore -o /var/lib/lions-server/db s3://lions-achern-litestream-replica-1/dev-db
[admin@lions-server:~]$ sudo chown lions-server:lions /var/lib/lions-server/db
```

Here's how to get prod DB onto local, if you already have a valid `.envrc` setup

`litestream restore -o $LIONS_SQLITE_PATH s3://lions-achern-litestream-replica-1/prod`

## Development

Rebuild and restart when source changes. This may or may not work reliably on MacOS.

```shell
$ fd . -e hs -e purs | entr -cr ./scripts/dev.hs
```

Check source files:
```shell
$ cd backend
$ ghcid --no-height-limit --clear --reverse
```

## Tests

Check test files:

```shell
$ cd backend
$ ghcid --no-height-limit --clear --reverse --target=test:tests
```

This will recompile and run the test binary, it's not using Cabal's test running facilities.

```shell
$ cd backend
$ fd -e hs | entr -c cabal v2-run test:tests
```

This will run the tests through cabal but it seems to generate less output. But
that output is useful, so prefer the above snippet.

```shell
$ cd backend
$ fd -e hs | entr -c cabal v2-test
```

### End-To-End

There's one simple end-to-end test which only works on Linux. You can make this
work on MacOS as well by running QEMU in Docker. Probably won't work for M1
though.

There's a memory leak hardcoded into the E2E test in GitHub CI. Reason being
that killing the server process also kills the entire GitHub worker. I don't
know if this is a Haskell bug or something about how `run: ` works.

### QEMU

Just run `lions-vm`.

Hit `CTRL-A X` to quit.

Login is `root` and an empty password.

Use `$ ssh -o StrictHostKeyChecking=no root@localhost -p 2221 systemctl status` to run commands.
Disabling the check is necessary because whenever you delete and recreate the
VM the `known_hosts` needs to be updated.

Last time I updated the flake I broke CI. The QEMU image seems to be stuck at
"Booting from ROM..." but GitHub actions logs seem a bit strange, since they
sometimes show certain logs and sometimes they don't. So there might be obvious
errors that I'm just not seeing. The QEMU image definitely works locally.

I compared the QEMU invocation scripts but I was too lazy to really look into it.

Here's the old one that worked at some point:
```text
#! /nix/store/9ywr69qi622lrmx5nn88gk8jpmihy0dz-bash-4.4-p23/bin/bash

NIX_DISK_IMAGE=$(readlink -f ${NIX_DISK_IMAGE:-./lions-server.qcow2})

if ! test -e "$NIX_DISK_IMAGE"; then
    /nix/store/rwbwjmpjw6yc19ya0l66ivrdj7nk9qnk-qemu-host-cpu-only-for-vm-tests-5.2.0/bin/qemu-img create -f qcow2 "$NIX_DISK_IMAGE" \
      512M || exit 1
fi

# Create a directory for storing temporary data of the running VM.
if [ -z "$TMPDIR" -o -z "$USE_TMPDIR" ]; then
    TMPDIR=$(mktemp -d nix-vm.XXXXXXXXXX --tmpdir)
fi

# Create a directory for exchanging data with the VM.
mkdir -p $TMPDIR/xchg



cd $TMPDIR
idx=0


# Start QEMU.
exec /nix/store/rwbwjmpjw6yc19ya0l66ivrdj7nk9qnk-qemu-host-cpu-only-for-vm-tests-5.2.0/bin/qemu-kvm -cpu max \
    -name lions-server \
    -m 384 \
    -smp 1 \
    -device virtio-rng-pci \
    -netdev user,id=user.0,${QEMU_NET_OPTS:+$QEMU_NET_OPTS} \
    -virtfs local,path=/nix/store,security_model=none,mount_tag=store \
    -virtfs local,path=$TMPDIR/xchg,security_model=none,mount_tag=xchg \
    -virtfs local,path=${SHARED_DIR:-$TMPDIR/xchg},security_model=none,mount_tag=shared \
    -drive cache=writeback,file=$NIX_DISK_IMAGE,id=drive1,if=none,index=1,werror=report -device virtio-blk-pci,drive=drive1 \
    -usb -device usb-tablet,bus=usb-bus.0 -kernel /nix/store/slv0s03g2sirb7n26a4pvkllq9d5l0aj-nixos-system-lions-server-20.09pre-git/kernel -initrd /nix/store/slv0s03g2sirb7n26a4pvkllq9d5l0aj-nixos-system-lions-server-20.09pre-git/initrd -append "$(cat /nix/store/slv0s03g2sirb7n26a4pvkllq9d5l0aj-nixos-system-lions-server-20.09pre-git/kernel-params) init=/nix/store/slv0s03g2sirb7n26a4pvkllq9d5l0aj-nixos-system-lions-server-20.09pre-git/init regInfo=/nix/store/r86zwlb8s2f53wx0yab1zw70w7w7hlab-closure-info/registration console=tty0 console=ttyS0,115200n8 $QEMU_KERNEL_PARAMS" -nographic \
    $QEMU_OPTS \
    "$@"
```

And the new one:
```text
#! /nix/store/qfb4j7w2fjjq953nd9xncz5mymj5n0kb-bash-5.1-p8/bin/bash

set -e

NIX_DISK_IMAGE=$(readlink -f "${NIX_DISK_IMAGE:-./lions-server.qcow2}")

if ! test -e "$NIX_DISK_IMAGE"; then
    /nix/store/9d288g1vld0jwfzrmfm238wfcsdz5z8i-qemu-6.1.0/bin/qemu-img create -f qcow2 "$NIX_DISK_IMAGE" \
      512M
fi

# Create a directory for storing temporary data of the running VM.
if [ -z "$TMPDIR" ] || [ -z "$USE_TMPDIR" ]; then
    TMPDIR=$(mktemp -d nix-vm.XXXXXXXXXX --tmpdir)
fi

# Create a directory for exchanging data with the VM.
mkdir -p "$TMPDIR/xchg"



cd "$TMPDIR"




# Start QEMU.
exec /nix/store/9d288g1vld0jwfzrmfm238wfcsdz5z8i-qemu-6.1.0/bin/qemu-kvm -cpu max \
    -name lions-server \
    -m 384 \
    -smp 1 \
    -device virtio-rng-pci \
    -net nic,netdev=user.0,model=virtio -netdev user,id=user.0,hostfwd=tcp::8080-:80,hostfwd=tcp::8081-:443,hostfwd=tcp::2221-:22,${QEMU_NET_OPTS:+,$QEMU_NET_OPTS} \
    -virtfs local,path=/nix/store,security_model=none,mount_tag=nix-store \
    -virtfs local,path="${SHARED_DIR:-$TMPDIR/xchg}",security_model=none,mount_tag=shared \
    -virtfs local,path="$TMPDIR"/xchg,security_model=none,mount_tag=xchg \
    -drive cache=writeback,file="$NIX_DISK_IMAGE",id=drive1,if=none,index=1,werror=report -device virtio-blk-pci,drive=drive1 \
    -usb \
    -device usb-tablet,bus=usb-bus.0 \
    -kernel /nix/store/n2is7mhax0h8w5rz4z287nxb5sqybd8l-nixos-system-lions-server-21.11pre-git/kernel \
    -initrd /nix/store/n2is7mhax0h8w5rz4z287nxb5sqybd8l-nixos-system-lions-server-21.11pre-git/initrd \
    -append "$(cat /nix/store/n2is7mhax0h8w5rz4z287nxb5sqybd8l-nixos-system-lions-server-21.11pre-git/kernel-params) init=/nix/store/n2is7mhax0h8w5rz4z287nxb5sqybd8l-nixos-system-lions-server-21.11pre-git/init regInfo=/nix/store/8m6h3j6iwyf9ivj3521xbzmkhgybwlj9-closure-info/registration console=tty0 console=ttyS0,115200n8 $QEMU_KERNEL_PARAMS" \
    -nographic \
    $QEMU_OPTS \
    "$@"
```

## TODO

### Code

- The modules are super granular, which is annoying. I don't like Haskell
  modules but overall it's easier to work with fewer, slightly bigger modules.
- Layouting is very ad-hoc
- The project would benefit from more tests
- Not all modules were cleaned up when I added the `mtl` everywhere. So some of
  the code might still be a little rough around the edges.
- I think I'd like to go back to no `mtl` in the end. It's simpler, just a bit
  more typing, less foot guns.
- Routing would benefit from more type safety and more uniformity. Maybe define
  a result that route handlers can return which also allows errors. That way
  route handlers are easier to test and it's clearer to see what the possible
  returned responses are

### UX

- URLs are plain text in news feed
- Profile pictures are missing
- I'd like to move away from OneDrive for sharing files. A very light weight
  file management system would be super cool.
- No push notifications for monitoring, only systemd logs on server

## Architecture

I'll hopefully spend more time on this section in the future. But here's the gist.

Configuration is propagated through ReaderT. Handlers generally return HTML,
but sometimes they also redirect. It's using WAI/Warp under the hood, so
nothing fancy. `mtl` is used only locally, so no error hierarchies. Right now
modules are tiny, but I'd like to go back to bigger modules. In general one
module per type. No special DB wrappers unless a record has more than 10
fields, which SQLite has no default instances for.

Configuration is read from environment variables. I'd prefer a file in the future.

Emails are sent through AWS. Email sending is replaced with a dummy in QEMU.
Outside of QEMU, if you start the server directly, you need to have AWS
credentials in the environment.

File storage happens on S3. Only files that can be uploaded right now are event
attachments.

### Modules

Haskell modules suck. It's hard to get all the following:

1. Import a single module rather than 10, all relating to `User`
2. No need to think about import cycles
3. No redundant names like Foo.Bar.Bar
4. Granular modules at the source level
5. No string prefixes for namspaces

You can either stuff everything in one module (gives you 1, 2 and 3), or make
everything extremely granular (2, 4, 5) but you won't get all of it.

I went for the very granular approach. I still had an import cycle which forced
me to very awkwardly split functions and types in one place, but other than
that it's OK. I could try to re-export some modules to reduce the number of
required imports, but that's not trivial, since you need to watch our for
conflicts when different submodules define the same names, such as `Id`.

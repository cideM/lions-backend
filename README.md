# Website for the LIONS Club Achern, Germany

This website used to be a [playground for functional programming](https://www.fbrs.io/fp/) but I've since removed a few of the more niche technologies, such as:
* NixOS
* Nix for building the apps
* Purescript
* SOPS

Right now, the technology stack includes:
* Haskell
* Tiny bit of Javascript
* Twitter Bootstrap for CSS (TODO: Replace with vanilla CSS)
* Docker
* Fly.io
* SQLite + Litestream
* `go-migrate` to generate and run migrations
* AWS SES for emails (user account creation and password retrieval)
* AWS S3 for SQLite backups
* Netlify takes care of DNS
* AWS Route53 for the domain

## Quickstart

Make sure to have a `.envrc` file, like shown below. The secrets are in 1Password.

```text
use flake
PATH_add ./client/node_modules/.bin
export LITESTREAM_ACCESS_KEY_ID=
export LITESTREAM_SECRET_ACCESS_KEY=
export LITESTREAM_BUCKET=lions-achern-litestream-replica-1
export LITESTREAM_REPLICATE_PATH=local-macbook-1
export LITESTREAM_RESTORE_PATH=prod

export LIONS_SQLITE_PATH=$XDG_DATA_HOME/lions/db
export LIONS_SESSION_KEY_FILE=$XDG_DATA_HOME/lions/session.aes
export LIONS_ENV=development
export LIONS_SERVER_LISTEN_ADDR=127.0.0.1
export LIONS_SCRYPT_SIGNER_KEY=
export LIONS_SCRYPT_SALT_SEP=
export LIONS_AWS_SES_ACCESS_KEY=
export LIONS_AWS_SES_SECRET_ACCESS_KEY=

export AWS_PROFILE="lions-shared-admin"
export AWS_DEFAULT_REGION="eu-central-1"
```

Then you can just do `docker compose up --build` and everything should just work.

### Email in development

Setting `LIONS_EMAIL_MODE=log` writes outgoing email (currently only the
password reset link) to the log instead of sending it through SES. The SES
credentials are then optional. The black box tests run with this setting.

### Maintenance mode

Setting `LIONS_MAINTENANCE=1` makes the server answer every request with a
503 and a short maintenance notice. Static files are still served so the
notice renders with the usual layout. In production, toggle it with
`flyctl secrets set LIONS_MAINTENANCE=1` and `flyctl secrets unset LIONS_MAINTENANCE`,
both of which restart the machines with the new value.

## Tests

`go test ./test/` runs the black box suite in `test/hurl/` against the Docker
image `lions-test` (build it with `docker build -t lions-test .`). Every Hurl
file gets its own server and its own copy of the fixture database, which is
built from the migrations and `test/fixture/seed.sql`. See `test/README.md`
for how the assertions are written and the header of `test/blackbox_test.go`
for the sidecar files and how to point the harness at a different server,
such as a locally running binary. `hurl` and `go` are part of the nix dev
shell.

## Tips & Tricks

* You can start from a blank slate by just removing the Docker volume for SQLite. At the next start, Litestream will download the production backup.
* `$ env -C backend ghcid --no-height-limit --clear --reverse`
* `$ env -C backend ghcid --no-height-limit --clear --reverse --target=test:tests`

To restore the DB from S3 to your local file system use `litestream restore -o $LIONS_SQLITE_PATH s3://$LITESTREAM_BUCKET/$LITESTREAM_RESTORE_PATH`

### Migrations

- create: `migrate -database "sqlite3://$LIONS_SQLITE_PATH" -path backend/migrations create -dir backend/migrations -ext sql -seq add_activities`
- version: `migrate -database "sqlite3://$LIONS_SQLITE_PATH" -path backend/migrations version`

## Deploy

> **Warning: this app must run on exactly one Fly machine.**
>
> The database is a SQLite file on a volume and Litestream replicates it to
> S3. Neither works with more than one machine: each machine gets its own
> volume, so writes end up split across unrelated databases, sessions created
> on one machine are unknown to the other, and two Litestream processes
> writing to the same S3 path corrupt the backup. This happened in 2026, see
> the "fly.io, SQLite and Litestream" note below.
>
> Fly creates a second machine by default on `fly deploy` and `fly scale`,
> and there is no `fly.toml` setting to prevent it. Always deploy like this:

```sh
flyctl scale count 1 -y -a lions
flyctl deploy --ha=false -a lions
```

Before and after deploying, check that there is a single machine and a
single volume:

```sh
flyctl status -a lions
flyctl volumes list -a lions
```

If a second machine shows up, stop it immediately with
`flyctl machine stop <id> -a lions`, then figure out which volume holds the
newer data before destroying anything.

### fly.io, SQLite and Litestream

* Litestream is a single node disaster recovery tool, not replication between
  live servers. Fly's own guidance is to run a single machine with
  `--ha=false` ([community answer by the Litestream
  author](https://community.fly.io/t/am-i-right-in-assuming-that-if-youre-using-litestream-not-litefs-that-you-should-only-run-one-machine/13842)).
* `--ha=false` only prevents new machines from being created. An existing
  extra machine stays, which is why the `scale count 1` step comes first.
* Litestream's [production tips](https://litestream.io/tips/) warn that
  multiple applications replicating into the same bucket and path can make
  the replica impossible to restore.
* Litestream's `restore -if-db-not-exists` in `entrypoint.sh` means a fresh
  machine silently starts with a full copy of the data, so a second machine
  looks healthy and is hard to notice.

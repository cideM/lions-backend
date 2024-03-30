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

## Tips & Tricks

* You can start from a blank slate by just removing the Docker volume for SQLite. At the next start, Litestream will download the production backup.
* `$ env -C backend ghcid --no-height-limit --clear --reverse`
* `$ env -C backend ghcid --no-height-limit --clear --reverse --target=test:tests`

To restore the DB from S3 to your local file system use `litestream restore -o $LIONS_SQLITE_PATH s3://$LITESTREAM_BUCKET/$LITESTREAM_RESTORE_PATH`

## Deploy

`flyctl deploy`

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

## Known Problems

- The modules are super granular, which is annoying. I don't like Haskell
  modules but overall it's easier to work with fewer, slightly bigger modules.
- Layouting is very ad-hoc
- The project would benefit from more tests
- CI broke randomly and now gives me `error: a 'x86_64-darwin' with features {} is required to build '/nix/store/zin2czycwviah6nghq6s0i4gnqxqm0am-source.drv', but I am a 'x86_64-linux' with features {benchmark, big-parallel, nixos-test, recursive-nix}` which makes no sense whatsoever. Why is CI trying to build a darwin thing, when it works locally?
- Not all modules were cleaned up when I added the `mtl` everywhere. So some of
  the code might still be a little rough around the edges.
- I think I'd like to go back to no `mtl` in the end. It's simpler, just a bit
  more typing, less foot guns.

## Known UX Problems

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

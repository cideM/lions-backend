# Hi!

## Deploy

`deploy .`

This might fail because of a weird error message that complains about the current env not being Linux. Whatever? Just comment out the darwin system in the flake and if you're on Darwin then please figure this out :), sincerely, paste me.

## SQLite

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

### Dummy Data

*There's now a command called `lions-dummy` that loads all*

```shell
for f in ./dev/*; sqlite3 $LIONS_SQLITE_PATH < $f; end
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

Restore the DB **from** S3 **to** a local file on the server. This process is pretty dangerous. First of all, files can only be accessed by the dynamic user created by systemd. You can override this with `sudo` but then you need to `chown` the files afterwards. Also, be sure to delete all DB files, not just the DB itself, also the WAL and the `.db-litestream` I guess?

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

### Tests

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

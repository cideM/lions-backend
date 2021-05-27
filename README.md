## Migrations

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

## SQLite Dummy Data

*There's now a command called `lions-dummy` that loads all*

```shell
for f in ./dev/*; sqlite3 $LIONS_SQLITE_PATH < $f; end
```

## Development

Rebuild and restart when source changes. This may or may not work reliably on MacOS.

```shell
$ fd . -e hs | entr -cr lions-dev
```

## Litestream

```shell
litestream generations s3://lions-achern-litestream-replica-1/test/
```

Restore the DB **from** S3 **to** a local file called `prod.db`:

```shell
litestream restore -o prod.db s3://lions-achern-litestream-replica-1/prod
```

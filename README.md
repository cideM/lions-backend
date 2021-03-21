## Migrations

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

## SQLite Dummy Data

```shell
$ cd dev
$ sqlite3 $LIONS_SQLITE_PATH
sqlite> .read add_user.sql
sqlite> select * from users ;
1|foo@bar.com|$2y$04$cZQmyFjUahmyZnojYpM4rOhwWI629ulZKI2Un92/ysvovfMnzN2/e||||||||
```

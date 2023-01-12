#!/bin/sh
/usr/local/bin/migrate -path /opt/app/migrations -database "sqlite3://$LIONS_SQLITE_PATH" up
/opt/app/run-lions-backend

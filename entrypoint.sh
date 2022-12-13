#!/bin/sh

litestream restore -if-db-not-exists -o $SQLITE_DB_PATH s3://$LITESTREAM_BUCKET/$LITESTREAM_RESTORE_PATH
exec litestream replicate -exec "/usr/local/bin/app" $SQLITE_DB_PATH s3://$LITESTREAM_BUCKET/$LITESTREAM_REPLICATE_PATH

#!/bin/sh

/usr/local/bin/litestream restore -if-db-not-exists -o $LIONS_SQLITE_PATH s3://$LITESTREAM_BUCKET/$LITESTREAM_RESTORE_PATH
exec /usr/local/bin/litestream replicate -exec "/opt/app/run-lions-backend" $LIONS_SQLITE_PATH s3://$LITESTREAM_BUCKET/$LITESTREAM_REPLICATE_PATH

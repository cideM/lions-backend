#! /usr/bin/env bash
# This script generates a single SQL statement to be used with SQLite. Use like this:
# $ cat ~/firebase_users.json | ./scripts/import_firebase.sh
# .json file should be exported Firebase auth data

echo "INSERT INTO users (email,password_digest,salt) VALUES"

inserts=$(jq -r '.users | .[] | [.email,.passwordHash,.salt] | @tsv' | awk -v q=\' '{ print "("q$1q"," q$2q"," q$3q")," }')

echo $inserts | sed 's/.$/;/'


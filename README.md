# Hi!

## Deploy

Don't deploy manually, instead rely on the 20-30min CI job. :(

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
```

### Creating Migrations

```
$ migrate create -ext sql -dir migrations/ -seq events
/home/tifa/lions-backend/migrations/000007_events.up.sql
/home/tifa/lions-backend/migrations/000007_events.down.sql
```

### Litestream

When accessing S3 through the CLI from the Administrator ARN, keep in mind that
the user doesn't have access to the S3 bucket. You can only access it through
the Litestream user. Use the shell below, together with values from either
`.envrc` or 1Password to check which replicas there are.

There's a `prod` and a `dev-db` prefix.

```shell
$ AWS_ACCESS_KEY_ID= AWS_SECRET_ACCESS_KEY= aws s3 ls lions-achern-litestream-replica-1/
```

#### List Generations

```shell
litestream generations s3://lions-achern-litestream-replica-1/prod/
```

#### Download A Generation

The shell below shows a pretty terrible way of getting a Litestream replica
onto the production server. It's better to run this as a one-off systemd job
with the same user as the server so permissions are setup correctly. I've never
had to do this though, so I'll leave this as an exercise to my future self.

```shell
[admin@lions-server:~]$ sudo rm /var/lib/lions-server/db /var/lib/lions-server/.db-litestream
[admin@lions-server:~]$ sudo LITESTREAM_ACCESS_KEY_ID= LITESTREAM_SECRET_ACCESS_KEY= litestream restore -o /var/lib/lions-server/db s3://lions-achern-litestream-replica-1/dev-db
[admin@lions-server:~]$ sudo chown lions-server:lions /var/lib/lions-server/db
```

Here's how to get prod DB onto local, if you already have a valid `.envrc` setup

`litestream restore -o $LIONS_SQLITE_PATH s3://lions-achern-litestream-replica-1/prod`

## Development

### Restart On Change

Rebuild and restart when source changes. This may or may not work reliably on MacOS.

```shell
$ fd . -e hs -e purs | entr -cr ./scripts/dev.hs
```

### GHCID

```shell
$ env -C backend ghcid --no-height-limit --clear --reverse
```

## Tests

### GHCID

```shell
$ env -C backend ghcid --no-height-limit --clear --reverse --target=test:tests
```

### Compile & Run

This will recompile and run the test binary, it's not using Cabal's test running facilities.

```shell
$ cd backend
$ fd -e hs | entr -c cabal v2-run test:tests
```

### Run With Cabal

This will run the tests through cabal but it seems to generate less output. But
that output is useful, so prefer the above snippet.

```shell
$ cd backend
$ fd -e hs | entr -c cabal v2-test
```

### End-To-End Tests

```shell
$ nix run .#e2e
```

There's one simple end-to-end test which only works on Linux. You can make this
work on MacOS as well by running QEMU in Docker. Probably won't work for M1
though.

There's a memory leak hardcoded into the E2E test in GitHub CI. Reason being
that killing the server process also kills the entire GitHub worker. I don't
know if this is a Haskell bug or something about how `run: ` works. I ran out
of energy to properly debug and fix this, sorry.

### QEMU

Just run `lions-vm`, it's setup to work on Linux and MacOS.

Hit `CTRL-A X` to quit.

Login is `root` and an empty password.

Use `$ ssh -o StrictHostKeyChecking=no root@localhost -p 2221 systemctl status` to run commands.
Disabling the check is necessary because whenever you delete and recreate the
VM the `known_hosts` needs to be updated.

## TODO

### Code

- The modules are super granular, which is annoying. I don't like Haskell
  modules but overall it's easier to work with fewer, slightly bigger modules.
- Layouting is very ad-hoc
- The project would benefit from more tests
- Not all modules were cleaned up when I added the `mtl` everywhere. So some of
  the code might still be a little rough around the edges.
- I think I'd like to go back to no `mtl` in the end. It's simpler, just a bit
  more typing, less foot guns.
- Routing would benefit from more type safety and more uniformity. Maybe define
  a result that route handlers can return which also allows errors. That way
  route handlers are easier to test and it's clearer to see what the possible
  returned responses are

### UX

- Profile pictures are missing
- I'd like to move away from OneDrive for sharing files. A very light weight
  file management system would be super cool.
- No push notifications for monitoring, only systemd logs on server. Kinda
  sucks. I'm used to being notified if something breaks.

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

### Modules

Haskell modules suck. It's hard to get all the following:

1. Import a single module rather than 10, all relating to `User`
2. No need to think about import cycles
3. No redundant names like Foo.Bar.Bar
4. Granular modules at the source level
5. No string prefixes for namspaces

You can either stuff everything in one module (gives you 1, 2 and 3), or make
everything extremely granular (2, 4, 5) but you won't get all of it.

I went for the very granular approach. I still had an import cycle which forced
me to very awkwardly split functions and types in one place, but other than
that it's OK. I could try to re-export some modules to reduce the number of
required imports, but that's not trivial, since you need to watch our for
conflicts when different submodules define the same names, such as `Id`.

self: super: {
  go-migrate = super.go-migrate.overrideAttrs (old: {
    buildFlags = "-tags 'sqlite3'";
  });
}

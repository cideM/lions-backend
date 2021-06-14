create table reset_tokens (
  id integer primary key,
  token text not null check(length(token) >= 24),
  expires text,
  userid integer,
  foreign key(userid) references users(id),
  unique (userid, token)
);

create index usertokens on reset_tokens (userid, token);

create index valuetokens on reset_tokens (token);

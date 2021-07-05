create table events (
  id integer primary key,
  title text not null check (title <> ''),
  date text not null check (title <> ''),
  family_allowed integer not null,
  description text,
  location text not null
);

create index userreplies on event_replies (userid, eventid);

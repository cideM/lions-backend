create table events (
  id integer primary key,
  title text not null check (title <> ''),
  date text not null check (title <> ''),
  family_allowed integer not null,
  description text,
  location text not null
);

create table event_replies (
  userid integer not null,
  eventid integer not null,
  coming integer not null,
  guests integer not null,
  foreign key (userid) references users(id) on delete cascade,
  foreign key (eventid) references events(id) on delete cascade,
  unique (userid, eventid)
);

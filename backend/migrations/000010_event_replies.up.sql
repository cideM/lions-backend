create table event_replies (
  userid integer not null,
  eventid integer not null,
  coming integer not null,
  guests integer not null,
  foreign key (userid) references users(id) on delete cascade,
  foreign key (eventid) references events(id) on delete cascade,
  unique (userid, eventid)
);

create index userreplies on event_replies (userid, eventid);

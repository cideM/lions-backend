create table event_attachments (
  id integer primary key,
  eventid integer,
  name text,
  foreign key(eventid) references events(id),
  unique(eventid, name)
)
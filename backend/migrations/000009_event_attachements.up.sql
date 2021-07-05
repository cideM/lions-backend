create table event_attachments (
  id integer primary key,
  eventid integer,
  filename text,
  foreign key(eventid) references events(id),
  unique(eventid, filename)
)

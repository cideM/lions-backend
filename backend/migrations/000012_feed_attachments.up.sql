create table feed_attachments (
  id integer primary key,
  postid integer,
  content blob,
  filename text,
  foreign key(postid) references welcome_text(id),
  unique(postid, filename)
)


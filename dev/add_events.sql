insert into events (
  title,
  date,
  description,
  family_allowed,
  location
) values
  (
    "Some event",
    "2015-25-01",
    "This is a random description of a
    random event",
    1,
    "some place else"
  ), (
    "Some other event",
    "2015-25-01",
    "Whoa super cool event stuff happening!",
    0,
    "totally not zoom"
  );

insert into event_replies (
  userid,
  eventid,
  coming,
  guests
) values (1, 1, 0, 0), (2, 1, 1, 5), (1, 2, 1, 3)


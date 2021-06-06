INSERT INTO users (email, first_name, last_name, address, mobile_phone_nr, landline_nr, birthday, first_name_partner, last_name_partner, birthday_partner, password_digest)
VALUES ("foo@bar.com", "foo", "bar", "some address, random

  stuff", "120348971203", "120348971203", date('1988-09-02'), "foopartner", "parner last name", date('1988-09-02'), "$2y$04$cZQmyFjUahmyZnojYpM4rOhwWI629ulZKI2Un92/ysvovfMnzN2/e");

INSERT INTO users (email, first_name, last_name, address, mobile_phone_nr, landline_nr, birthday, first_name_partner, last_name_partner, birthday_partner, password_digest)
VALUES ("test@test.com", "Test", "Person", "some address, random

  stuff", "120348971203", "120348971203", date('1988-09-02'), "foopartner", "partner last", date('1988-09-02'), "$2y$04$cZQmyFjUahmyZnojYpM4rOhwWI629ulZKI2Un92/ysvovfMnzN2/e");

/* Firebase PW and salt */
INSERT INTO users (email, first_name, last_name, address, mobile_phone_nr, landline_nr, birthday, first_name_partner, last_name_partner, birthday_partner, password_digest, salt)
VALUES ("board@test.com", "Big Boss", "Fooblurtb", "board member address", "120348971203", "120348971203", date('1988-09-02'), "foopartner", "parnter last name", date('1988-09-02'), "lSrfV15cpx95/sZS2W9c9Kp6i/LVgQNDNC/qzrCnh1SAyZvqmZqAjTdn3aoItz+VHjoZilo78198JAdRuid5lQ==", "42xEC+ixf3L2lw==");

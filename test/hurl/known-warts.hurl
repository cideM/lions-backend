# Behaviour of the current app that is accidental rather than designed:
# unhandled errors that surface as the generic 500 page. The rewrite does not
# need to preserve these. They are recorded so that a change is a conscious
# decision, and so that the generic error page itself is covered. Delete
# entries as the rewrite replaces them with proper 404s or validation.

POST {{base}}/login
[Form]
email: member@example.com
password: member-password
HTTP 302

# Non numeric ids are not validated
GET {{base}}/nutzer/abc
HTTP 500
[Asserts]
xpath "string(//title)" == "Fehler"
xpath "string(//*[@role='alert'])" == "Es ist leider ein Fehler aufgetreten"

GET {{base}}/veranstaltungen/abc
HTTP 500

GET {{base}}/activities/abc
HTTP 500

GET {{base}}/nutzer/abc/editieren
HTTP 500

# Unknown filter value
GET {{base}}/nutzer?userselect=bogus
HTTP 500

# Change form without a token
GET {{base}}/passwort/aendern
HTTP 500

# Deleting someone else's work time, or a missing one, fails with a 500
GET {{base}}/activities/1/times/2/delete
HTTP 500

POST {{base}}/activities/1/times/2/delete
HTTP 500

GET {{base}}/activities/1/times/999/delete
HTTP 500

# Unknown RSVP value
POST {{base}}/veranstaltungen/1/antwort
[Form]
reply: maybe
numberOfGuests: 0
HTTP 500

# Logging time against a missing activity
POST {{base}}/activities/999
[Form]
hours: 1
minutes: 0
HTTP 500

POST {{base}}/logout
HTTP 302

POST {{base}}/login
[Form]
email: admin@example.com
password: admin-password
HTTP 302

# Admin pages for missing records
GET {{base}}/veranstaltungen/999/editieren
HTTP 500

GET {{base}}/veranstaltungen/999/loeschen
HTTP 500

POST {{base}}/veranstaltungen/999/loeschen
HTTP 500

GET {{base}}/editieren/999
HTTP 500

GET {{base}}/loeschen/999
HTTP 500

GET {{base}}/nutzer/999/loeschen
HTTP 500

POST {{base}}/nutzer/999/loeschen
HTTP 500

GET {{base}}/activities/999/edit
HTTP 500

GET {{base}}/activities/999/delete
HTTP 500

# Duplicate email on create hits the unique index
POST {{base}}/nutzer/neu
[Form]
inputEmail: Member@example.com
HTTP 500

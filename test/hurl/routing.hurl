# Static files, unknown routes, wrong methods and the 404 page.

GET {{base}}/style.css
HTTP 200
[Asserts]
header "Content-Type" contains "text/css"

GET {{base}}/style.css
Accept-Encoding: gzip
HTTP 200
[Asserts]
header "Content-Encoding" == "gzip"

GET {{base}}/index.js
HTTP 200
[Asserts]
header "Content-Type" contains "javascript"

GET {{base}}/emblem_color.svg
HTTP 200
[Asserts]
header "Content-Type" contains "image/svg+xml"

# Unknown paths redirect anonymous visitors to the login page like any other
GET {{base}}/gibt/es/nicht
HTTP 302
[Asserts]
header "Location" == "/login"

POST {{base}}/login
[Form]
email: member@example.com
password: member-password
HTTP 302

# Logged in, unknown paths get a styled 404 with the normal navigation
GET {{base}}/gibt/es/nicht
HTTP 404
[Asserts]
header "Content-Type" == "text/html; charset=UTF-8"
xpath "string(//title)" == "Nicht gefunden"
xpath "string(//*[@role='alert'])" == "Nicht Gefunden"
xpath "count(//nav//a[normalize-space()='Startseite'])" == 1

# Wrong methods on known routes are 404s, except on the activities routes
POST {{base}}/
HTTP 404

PUT {{base}}/nutzer
HTTP 404

DELETE {{base}}/veranstaltungen
HTTP 404

PUT {{base}}/activities
HTTP 405

DELETE {{base}}/activities/1
HTTP 405

GET {{base}}/style.css
HTTP 200

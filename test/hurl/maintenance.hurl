# With LIONS_MAINTENANCE set (see maintenance.pre.sh) every request gets a
# 503 notice, but static files still load so the notice is styled.

GET {{base}}/
HTTP 503
[Asserts]
header "Retry-After" == "3600"
header "Cache-Control" == "no-store"
header "Content-Type" == "text/html; charset=UTF-8"
xpath "string(//title)" == "Wartungsarbeiten"
xpath "string(//h1)" == "Wartungsarbeiten"
xpath "string(//*[@role='alert'])" contains "vorübergehend nicht erreichbar"

GET {{base}}/login
HTTP 503

POST {{base}}/login
[Form]
email: admin@example.com
password: admin-password
HTTP 503
[Asserts]
header "Set-Cookie" not exists

GET {{base}}/passwort/link
HTTP 503

GET {{base}}/style.css
HTTP 200

GET {{base}}/emblem_color.svg
HTTP 200

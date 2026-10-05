# Login, logout, the session cookie and what anonymous visitors can reach.

# Anonymous visitors are sent to the login page everywhere except the login
# and password pages and static files.
GET {{base}}/
HTTP 302
[Asserts]
header "Location" == "/login"

GET {{base}}/veranstaltungen
HTTP 302
[Asserts]
header "Location" == "/login"

GET {{base}}/nutzer/1
HTTP 302
[Asserts]
header "Location" == "/login"

GET {{base}}/activities
HTTP 302
[Asserts]
header "Location" == "/login"

GET {{base}}/passwort/link
HTTP 200
[Asserts]
xpath "string(//title)" == "Passwort Zurücksetzen"

GET {{base}}/login
HTTP 200
[Asserts]
header "Content-Type" == "text/html; charset=UTF-8"
xpath "string(//title)" == "Login"
xpath "string(/html/@lang)" == "de-DE"
xpath "string(//form[.//button[normalize-space()='Einloggen']]/@action)" == "/login"
xpath "string(//form[.//button[normalize-space()='Einloggen']]/@method)" == "post"
xpath "string(//input[@id=//label[normalize-space()='Email Adresse']/@for]/@name)" == "email"
xpath "string(//input[@id=//label[normalize-space()='Passwort']/@for]/@name)" == "password"
xpath "string(//input[@name='password']/@type)" == "password"
xpath "string(//a[normalize-space()='Passwort vergessen']/@href)" == "/passwort/link"
# Anonymous navigation offers Login, but no profile and no logout
xpath "string(//nav//a[normalize-space()='Login']/@href)" == "/login"
xpath "count(//nav//a[normalize-space()='Mein Profil'])" == 0
xpath "count(//nav//button[normalize-space()='Ausloggen'])" == 0

# Wrong password: 401, the form comes back with the email kept
POST {{base}}/login
[Form]
email: admin@example.com
password: wrong
HTTP 401
[Asserts]
xpath "string(//title)" == "Login"
xpath "string(//input[@name='email']/@value)" == "admin@example.com"
xpath "count(//*[normalize-space()='Ungültige Kombination aus Email und Passwort'])" == 2
header "Set-Cookie" not exists

# Unknown email: same answer, no user enumeration
POST {{base}}/login
[Form]
email: nobody@example.com
password: admin-password
HTTP 401
[Asserts]
xpath "count(//*[normalize-space()='Ungültige Kombination aus Email und Passwort'])" == 2

POST {{base}}/login
[Form]
email:
password:
HTTP 401

# Successful login sets a hardened session cookie and redirects home
POST {{base}}/login
[Form]
email: admin@example.com
password: admin-password
HTTP 302
[Asserts]
header "Location" == "/"
cookie "lions_session" exists
cookie "lions_session[HttpOnly]" exists
cookie "lions_session[Secure]" exists
cookie "lions_session[SameSite]" == "Lax"
cookie "lions_session[Path]" == "/"
cookie "lions_session[Expires]" exists
[Captures]
admin_session: cookie "lions_session"

GET {{base}}/
HTTP 200
[Asserts]
xpath "string(//title)" == "Willkommen"
xpath "string(//nav//a[normalize-space()='Startseite']/@href)" == "/"
xpath "string(//nav//a[normalize-space()='Veranstaltungen']/@href)" == "/veranstaltungen"
xpath "string(//nav//a[normalize-space()='Mitglieder']/@href)" == "/nutzer"
xpath "string(//nav//a[normalize-space()='Activities']/@href)" == "/activities"
xpath "string(//nav//a[normalize-space()='Mein Profil']/@href)" == "/nutzer/1"
xpath "string(//nav//form[.//button[normalize-space()='Ausloggen']]/@action)" == "/logout"
xpath "string(//nav//form[.//button[normalize-space()='Ausloggen']]/@method)" == "post"
xpath "count(//nav//a[normalize-space()='Login'])" == 0

# The login page tells a logged in user so
GET {{base}}/login
HTTP 200
[Asserts]
xpath "string(//*[@role='alert'])" == "Du bist bereits eingelogged!"
xpath "count(//button[normalize-space()='Ausloggen'])" >= 1
xpath "count(//input[@name='password'])" == 0

GET {{base}}/passwort/link
HTTP 200

# Logout is POST only
GET {{base}}/logout
HTTP 404

POST {{base}}/logout
HTTP 302
[Asserts]
header "Location" == "/login"
header "Set-Cookie" contains "lions_session=;"

# The session is gone server side, not just in the browser: the old cookie
# value no longer works either
GET {{base}}/
HTTP 302
[Asserts]
header "Location" == "/login"

GET {{base}}/
[Cookies]
lions_session: {{admin_session}}
HTTP 302
[Asserts]
header "Location" == "/login"

GET {{base}}/
Cookie: lions_session=not-a-real-session
HTTP 302
[Asserts]
header "Location" == "/login"

# Users imported from Firebase log in with their scrypt hash
POST {{base}}/login
[Form]
email: firebase@example.com
password: user1password
HTTP 302
[Asserts]
header "Location" == "/"

GET {{base}}/
HTTP 200
[Asserts]
xpath "string(//nav//a[normalize-space()='Mein Profil']/@href)" == "/nutzer/3"

POST {{base}}/logout
HTTP 302

POST {{base}}/login
[Form]
email: firebase@example.com
password: wrongpassword
HTTP 401

# Email matching ignores case and surrounding whitespace
POST {{base}}/login
[Form]
email:  Member@EXAMPLE.com 
password: member-password
HTTP 302
[Asserts]
header "Location" == "/"

GET {{base}}/
HTTP 200
[Asserts]
xpath "string(//nav//a[normalize-space()='Mein Profil']/@href)" == "/nutzer/2"

POST {{base}}/logout
HTTP 302

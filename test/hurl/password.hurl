# Password reset by email link and the password change form. The tokens used
# in the change part are seeded by password.pre.sh.

GET {{base}}/passwort/link
HTTP 200
[Asserts]
xpath "string(//title)" == "Passwort Zurücksetzen"
xpath "string(//h1)" == "Passwort Zurücksetzen"
xpath "string(//form[.//button[normalize-space()='Absenden']]/@method)" == "post"
xpath "string(//input[@id=//label[normalize-space()='Email Adresse']/@for]/@name)" == "email"
[Captures]
reset_action: xpath "string(//form[.//button[normalize-space()='Absenden']]/@action)"

POST {{base}}{{reset_action}}
[Form]
email: unbekannt@example.com
HTTP 200
[Asserts]
xpath "count(//p[normalize-space()='Diese Email-Adresse ist nicht beim Lions Club Achern registiert'])" == 1
xpath "count(//button[normalize-space()='Absenden'])" == 1

POST {{base}}{{reset_action}}
[Form]
email:
HTTP 200
[Asserts]
xpath "count(//p[normalize-space()='Diese Email-Adresse ist nicht beim Lions Club Achern registiert'])" == 1

POST {{base}}{{reset_action}}
HTTP 200
[Asserts]
xpath "count(//p[normalize-space()='Email darf nicht leer sein'])" == 1

# Known address: a token is stored (checked in password.post.sh) and mailed
POST {{base}}{{reset_action}}
[Form]
email: member@example.com
HTTP 200
[Asserts]
xpath "string(//*[@role='alert'])" == "Email mit Link wurde verschickt!"

# The change form, reached through the emailed link
GET {{base}}/passwort/aendern?token={{valid_token}}
HTTP 200
[Asserts]
xpath "string(//title)" == "Passwort Ändern"
xpath "string(//form[.//button[normalize-space()='Speichern']]/@method)" == "post"
xpath "string(//input[@id=//label[normalize-space()='Passwort']/@for]/@name)" == "inputPassword"
xpath "string(//input[@id=//label[normalize-space()='Passwort']/@for]/@type)" == "password"
xpath "string(//input[@id=//label[normalize-space()='Passwort wiederholen']/@for]/@name)" == "inputPasswordMatch"
xpath "string(//input[@name='token']/@type)" == "hidden"
xpath "string(//input[@name='token']/@value)" == "{{valid_token}}"
[Captures]
change_action: xpath "string(//form[.//button[normalize-space()='Speichern']]/@action)"

POST {{base}}{{change_action}}
[Form]
inputPassword: neues-passwort
inputPasswordMatch: anderes-passwort
token: {{valid_token}}
HTTP 200
[Asserts]
xpath "count(//text()[normalize-space()='Passwörter stimmen nicht überein'])" == 2
xpath "string(//input[@name='token']/@value)" == "{{valid_token}}"

POST {{base}}{{change_action}}
[Form]
inputPassword:
inputPasswordMatch:
token: {{valid_token}}
HTTP 200
[Asserts]
xpath "count(//text()[normalize-space()='Feld darf nicht leer sein'])" == 2

POST {{base}}{{change_action}}
[Form]
inputPassword: neues-passwort
inputPasswordMatch: neues-passwort
HTTP 200
[Asserts]
xpath "string(//*[@role='alert'])" contains "Verifizierungs-Code"
xpath "string(//*[@role='alert'])" contains "fehlt"

POST {{base}}{{change_action}}
[Form]
inputPassword: neues-passwort
inputPasswordMatch: neues-passwort
token: gibt-es-nicht
HTTP 200
[Asserts]
xpath "string(//*[@role='alert'])" contains "wurde nicht gefunden"

POST {{base}}{{change_action}}
[Form]
inputPassword: neues-passwort
inputPasswordMatch: neues-passwort
token: {{expired_token}}
HTTP 200
[Asserts]
xpath "string(//*[@role='alert'])" contains "bereits abgelaufen"

# The old password still works until the change succeeds
POST {{base}}/login
[Form]
email: firebase@example.com
password: user1password
HTTP 302

POST {{base}}/logout
HTTP 302

POST {{base}}{{change_action}}
[Form]
inputPassword: neues-passwort
inputPasswordMatch: neues-passwort
token: {{valid_token}}
HTTP 200
[Asserts]
xpath "string(//*[@role='alert'])" == "Password erfolgreich geändert"

# The token is single use
POST {{base}}{{change_action}}
[Form]
inputPassword: neues-passwort
inputPasswordMatch: neues-passwort
token: {{valid_token}}
HTTP 200
[Asserts]
xpath "string(//*[@role='alert'])" contains "wurde nicht gefunden"

# Old password rejected, new one accepted (the Firebase user is now bcrypt)
POST {{base}}/login
[Form]
email: firebase@example.com
password: user1password
HTTP 401

POST {{base}}/login
[Form]
email: firebase@example.com
password: neues-passwort
HTTP 302
[Asserts]
header "Location" == "/"

GET {{base}}/
HTTP 200
[Asserts]
xpath "string(//nav//a[normalize-space()='Mein Profil']/@href)" == "/nutzer/3"

# Member list, profiles, editing, and the admin only create and delete flows.

POST {{base}}/login
[Form]
email: member@example.com
password: member-password
HTTP 302

# List: every member sees everyone; members have no create button
GET {{base}}/nutzer
HTTP 200
[Asserts]
xpath "string(//title)" == "Mitglieder"
xpath "string(//h1)" == "Mitgliederliste"
xpath "count(//table[thead//th[normalize-space()='Email']]/tbody/tr)" == 5
xpath "count(//a[normalize-space()='Neues Mitglied hinzufügen'])" == 0
xpath "string(//tr[td//a[normalize-space()='board@example.com']]//a[starts-with(@href, '/nutzer/')]/@href)" == "/nutzer/4"
xpath "string(//tr[td//a[normalize-space()='board@example.com']]//a[normalize-space()='board@example.com']/@href)" == "mailto:board@example.com"
xpath "string(//tr[td//a[normalize-space()='board@example.com']])" contains "Carol Board"
xpath "string(//tr[td//a[normalize-space()='board@example.com']])" contains "Vorstand"
xpath "count(//a[normalize-space()='Email abschicken'])" == 1
xpath "count(//select[@name='userselect']/option)" == 5
xpath "string(//select[@name='userselect']/option[1])" == "Alle anzeigen"
xpath "string(//select[@name='userselect']/option[1]/@value)" == "all"
xpath "string(//form[.//select[@name='userselect']]/@method)" == "get"

# Filtering by role
GET {{base}}/nutzer?userselect=board
HTTP 200
[Asserts]
xpath "count(//table[thead//th[normalize-space()='Email']]/tbody/tr)" == 1
xpath "count(//tr[td//a[normalize-space()='board@example.com']])" == 1
xpath "string(//select[@name='userselect']/option[1])" == "Nur Vorstand"
xpath "string(//select[@name='userselect']/option[1]/@value)" == "board"

GET {{base}}/nutzer?userselect=admin
HTTP 200
[Asserts]
xpath "count(//table[thead//th[normalize-space()='Email']]/tbody/tr)" == 1
xpath "count(//tr[td//a[normalize-space()='admin@example.com']])" == 1

GET {{base}}/nutzer?userselect=passive
HTTP 200
[Asserts]
xpath "count(//table[thead//th[normalize-space()='Email']]/tbody/tr)" == 1
xpath "count(//tr[td//a[normalize-space()='delete-me@example.com']])" == 1

GET {{base}}/nutzer?userselect=president
HTTP 200
[Asserts]
xpath "count(//table[thead//th[normalize-space()='Email']]/tbody/tr)" == 0

GET {{base}}/nutzer?userselect=all
HTTP 200
[Asserts]
xpath "count(//table[thead//th[normalize-space()='Email']]/tbody/tr)" == 5

# Someone else's profile: visible, but neither editable nor deletable
GET {{base}}/nutzer/4
HTTP 200
[Asserts]
xpath "string(//title)" == "Nutzerprofil"
xpath "string(//h1)" == "Carol Board"
body contains "Vorstand"
xpath "string(//a[normalize-space()='board@example.com']/@href)" == "mailto:board@example.com"
body contains "Ringstraße 4, 77855 Achern"
body contains "0171-4444444"
body contains "04.04.1975"
xpath "count(//a[normalize-space()='Nutzer editieren'])" == 0
xpath "count(//a[normalize-space()='Nutzer löschen'])" == 0

# Own profile: editable, not deletable
GET {{base}}/nutzer/2
HTTP 200
[Asserts]
xpath "string(//h1)" == "Bob Member"
xpath "count(//a[normalize-space()='Nutzer editieren'])" == 1
xpath "count(//a[normalize-space()='Nutzer löschen'])" == 0
[Captures]
own_edit_url: xpath "string(//a[normalize-space()='Nutzer editieren']/@href)"

GET {{base}}/nutzer/999
HTTP 404

# Admin only routes are forbidden for members
GET {{base}}/nutzer/4/editieren
HTTP 403
[Asserts]
xpath "string(//title)" == "Fehler"
xpath "string(//*[@role='alert'])" == "Du hast keinen Zugriff auf diese Seite"

POST {{base}}/nutzer/4/editieren
[Form]
inputEmail: board@example.com
HTTP 403

GET {{base}}/nutzer/neu
HTTP 403

POST {{base}}/nutzer/neu
[Form]
inputEmail: sneaky@example.com
HTTP 403

GET {{base}}/nutzer/5/loeschen
HTTP 403

POST {{base}}/nutzer/5/loeschen
HTTP 403

# Members edit their own profile, but the role checkboxes are disabled
GET {{base}}{{own_edit_url}}
HTTP 200
[Asserts]
xpath "string(//title)" == "Nutzer Editieren"
xpath "string(//input[@id=//label[normalize-space()='Email']/@for]/@value)" == "member@example.com"
xpath "string(//input[@id=//label[normalize-space()='Email']/@for]/@name)" == "inputEmail"
xpath "string(//input[@id=//label[normalize-space()='Vorname']/@for]/@value)" == "Bob"
xpath "string(//input[@id=//label[normalize-space()='Nachname']/@for]/@value)" == "Member"
xpath "string(//input[@id=//label[normalize-space()='Geburtstag']/@for]/@value)" == "03.03.1980"
xpath "string(//input[@id=//label[normalize-space()='Handynummer']/@for]/@value)" == "0171-2222222"
xpath "string(//textarea[@id=//label[normalize-space()='Adresse']/@for])" == "Nebenstraße 2, 77855 Achern"
xpath "count(//input[@id=//label[normalize-space()='Administrator']/@for][@disabled])" == 1
xpath "count(//input[@id=//label[normalize-space()='Vorstand']/@for][@disabled])" == 1
xpath "count(//input[@id=//label[normalize-space()='Präsident']/@for][@disabled])" == 1
xpath "count(//input[@id=//label[normalize-space()='Passiv']/@for][@disabled])" == 1
[Captures]
own_edit_action: xpath "string(//form[.//button[normalize-space()='Nutzer editieren']]/@action)"

# Sending role fields anyway changes nothing about the roles
POST {{base}}{{own_edit_action}}
[Form]
inputEmail: member@example.com
inputFirstName: Bobby
inputLastName: Member
inputAddress: Neue Straße 3
inputMobile: 0171-2222222
inputLandline:
inputBirthday: 03.03.1980
inputFirstNamePartner:
inputLastNamePartner:
inputBirthdayPartner:
inputIsAdmin: on
inputIsBoard: on
HTTP 200
[Asserts]
xpath "string(//*[@role='alert'])" == "Nutzer member@example.com erfolgreich editiert"

GET {{base}}/nutzer/2
HTTP 200
[Asserts]
xpath "string(//h1)" == "Bobby Member"
body contains "Neue Straße 3"
body not contains "Administrator"
body not contains "Vorstand"

# Validation on edit
POST {{base}}{{own_edit_action}}
[Form]
inputEmail: not-an-email
inputFirstName: Bobby
HTTP 200
[Asserts]
xpath "count(//text()[normalize-space()='Falsches Format'])" == 1
xpath "string(//input[@name='inputEmail']/@value)" == "not-an-email"

POST {{base}}{{own_edit_action}}
[Form]
inputEmail:
HTTP 200
[Asserts]
xpath "count(//text()[normalize-space()='Email darf nicht leer sein'])" == 1

POST {{base}}/logout
HTTP 302

# Admin flows
POST {{base}}/login
[Form]
email: admin@example.com
password: admin-password
HTTP 302

GET {{base}}/nutzer
HTTP 200
[Asserts]
xpath "count(//a[normalize-space()='Neues Mitglied hinzufügen'])" == 1
[Captures]
create_url: xpath "string(//a[normalize-space()='Neues Mitglied hinzufügen']/@href)"

GET {{base}}/nutzer/4
HTTP 200
[Asserts]
xpath "count(//a[normalize-space()='Nutzer editieren'])" == 1
xpath "count(//a[normalize-space()='Nutzer löschen'])" == 1
[Captures]
edit_4_url: xpath "string(//a[normalize-space()='Nutzer editieren']/@href)"

GET {{base}}{{create_url}}
HTTP 200
[Asserts]
xpath "string(//title)" == "Nutzer Hinzufügen"
xpath "count(//input[@id=//label[normalize-space()='Administrator']/@for][not(@disabled)])" == 1
xpath "count(//input[@id=//label[normalize-space()='Vorstand']/@for][not(@disabled)])" == 1
xpath "count(//input[@id=//label[normalize-space()='Präsident']/@for][not(@disabled)])" == 1
xpath "count(//input[@id=//label[normalize-space()='Passiv']/@for][not(@disabled)])" == 1
xpath "string(//input[@id=//label[normalize-space()='Vorstand']/@for]/@name)" == "inputIsBoard"
[Captures]
create_action: xpath "string(//form[.//button[normalize-space()='Nutzer erstellen']]/@action)"

POST {{base}}{{create_action}}
[Form]
inputEmail: nope
HTTP 200
[Asserts]
xpath "count(//text()[normalize-space()='Falsches Format'])" == 1

POST {{base}}{{create_action}}
[Form]
inputEmail: new@example.com
inputFirstName: Nina
inputLastName: Neu
inputAddress:
inputMobile:
inputLandline:
inputBirthday:
inputFirstNamePartner:
inputLastNamePartner:
inputBirthdayPartner:
inputIsBoard: on
inputIsPresident: on
HTTP 200
[Asserts]
xpath "string(//*[@role='alert'])" == "Nutzer new@example.com erfolgreich erstellt"

GET {{base}}/nutzer
HTTP 200
[Asserts]
xpath "count(//table[thead//th[normalize-space()='Email']]/tbody/tr)" == 6
xpath "count(//tr[td//a[normalize-space()='new@example.com']])" == 1
[Captures]
new_profile_url: xpath "string(//tr[td//a[normalize-space()='new@example.com']]//a[starts-with(@href, '/nutzer/')]/@href)"

GET {{base}}/nutzer?userselect=president
HTTP 200
[Asserts]
xpath "count(//tr[td//a[normalize-space()='new@example.com']])" == 1

GET {{base}}{{new_profile_url}}
HTTP 200
[Asserts]
xpath "string(//h1)" == "Nina Neu"
body contains "Vorstand"
body contains "Präsident"

# Admins change roles of others
GET {{base}}{{edit_4_url}}
HTTP 200
[Asserts]
xpath "count(//input[@id=//label[normalize-space()='Vorstand']/@for][@checked])" == 1
xpath "count(//input[@id=//label[normalize-space()='Administrator']/@for][@checked])" == 0
xpath "count(//input[@id=//label[normalize-space()='Administrator']/@for][not(@disabled)])" == 1
[Captures]
edit_4_action: xpath "string(//form[.//button[normalize-space()='Nutzer editieren']]/@action)"

POST {{base}}{{edit_4_action}}
[Form]
inputEmail: board@example.com
inputFirstName: Carol
inputLastName: Board
inputAddress: Ringstraße 4, 77855 Achern
inputMobile: 0171-4444444
inputLandline:
inputBirthday: 04.04.1975
inputFirstNamePartner:
inputLastNamePartner:
inputBirthdayPartner:
inputIsAdmin: on
HTTP 200
[Asserts]
xpath "string(//*[@role='alert'])" == "Nutzer board@example.com erfolgreich editiert"

GET {{base}}/nutzer/4
HTTP 200
[Asserts]
body contains "Administrator"
body not contains "Vorstand"

# Editing a missing user renders a notice rather than a 404
GET {{base}}/nutzer/999/editieren
HTTP 200
[Asserts]
xpath "string(//*[@role='alert'])" == "Kein Nutzer mit dieser ID gefunden"

# Delete, with a confirmation page
GET {{base}}/nutzer/5
HTTP 200
[Captures]
delete_url: xpath "string(//a[normalize-space()='Nutzer löschen']/@href)"

GET {{base}}{{delete_url}}
HTTP 200
[Asserts]
xpath "string(//*[@role='alert'])" == "Nutzer delete-me@example.com wirklich löschen?"
xpath "string(//form[.//button[normalize-space()='Ja, Nutzer löschen!']]/@method)" == "post"
[Captures]
delete_action: xpath "string(//form[.//button[normalize-space()='Ja, Nutzer löschen!']]/@action)"

POST {{base}}{{delete_action}}
HTTP 200
[Asserts]
xpath "string(//*[@role='alert'])" == "Nutzer delete-me@example.com erfolgreich gelöscht"

GET {{base}}/nutzer/5
HTTP 404

GET {{base}}/nutzer
HTTP 200
[Asserts]
xpath "count(//table[thead//th[normalize-space()='Email']]/tbody/tr)" == 5
xpath "count(//tr[td//a[normalize-space()='delete-me@example.com']])" == 0

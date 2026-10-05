# Events: list, detail, RSVP, attachments, and the admin only CRUD flows.

GET {{base}}/events/1/flyer.txt
HTTP 302
[Asserts]
header "Location" == "/login"

POST {{base}}/login
[Form]
email: member@example.com
password: member-password
HTTP 302

# List: upcoming events first, then past ones; badges summarise the replies
GET {{base}}/veranstaltungen
HTTP 200
[Asserts]
xpath "string(//title)" == "Veranstaltung"
xpath "string(//h1[1])" == "Veranstaltungen"
xpath "string(//a[normalize-space()='Sommerfest']/@href)" == "/veranstaltungen/1"
xpath "string(//a[normalize-space()='Vorstandssitzung']/@href)" == "/veranstaltungen/2"
xpath "contains(substring-after(string(//body), 'Sommerfest'), 'Vorstandssitzung')" == true
body contains "15. Juli 2027 18:00"
body contains "Ort: Vereinsheim Achern"
xpath "count(//text()[normalize-space()='Mit Familie'])" == 1
xpath "count(//text()[normalize-space()='Zugesagt'])" == 1
xpath "count(//text()[normalize-space()='Teilnehmer: 3'])" == 1
xpath "count(//text()[normalize-space()='Absagen: 1'])" == 1
xpath "count(//text()[normalize-space()='Bereits stattgefunden'])" == 1
xpath "count(//a[normalize-space()='Neue Veranstaltung'])" == 0
[Captures]
sommerfest_url: xpath "string(//a[normalize-space()='Sommerfest']/@href)"
sitzung_url: xpath "string(//a[normalize-space()='Vorstandssitzung']/@href)"

# Detail page with the own reply preselected
GET {{base}}{{sommerfest_url}}
HTTP 200
[Asserts]
xpath "string(//title)" == "Sommerfest"
xpath "string(//h1)" == "Sommerfest"
xpath "string(//h2)" == "Ort: Vereinsheim Achern"
body contains "15. Juli 2027 18:00"
body contains "Ein Fest für alle Mitglieder und Familien"
xpath "count(//text()[normalize-space()='Mit Familie'])" == 1
xpath "string(//a[normalize-space()='flyer.txt']/@href)" == "/events/1/flyer.txt"
xpath "string(//form[.//button[normalize-space()='Speichern']]/@method)" == "post"
xpath "string(//select[@id=//label[normalize-space()='Antwort']/@for]/@name)" == "reply"
xpath "string(//select[@name='reply']/option[@selected])" == "Zusage"
xpath "string(//select[@name='reply']/option[@selected]/@value)" == "coming"
xpath "count(//select[@name='reply']/option)" == 3
xpath "count(//select[@name='reply'][@disabled])" == 0
xpath "string(//input[@id=//label[normalize-space()='Anzahl Gäste die du mitbringst']/@for]/@name)" == "numberOfGuests"
xpath "string(//input[@name='numberOfGuests']/@value)" == "2"
body contains "Teilnehmer (3)"
body contains "Absagen (1)"
xpath "count(//a[normalize-space()='Löschen'])" == 0
xpath "count(//a[normalize-space()='Editieren'])" == 0
[Captures]
rsvp_action: xpath "string(//form[.//button[normalize-space()='Speichern']]/@action)"
yes_tab: xpath "string(//a[contains(normalize-space(), 'Teilnehmer (')]/@href)"
no_tab: xpath "string(//a[contains(normalize-space(), 'Absagen (')]/@href)"

GET {{base}}{{sommerfest_url}}{{yes_tab}}
HTTP 200
[Asserts]
xpath "count(//table[thead//th[normalize-space()='Gäste']]/tbody/tr)" == 1
xpath "count(//tr[td[normalize-space()='member@example.com'] and td[normalize-space()='2']])" == 1
xpath "string(//tr[td[normalize-space()='member@example.com']]//a[normalize-space()='Zum Profil']/@href)" == "/nutzer/2"

GET {{base}}{{sommerfest_url}}{{no_tab}}
HTTP 200
[Asserts]
xpath "count(//table[thead//th[normalize-space()='Email']]/tbody/tr)" == 1
xpath "string(//tr[td[normalize-space()='board@example.com']]//a[normalize-space()='Zum Profil']/@href)" == "/nutzer/4"

# Past events can no longer be answered
GET {{base}}{{sitzung_url}}
HTTP 200
[Asserts]
xpath "string(//h1)" == "Vorstandssitzung"
xpath "count(//text()[normalize-space()='Bereits stattgefunden'])" == 1
xpath "count(//text()[normalize-space()='Mit Familie'])" == 0
xpath "count(//select[@name='reply'][@disabled])" == 1
xpath "count(//input[@name='numberOfGuests'][@disabled])" == 1
xpath "string(//select[@name='reply']/option[@selected])" == "Keine Antwort"

GET {{base}}/veranstaltungen/999
HTTP 404

GET {{base}}/events/1/flyer.txt
HTTP 200
[Asserts]
header "Content-Type" contains "text/plain"
body == "Flyer Inhalt"

GET {{base}}/events/1/fehlt.txt
HTTP 404

# RSVP: decline
POST {{base}}{{rsvp_action}}
[Form]
reply: notcoming
numberOfGuests:
HTTP 303
[Asserts]
header "Location" == "/veranstaltungen/1"

GET {{base}}{{sommerfest_url}}
HTTP 200
[Asserts]
xpath "string(//select[@name='reply']/option[@selected])" == "Absage"
xpath "string(//input[@name='numberOfGuests']/@value)" == "0"
body contains "Teilnehmer (0)"
body contains "Absagen (2)"

GET {{base}}/veranstaltungen
HTTP 200
[Asserts]
xpath "count(//text()[normalize-space()='Abgesagt'])" == 1
xpath "count(//text()[normalize-space()='Zugesagt'])" == 0
xpath "count(//text()[starts-with(normalize-space(), 'Teilnehmer:')])" == 0
xpath "count(//text()[normalize-space()='Absagen: 2'])" == 1

# RSVP: accept with guests
POST {{base}}{{rsvp_action}}
[Form]
reply: coming
numberOfGuests: 3
HTTP 303

GET {{base}}{{sommerfest_url}}
HTTP 200
[Asserts]
xpath "string(//select[@name='reply']/option[@selected])" == "Zusage"
xpath "string(//input[@name='numberOfGuests']/@value)" == "3"
body contains "Teilnehmer (4)"
body contains "Absagen (1)"

GET {{base}}/veranstaltungen
HTTP 200
[Asserts]
xpath "count(//text()[normalize-space()='Zugesagt'])" == 1
xpath "count(//text()[normalize-space()='Teilnehmer: 4'])" == 1

# RSVP: withdraw the reply entirely
POST {{base}}{{rsvp_action}}
[Form]
reply: noreply
numberOfGuests: 3
HTTP 303

GET {{base}}{{sommerfest_url}}
HTTP 200
[Asserts]
xpath "string(//select[@name='reply']/option[@selected])" == "Keine Antwort"
xpath "string(//input[@name='numberOfGuests']/@value)" == "0"
body contains "Teilnehmer (0)"

GET {{base}}/veranstaltungen
HTTP 200
[Asserts]
xpath "count(//text()[normalize-space()='Zugesagt'])" == 0
xpath "count(//text()[normalize-space()='Abgesagt'])" == 0

# Admin only
GET {{base}}/veranstaltungen/neu
HTTP 403

POST {{base}}/veranstaltungen/neu
[Multipart]
eventTitleInput: sneaky
HTTP 403

GET {{base}}/veranstaltungen/1/editieren
HTTP 403

POST {{base}}/veranstaltungen/1/editieren
[Multipart]
eventTitleInput: sneaky
HTTP 403

GET {{base}}/veranstaltungen/1/loeschen
HTTP 403

POST {{base}}/veranstaltungen/1/loeschen
HTTP 403

POST {{base}}/logout
HTTP 302

POST {{base}}/login
[Form]
email: admin@example.com
password: admin-password
HTTP 302

GET {{base}}/veranstaltungen
HTTP 200
[Asserts]
xpath "count(//a[normalize-space()='Neue Veranstaltung'])" == 1
[Captures]
create_url: xpath "string(//a[normalize-space()='Neue Veranstaltung']/@href)"

GET {{base}}{{sommerfest_url}}
HTTP 200
[Asserts]
xpath "count(//a[normalize-space()='Löschen'])" == 1
xpath "count(//a[normalize-space()='Editieren'])" == 1

GET {{base}}{{create_url}}
HTTP 200
[Asserts]
xpath "string(//title)" == "Neue Veranstaltung"
xpath "string(//form[.//button[normalize-space()='Speichern']]/@method)" == "post"
xpath "string(//form[.//button[normalize-space()='Speichern']]/@enctype)" == "multipart/form-data"
xpath "string(//input[@id=//label[normalize-space()='Name']/@for]/@name)" == "eventTitleInput"
xpath "string(//input[@id=//label[normalize-space()='Ort']/@for]/@name)" == "eventLocationInput"
xpath "string(//input[@id=//label[normalize-space()='Datum']/@for]/@name)" == "eventDateInput"
xpath "string(//textarea[@id=//label[normalize-space()='Beschreibung']/@for]/@name)" == "eventDescriptionInput"
xpath "string(//input[@id=//label[normalize-space()='Dateien']/@for]/@name)" == "eventAttachmentsInput"
xpath "string(//input[@id=//label[normalize-space()='Dateien']/@for]/@type)" == "file"
xpath "string(//input[@id=//label[normalize-space()='Mit Familie?']/@for]/@name)" == "eventFamAllowedInput"
xpath "string(//input[@id=//label[normalize-space()='Mit Familie?']/@for]/@type)" == "checkbox"
xpath "count(//text()[normalize-space()=\"Bitte als Format '12.01.2022 15:00' verwenden.\"])" == 1
[Captures]
create_action: xpath "string(//form[.//button[normalize-space()='Speichern']]/@action)"

# Validation
POST {{base}}{{create_action}}
[Multipart]
eventTitleInput:
eventDateInput: 2028-01-10
eventLocationInput:
eventDescriptionInput: x
HTTP 200
[Asserts]
xpath "count(//text()[normalize-space()='Feld darf nicht leer sein'])" == 2
xpath "count(//text()[normalize-space()='Falsches Format'])" == 1
xpath "string(//textarea[@name='eventDescriptionInput'])" == "x"

# Create with attachment and family flag
POST {{base}}{{create_action}}
[Multipart]
eventTitleInput: Neujahrsempfang
eventDateInput: 10.01.2028 19:00
eventLocationInput: Rathaus
eventDescriptionInput: Mit Sekt. Infos: https://example.org/empfang
eventFamAllowedInput: on
eventAttachmentsInput: file,anhang.txt;
HTTP 200
[Asserts]
xpath "string(//*[@role='alert'])" == "Neue Veranstaltung Neujahrsempfang erfolgreich erstellt!"

GET {{base}}/veranstaltungen
HTTP 200
[Asserts]
xpath "count(//a[normalize-space()='Neujahrsempfang'])" == 1
xpath "contains(substring-after(string(//body), 'Sommerfest'), 'Neujahrsempfang')" == true
xpath "contains(substring-after(string(//body), 'Neujahrsempfang'), 'Vorstandssitzung')" == true
xpath "count(//text()[normalize-space()='Mit Familie'])" == 2
[Captures]
empfang_url: xpath "string(//a[normalize-space()='Neujahrsempfang']/@href)"

GET {{base}}{{empfang_url}}
HTTP 200
[Asserts]
xpath "string(//h1)" == "Neujahrsempfang"
xpath "string(//h2)" == "Ort: Rathaus"
body contains "10. Januar 2028 19:00"
xpath "count(//text()[normalize-space()='Mit Familie'])" == 1
xpath "string(//a[normalize-space()='https://example.org/empfang']/@href)" == "https://example.org/empfang"
xpath "count(//a[normalize-space()='anhang.txt'])" == 1
[Captures]
empfang_attachment: xpath "string(//a[normalize-space()='anhang.txt']/@href)"
empfang_edit_url: xpath "string(//a[normalize-space()='Editieren']/@href)"

GET {{base}}{{empfang_attachment}}
HTTP 200
[Asserts]
body == "Anhang Inhalt\n"

# Edit form shows the stored values
GET {{base}}{{empfang_edit_url}}
HTTP 200
[Asserts]
xpath "string(//title)" == "Veranstaltung Editieren"
xpath "string(//input[@id=//label[normalize-space()='Name']/@for]/@value)" == "Neujahrsempfang"
xpath "string(//input[@id=//label[normalize-space()='Datum']/@for]/@value)" == "10.01.2028 19:00"
xpath "string(//input[@id=//label[normalize-space()='Ort']/@for]/@value)" == "Rathaus"
xpath "string(//textarea[@id=//label[normalize-space()='Beschreibung']/@for])" == "Mit Sekt. Infos: https://example.org/empfang"
xpath "count(//input[@id=//label[normalize-space()='Mit Familie?']/@for][@checked])" == 1
xpath "count(//input[@id=//label[normalize-space()='anhang.txt']/@for][@type='checkbox'][@checked])" == 1
xpath "string(//input[@id=//label[normalize-space()='anhang.txt']/@for]/@name)" == "newFileCheckbox"
[Captures]
empfang_edit_action: xpath "string(//form[.//button[normalize-space()='Speichern']]/@action)"

# Edit: drop the attachment, add another, clear the family flag
POST {{base}}{{empfang_edit_action}}
[Multipart]
eventTitleInput: Neujahrsempfang 2028
eventDateInput: 11.01.2028 18:30
eventLocationInput: Altes Rathaus
eventDescriptionInput: Neue Beschreibung
eventAttachmentsInput: file,zweiter.txt;
HTTP 200
[Asserts]
xpath "string(//*[@role='alert'])" == "Veranstaltung Neujahrsempfang 2028 erfolgreich editiert"

GET {{base}}{{empfang_url}}
HTTP 200
[Asserts]
xpath "string(//h1)" == "Neujahrsempfang 2028"
xpath "string(//h2)" == "Ort: Altes Rathaus"
body contains "11. Januar 2028 18:30"
body contains "Neue Beschreibung"
xpath "count(//text()[normalize-space()='Mit Familie'])" == 0
xpath "count(//a[normalize-space()='anhang.txt'])" == 0
xpath "count(//a[normalize-space()='zweiter.txt'])" == 1

GET {{base}}{{empfang_attachment}}
HTTP 404

POST {{base}}{{empfang_edit_action}}
[Multipart]
eventTitleInput:
eventDateInput: 11.01.2028 18:30
eventLocationInput: Altes Rathaus
eventDescriptionInput: Neue Beschreibung
HTTP 200
[Asserts]
xpath "count(//text()[normalize-space()='Feld darf nicht leer sein'])" == 1

# Delete, with confirmation; replies and attachments go with the event
GET {{base}}{{sommerfest_url}}
HTTP 200
[Captures]
delete_url: xpath "string(//a[normalize-space()='Löschen']/@href)"

GET {{base}}{{delete_url}}
HTTP 200
[Asserts]
xpath "string(//title)" == "Veranstaltung Löschen"
xpath "string(//*[@role='alert'])" == "Veranstaltung Sommerfest wirklich löschen?"
xpath "string(//form[.//button[normalize-space()='Ja, Veranstaltung löschen!']]/@method)" == "post"
[Captures]
delete_action: xpath "string(//form[.//button[normalize-space()='Ja, Veranstaltung löschen!']]/@action)"

POST {{base}}{{delete_action}}
HTTP 200
[Asserts]
xpath "string(//*[@role='alert'])" == "Veranstaltung Sommerfest erfolgreich gelöscht"

GET {{base}}{{sommerfest_url}}
HTTP 404

GET {{base}}/events/1/flyer.txt
HTTP 404

GET {{base}}/veranstaltungen
HTTP 200
[Asserts]
xpath "count(//a[normalize-space()='Sommerfest'])" == 0
xpath "count(//a[normalize-space()='Neujahrsempfang 2028'])" == 1
xpath "count(//a[normalize-space()='Vorstandssitzung'])" == 1

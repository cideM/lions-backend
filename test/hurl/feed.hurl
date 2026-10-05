# The news feed on the start page: markdown posts with attachments.

GET {{base}}/news/2/protokoll.txt
HTTP 302
[Asserts]
header "Location" == "/login"

POST {{base}}/login
[Form]
email: member@example.com
password: member-password
HTTP 302

GET {{base}}/
HTTP 200
[Asserts]
xpath "string(//title)" == "Willkommen"
xpath "string(//h1)" == "Interne Neuigkeiten"
xpath "count(//text()[normalize-space()='Donnerstag, 20. Februar 2025'])" == 1
xpath "count(//text()[normalize-space()='Freitag, 10. Januar 2025'])" == 1
# Newest post first
xpath "contains(substring-after(string(//body), 'Donnerstag, 20. Februar 2025'), 'Freitag, 10. Januar 2025')" == true
# Markdown is rendered, URLs become links
xpath "count(//strong[normalize-space()='Mitgliederbereich'])" == 1
xpath "string(//a[normalize-space()='https://lions-achern.de']/@href)" == "https://lions-achern.de"
body contains "Protokoll der letzten Sitzung im Anhang."
# Attachments are listed and linked
xpath "string(//a[normalize-space()='protokoll.txt']/@href)" == "/news/2/protokoll.txt"
# Members see neither create nor edit links
xpath "count(//a[normalize-space()='Neue Nachricht'])" == 0
xpath "count(//a[normalize-space()='Ändern'])" == 0
xpath "count(//a[normalize-space()='Löschen'])" == 0

GET {{base}}/news/2/protokoll.txt
HTTP 200
[Asserts]
header "Content-Type" contains "text/plain"
body == "Protokoll Inhalt"

GET {{base}}/news/2/fehlt.txt
HTTP 404
[Asserts]
xpath "string(//title)" == "Nicht gefunden"

GET {{base}}/news/999/protokoll.txt
HTTP 404

GET {{base}}/neu
HTTP 403

POST {{base}}/neu
[Multipart]
date: 01.03.2025 09:15
message: sneaky
HTTP 403

GET {{base}}/editieren/1
HTTP 403

POST {{base}}/editieren/1
[Multipart]
date: 01.03.2025 09:15
message: sneaky
HTTP 403

GET {{base}}/loeschen/1
HTTP 403

POST {{base}}/loeschen/1
HTTP 403

POST {{base}}/logout
HTTP 302

POST {{base}}/login
[Form]
email: admin@example.com
password: admin-password
HTTP 302

GET {{base}}/
HTTP 200
[Asserts]
xpath "count(//a[normalize-space()='Neue Nachricht'])" == 1
xpath "count(//a[normalize-space()='Ändern'])" == 2
xpath "count(//a[normalize-space()='Löschen'])" == 2
[Captures]
create_url: xpath "string(//a[normalize-space()='Neue Nachricht']/@href)"

# Create form is prefilled with the current time
GET {{base}}{{create_url}}
HTTP 200
[Asserts]
xpath "string(//title)" == "Nachricht Erstellen"
xpath "string(//form[.//button[normalize-space()='Speichern']]/@method)" == "post"
xpath "string(//form[.//button[normalize-space()='Speichern']]/@enctype)" == "multipart/form-data"
xpath "string(//input[@id=//label[normalize-space()='Datum']/@for]/@value)" matches /^\d{2}\.\d{2}\.\d{4} \d{2}:\d{2}$/
xpath "string(//input[@id=//label[normalize-space()='Datum']/@for]/@name)" == "date"
xpath "string(//textarea[@id=//label[normalize-space()='Nachricht']/@for]/@name)" == "message"
xpath "string(//input[@id=//label[normalize-space()='Dateien']/@for]/@type)" == "file"
xpath "string(//input[@id=//label[normalize-space()='Dateien']/@for]/@name)" == "attachments"
[Captures]
create_action: xpath "string(//form[.//button[normalize-space()='Speichern']]/@action)"

# Validation keeps what was typed
POST {{base}}{{create_action}}
[Multipart]
date: 2025-03-01
message: Text
HTTP 200
[Asserts]
xpath "count(//text()[normalize-space()='Falsches Format'])" == 1
xpath "string(//textarea[@name='message'])" == "Text"

POST {{base}}{{create_action}}
[Multipart]
date: 01.03.2025 09:15
message:
HTTP 200
[Asserts]
xpath "count(//text()[normalize-space()='Feld darf nicht leer sein'])" == 1
xpath "string(//input[@name='date']/@value)" == "01.03.2025 09:15"

POST {{base}}{{create_action}}
[Multipart]
date:
message: Text
HTTP 200
[Asserts]
xpath "count(//text()[normalize-space()='Feld darf nicht leer sein'])" == 1

# Create with two attachments
POST {{base}}{{create_action}}
[Multipart]
date: 01.03.2025 09:15
message: Neue **Nachricht** mit Link https://example.org/seite
attachments: file,anhang.txt;
attachments: file,zweiter.txt;
HTTP 200
[Asserts]
xpath "string(//*[@role='alert'])" == "Nachricht erfolgreich erstellt"

GET {{base}}/
HTTP 200
[Asserts]
xpath "count(//text()[normalize-space()='Samstag, 01. März 2025'])" == 1
xpath "contains(substring-after(string(//body), 'Samstag, 01. März 2025'), 'Donnerstag, 20. Februar 2025')" == true
xpath "count(//strong[normalize-space()='Nachricht'])" == 1
xpath "string(//a[normalize-space()='https://example.org/seite']/@href)" == "https://example.org/seite"
xpath "string(//a[normalize-space()='anhang.txt']/@href)" == "/news/3/anhang.txt"
xpath "string(//a[normalize-space()='zweiter.txt']/@href)" == "/news/3/zweiter.txt"
[Captures]
# The edit link inside the box around the new post's date
edit_url: xpath "string((//*[normalize-space()='Samstag, 01. März 2025']/ancestor::*[.//a[normalize-space()='Ändern']])[last()]//a[normalize-space()='Ändern']/@href)"

GET {{base}}/news/3/anhang.txt
HTTP 200
[Asserts]
body == "Anhang Inhalt\n"

# Edit form shows the stored values and one checked checkbox per attachment
GET {{base}}{{edit_url}}
HTTP 200
[Asserts]
xpath "string(//title)" == "Nachricht Editieren"
xpath "string(//input[@id=//label[normalize-space()='Datum']/@for]/@value)" == "01.03.2025 09:15"
xpath "string(//textarea[@id=//label[normalize-space()='Nachricht']/@for])" == "Neue **Nachricht** mit Link https://example.org/seite"
xpath "count(//input[@id=//label[normalize-space()='anhang.txt']/@for][@type='checkbox'][@checked])" == 1
xpath "count(//input[@id=//label[normalize-space()='zweiter.txt']/@for][@type='checkbox'][@checked])" == 1
xpath "string(//input[@id=//label[normalize-space()='zweiter.txt']/@for]/@name)" == "newFileCheckbox"
xpath "string(//input[@id=//label[normalize-space()='zweiter.txt']/@for]/@value)" == "zweiter.txt"
[Captures]
edit_action: xpath "string(//form[.//button[normalize-space()='Speichern']]/@action)"

# Unchecked attachments are removed, checked ones kept
POST {{base}}{{edit_action}}
[Multipart]
date: 02.03.2025 10:00
message: Editierte Nachricht
newFileCheckbox: zweiter.txt
HTTP 200
[Asserts]
xpath "string(//*[@role='alert'])" == "Nachricht erfolgreich editiert"

GET {{base}}/
HTTP 200
[Asserts]
xpath "count(//text()[normalize-space()='Sonntag, 02. März 2025'])" == 1
xpath "count(//text()[normalize-space()='Samstag, 01. März 2025'])" == 0
xpath "count(//p[normalize-space()='Editierte Nachricht'])" == 1
xpath "count(//a[normalize-space()='anhang.txt'])" == 0
xpath "string(//a[normalize-space()='zweiter.txt']/@href)" == "/news/3/zweiter.txt"

GET {{base}}/news/3/anhang.txt
HTTP 404

POST {{base}}{{edit_action}}
[Multipart]
date: kein datum
message: Editierte Nachricht
HTTP 200
[Asserts]
xpath "count(//text()[normalize-space()='Falsches Format'])" == 1

# Delete, with confirmation
GET {{base}}/
HTTP 200
[Captures]
delete_url: xpath "string((//*[normalize-space()='Donnerstag, 20. Februar 2025']/ancestor::*[.//a[normalize-space()='Löschen']])[last()]//a[normalize-space()='Löschen']/@href)"

GET {{base}}{{delete_url}}
HTTP 200
[Asserts]
xpath "string(//title)" == "Nachricht Löschen"
body contains "Nachricht wirklich löschen?"
body contains "Protokoll der letzten Sitzung im Anhang."
xpath "string(//form[.//button[normalize-space()='Ja, Nachricht löschen!']]/@method)" == "post"
[Captures]
delete_action: xpath "string(//form[.//button[normalize-space()='Ja, Nachricht löschen!']]/@action)"

POST {{base}}{{delete_action}}
HTTP 200
[Asserts]
xpath "string(//*[@role='alert'])" == "Nachricht erfolgreich gelöscht"

GET {{base}}/
HTTP 200
[Asserts]
xpath "count(//text()[normalize-space()='Donnerstag, 20. Februar 2025'])" == 0
body not contains "Protokoll der letzten Sitzung im Anhang."
xpath "count(//a[normalize-space()='protokoll.txt'])" == 0

GET {{base}}/news/2/protokoll.txt
HTTP 404

# Script tags in posts are sanitized
POST {{base}}{{create_action}}
[Multipart]
date: 03.03.2025 09:15
message: Hallo <script>alert(1)</script> Welt
HTTP 200

GET {{base}}/
HTTP 200
[Asserts]
body not contains "<script>alert(1)</script>"
body contains "Hallo"

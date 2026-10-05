# Activities and the work time members log against them.

POST {{base}}/login
[Form]
email: member@example.com
password: member-password
HTTP 302

GET {{base}}/activities
HTTP 200
[Asserts]
xpath "string(//title)" == "Activities"
xpath "string(//h1)" == "Activities"
xpath "count(//a[normalize-space()='Öffnen'])" == 2
body contains "Weihnachtsmarkt"
body contains "Marktplatz Achern"
body contains "Glühweinstand"
body contains "07. Dezember 2024 15:00"
body contains "Blutspende"
xpath "count(//a[normalize-space()='Neue Activity'])" == 0
xpath "count(//a[normalize-space()='Bearbeiten'])" == 0
xpath "count(//a[normalize-space()='Löschen'])" == 0
[Captures]
# The "Öffnen" link in the box around the activity title
markt_url: xpath "string((//h2[normalize-space()='Weihnachtsmarkt']/ancestor::*[.//a[normalize-space()='Öffnen']])[last()]//a[normalize-space()='Öffnen']/@href)"
blut_url: xpath "string((//h2[normalize-space()='Blutspende']/ancestor::*[.//a[normalize-space()='Öffnen']])[last()]//a[normalize-space()='Öffnen']/@href)"

# Default tab: log time, with a summary of what you logged already
GET {{base}}{{markt_url}}
HTTP 200
[Asserts]
xpath "string(//title)" == "Weihnachtsmarkt"
xpath "string(//h1)" == "Weihnachtsmarkt"
body contains "07. Dezember 2024 15:00"
body contains "Marktplatz Achern"
xpath "count(//p[normalize-space()='Du hast bereits 2 Stunden und 30 Minuten eingetragen.'])" == 1
xpath "string(//form[.//button[normalize-space()='Speichern']]/@method)" == "post"
xpath "string(//input[@id=//label[normalize-space()='Wann die Arbeit geleistet wurde']/@for]/@name)" == "date"
xpath "string(//input[@id=//label[normalize-space()='Wann die Arbeit geleistet wurde']/@for]/@type)" == "date"
xpath "string(//input[@id=//label[normalize-space()='Stunden']/@for]/@name)" == "hours"
xpath "string(//input[@id=//label[normalize-space()='Minuten']/@for]/@name)" == "minutes"
xpath "count(//a[normalize-space()='Bearbeiten'])" == 0
xpath "count(//a[normalize-space()='Zeit hinzufügen'])" == 1
[Captures]
markt_action: xpath "string(//form[.//button[normalize-space()='Speichern']]/@action)"
details_tab: xpath "string(//a[normalize-space()='Details']/@href)"
summary_tab: xpath "string(//a[normalize-space()='Zusammenfassung']/@href)"

GET {{base}}{{blut_url}}
HTTP 200
[Asserts]
xpath "string(//h1)" == "Blutspende"
xpath "count(//p[normalize-space()='Du hast bereits 0 Stunden und 45 Minuten eingetragen.'])" == 1

# Details tab: own times with delete links, other people's times grouped
GET {{base}}{{markt_url}}{{details_tab}}
HTTP 200
[Asserts]
xpath "count(//h2[normalize-space()='Eigene Zeiten']/following::table[1]//tr[td[normalize-space()='07.12.2024'] and td[normalize-space()='02:30']])" == 1
xpath "count(//h2[normalize-space()='Eigene Zeiten']/following::table[1]//tbody/tr)" == 1
xpath "count(//a[normalize-space()='Löschen'])" == 1
xpath "string(//a[normalize-space()='board@example.com']/@href)" == "/nutzer/4"
xpath "contains(substring-after(string(//body), 'Zeiten anderer'), 'board@example.com')" == true
xpath "count(//a[normalize-space()='board@example.com']/following::table[1]//td[normalize-space()='01:00'])" == 1
[Captures]
delete_time_url: xpath "string(//tr[td[normalize-space()='07.12.2024']]//a[normalize-space()='Löschen']/@href)"

GET {{base}}{{markt_url}}{{summary_tab}}
HTTP 200
[Asserts]
body contains "Insgesamt wurden 3 Stunden und 30 Minuten von 2 Personen eingetragen."

# Log time
POST {{base}}{{markt_action}}
[Form]
date: 2025-01-05
hours: 1
minutes: 15
HTTP 200
[Asserts]
xpath "count(//p[normalize-space()='Du hast bereits 3 Stunden und 45 Minuten eingetragen.'])" == 1
xpath "string(//input[@name='hours']/@value)" == "0"

# Date is optional
POST {{base}}{{markt_action}}
[Form]
hours: 0
minutes: 30
HTTP 200
[Asserts]
xpath "count(//p[normalize-space()='Du hast bereits 4 Stunden und 15 Minuten eingetragen.'])" == 1

GET {{base}}{{markt_url}}{{details_tab}}
HTTP 200
[Asserts]
xpath "count(//h2[normalize-space()='Eigene Zeiten']/following::table[1]//tbody/tr)" == 3
xpath "count(//h2[normalize-space()='Eigene Zeiten']/following::table[1]//tr[td[normalize-space()='05.01.2025'] and td[normalize-space()='01:15']])" == 1
xpath "count(//h2[normalize-space()='Eigene Zeiten']/following::table[1]//tr[td[normalize-space()='00:30']])" == 1
# Newest own entry first
xpath "contains(substring-after(string(//body), '05.01.2025'), '07.12.2024')" == true

# Validation keeps the entered values
POST {{base}}{{markt_action}}
[Form]
hours: 0
minutes: 0
HTTP 200
[Asserts]
xpath "count(//text()[normalize-space()='Stunden und Minuten dürfen nicht beide 0 sein'])" == 2

POST {{base}}{{markt_action}}
[Form]
hours: abc
minutes: 5
HTTP 200
[Asserts]
xpath "count(//text()[normalize-space()='Stunden müssen eine ganze Zahl sein'])" == 1
xpath "string(//input[@name='hours']/@value)" == "abc"

POST {{base}}{{markt_action}}
[Form]
date: 05.01.2025
hours: 1
minutes: 0
HTTP 200
[Asserts]
xpath "count(//text()[normalize-space()='Datum ungültig'])" == 1

POST {{base}}{{markt_action}}
[Form]
hours: -1
minutes: 0
HTTP 200
[Asserts]
xpath "count(//text()[normalize-space()='Stunden müssen positiv sein'])" == 1

GET {{base}}{{markt_url}}{{summary_tab}}
HTTP 200
[Asserts]
body contains "Insgesamt wurden 5 Stunden und 15 Minuten von 4 Personen eingetragen."

# Delete an own time with confirmation
GET {{base}}{{delete_time_url}}
HTTP 200
[Asserts]
xpath "string(//title)" == "Zeit löschen"
xpath "string(//*[@role='alert'])" == "Soll die Zeit vom 07.12.2024 wirklich gelöscht werden?"
xpath "string(//form[.//button[normalize-space()='Löschen']]/@method)" == "post"
xpath "string(//a[normalize-space()='Abbrechen']/@href)" == "{{markt_url}}"
[Captures]
delete_time_action: xpath "string(//form[.//button[normalize-space()='Löschen']]/@action)"

POST {{base}}{{delete_time_action}}
HTTP 200
[Asserts]
body contains "Zeit vom 07.12.2024 gelöscht"

GET {{base}}{{markt_url}}
HTTP 200
[Asserts]
xpath "count(//p[normalize-space()='Du hast bereits 1 Stunden und 45 Minuten eingetragen.'])" == 1

# Unknown activity renders a notice
GET {{base}}/activities/999
HTTP 200
[Asserts]
xpath "string(//title)" == "Activity nicht gefunden"
xpath "string(//*[@role='alert'])" == "Activity nicht gefunden"

# Admin only
GET {{base}}/activities/create
HTTP 403

POST {{base}}/activities/create
[Form]
name: sneaky
HTTP 403

GET {{base}}/activities/1/edit
HTTP 403

POST {{base}}/activities/1/edit
[Form]
name: sneaky
HTTP 403

GET {{base}}/activities/1/delete
HTTP 403

POST {{base}}/activities/1/delete
HTTP 403

POST {{base}}/logout
HTTP 302

POST {{base}}/login
[Form]
email: admin@example.com
password: admin-password
HTTP 302

GET {{base}}/activities
HTTP 200
[Asserts]
xpath "count(//a[normalize-space()='Neue Activity'])" == 1
xpath "count(//a[normalize-space()='Bearbeiten'])" == 2
xpath "count(//a[normalize-space()='Löschen'])" == 2
[Captures]
create_url: xpath "string(//a[normalize-space()='Neue Activity']/@href)"
blut_delete_url: xpath "string((//h2[normalize-space()='Blutspende']/ancestor::*[.//a[normalize-space()='Löschen']])[last()]//a[normalize-space()='Löschen']/@href)"

GET {{base}}{{markt_url}}
HTTP 200
[Asserts]
xpath "count(//a[normalize-space()='Bearbeiten'])" == 1

GET {{base}}{{create_url}}
HTTP 200
[Asserts]
xpath "string(//title)" == "Neue Activity"
xpath "string(//input[@id=//label[normalize-space()='Name']/@for]/@name)" == "name"
xpath "string(//textarea[@id=//label[normalize-space()='Beschreibung']/@for]/@name)" == "description"
xpath "string(//input[@id=//label[normalize-space()='Ort']/@for]/@name)" == "location"
xpath "string(//input[@id=//label[normalize-space()='Datum']/@for]/@name)" == "date"
xpath "count(//text()[normalize-space()=\"Bitte als Format '12.01.2022 15:00' verwenden.\"])" == 1
[Captures]
create_action: xpath "string(//form[.//button[normalize-space()='Speichern']]/@action)"

POST {{base}}{{create_action}}
[Form]
name:
description:
location: Bauhof
date: 15.03.2026 08:00
HTTP 200
[Asserts]
xpath "count(//text()[normalize-space()='Titel darf nicht leer sein'])" == 1

POST {{base}}{{create_action}}
[Form]
name: Altpapiersammlung
description:
location: Bauhof
date: 2026-03-15
HTTP 200
[Asserts]
xpath "count(//text()[normalize-space()='Datum ungültig'])" == 1
xpath "string(//input[@name='name']/@value)" == "Altpapiersammlung"

POST {{base}}{{create_action}}
[Form]
name: Altpapiersammlung
description: Sammeln im Stadtgebiet
location: Bauhof
date: 15.03.2026 08:00
HTTP 200
[Asserts]
xpath "string(//*[@role='alert'])" == "Activity gespeichert"

# Location and date are optional
POST {{base}}{{create_action}}
[Form]
name: Ohne Details
description:
location:
date:
HTTP 200
[Asserts]
xpath "string(//*[@role='alert'])" == "Activity gespeichert"

GET {{base}}/activities
HTTP 200
[Asserts]
xpath "count(//a[normalize-space()='Öffnen'])" == 4
body contains "Altpapiersammlung"
body contains "Sammeln im Stadtgebiet"
body contains "15. März 2026 08:00"
body contains "Ohne Details"
[Captures]
papier_url: xpath "string((//h2[normalize-space()='Altpapiersammlung']/ancestor::*[.//a[normalize-space()='Öffnen']])[last()]//a[normalize-space()='Öffnen']/@href)"
papier_edit_url: xpath "string((//h2[normalize-space()='Altpapiersammlung']/ancestor::*[.//a[normalize-space()='Bearbeiten']])[last()]//a[normalize-space()='Bearbeiten']/@href)"

GET {{base}}{{papier_edit_url}}
HTTP 200
[Asserts]
xpath "string(//title)" == "Activity bearbeiten"
xpath "string(//input[@id=//label[normalize-space()='Name']/@for]/@value)" == "Altpapiersammlung"
xpath "string(//textarea[@id=//label[normalize-space()='Beschreibung']/@for])" == "Sammeln im Stadtgebiet"
xpath "string(//input[@id=//label[normalize-space()='Ort']/@for]/@value)" == "Bauhof"
xpath "string(//input[@id=//label[normalize-space()='Datum']/@for]/@value)" == "15.03.2026 08:00"
[Captures]
papier_edit_action: xpath "string(//form[.//button[normalize-space()='Speichern']]/@action)"

POST {{base}}{{papier_edit_action}}
[Form]
name: Altpapier
description: Sammeln im Stadtgebiet
location: Bauhof
date: 16.03.2026 09:00
HTTP 200
[Asserts]
xpath "string(//*[@role='alert'])" == "Activity gespeichert"

GET {{base}}{{papier_url}}
HTTP 200
[Asserts]
xpath "string(//h1)" == "Altpapier"
body contains "16. März 2026 09:00"

GET {{base}}{{blut_delete_url}}
HTTP 200
[Asserts]
xpath "string(//title)" == "Activity Löschen"
xpath "string(//h1)" == "Activity Blutspende löschen?"
xpath "string(//form[.//button[normalize-space()='Löschen']]/@method)" == "post"
[Captures]
blut_delete_action: xpath "string(//form[.//button[normalize-space()='Löschen']]/@action)"

POST {{base}}{{blut_delete_action}}
HTTP 200
[Asserts]
xpath "string(//*[@role='alert'])" == "Activity Blutspende gelöscht"
xpath "string(//a[normalize-space()='Zurück']/@href)" == "/activities"

GET {{base}}/activities
HTTP 200
[Asserts]
xpath "count(//a[normalize-space()='Öffnen'])" == 3
body not contains "Blutspende"

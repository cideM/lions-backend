# Black box tests

`go test ./test/` (from the repository root) runs every `test/hurl/*.hurl`
file against a freshly started server with its own copy of the fixture
database. The header of `blackbox_test.go` documents the sidecar files
(`.pre.sql`, `.vars`, `.env`) and how to pick the server under test.

## Writing assertions

The tests read pages the way a person does, so that the markup can change
freely as long as the page still says and does the same things.

Allowed:

- status codes, redirect targets, entry URLs, attachment URLs
- `<title>`, headings, link and button text, table header and cell text
- messages via `//*[@role='alert']` or their literal text; count phrases as
  text nodes, `count(//text()[normalize-space()='Mit Familie'])`, because an
  element whose only content is the phrase matches `//*` too
- form field *names* (they are the wire format), form `action`, `method`,
  `enctype`, `checked`, `selected`, `disabled`
- fields located through their label:
  `//input[@id=//label[normalize-space()='Vorname']/@for]`
- forms located through their submit button:
  `//form[.//button[normalize-space()='Speichern']]`
- tables located through a header cell:
  `//table[thead//th[normalize-space()='Email']]`
- "the box around X", i.e. the innermost element that contains X and Y:
  `(//*[normalize-space()='X']/ancestor::*[.//a[normalize-space()='Y']])[last()]`
- reading order through string position:
  `contains(substring-after(string(//body), 'first'), 'second')`

Not allowed: CSS classes, element positions like `(//div)[2]`, nesting
depth, ids that exist only for styling or scripting.

Prefer following links: capture the `href` of the link a person would click
and request that, instead of spelling out every URL.

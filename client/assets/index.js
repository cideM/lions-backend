const $emailButton = document.getElementById("email-button");

const selected = new Set();
let $checkboxes;

function updateEmailButton() {
  $emailButton.href = "mailto:" + Array.from(selected).sort().join(",");
  if (selected.size > 0) {
    $emailButton.classList.remove("disabled");
  } else {
    $emailButton.classList.add("disabled");
  }
}

function onClickToggle() {
  switch (selected.size) {
    case $checkboxes.length: {
      $checkboxes.forEach(($cb) => {
        $cb.checked = false;
        selected.clear();
      });
      break;
    }
    case 0:
      $checkboxes.forEach(($cb) => {
        $cb.checked = true;
        selected.add($cb.dataset.email);
      });
      break;
    default:
      $checkboxes.forEach(($cb) => {
        $cb.checked = true;
        selected.add($cb.dataset.email);
      });
      break;
  }
  updateEmailButton();
}

function onClickCheckbox(event) {
  const $el = event.currentTarget;
  if ($el.checked) {
    selected.add($el.dataset.email);
  } else {
    selected.delete($el.dataset.email);
  }
  updateEmailButton();
}

if ($emailButton) {
  $checkboxes = document.querySelectorAll("[data-email]");

  document
    .getElementById("toggle-email-button")
    .addEventListener("click", onClickToggle);

  $checkboxes.forEach(($cb) => {
    $cb.addEventListener("click", onClickCheckbox);
  });
}

shinyjs.initfocus = function () {
    Shiny.setInputValue("focused", "");
}

document.addEventListener('focusin', function(event) {
    Shiny.setInputValue("focused", event.target.id);
});

document.addEventListener('focusout', function(event) {
    Shiny.setInputValue("focused", "");
});
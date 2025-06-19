shinyjs.uishape = function (elementId) {
    var element = document.getElementById(elementId);
    if (element) {
        var width = element.offsetWidth;
        var height = element.offsetHeight;
        Shiny.onInputChange(elementId + '_width', width);
        Shiny.onInputChange(elementId + '_height', height);
    }
}

$(window).resize(function () {
    var w = $(this).width();
    var h = $(this).height();
    var obj = { width: w, height: h };
    Shiny.onInputChange("win_resize", obj);
});

document.addEventListener("keydown", function (event) {
    if (event.key === "Escape") {
        Shiny.onInputChange("escKey", Math.random());
    }

    if (event.key === 'Enter') {
        Shiny.onInputChange("retKey", Math.random());
    }

    if (event.key === 'n') {
        Shiny.onInputChange("newKey", Math.random());
    }

    if (event.key === 'r') {
        Shiny.onInputChange("remKey", Math.random());
    }

});
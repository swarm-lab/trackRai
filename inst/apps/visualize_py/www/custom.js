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
    if (event.key === "Escape" || event.keyCode === 27) {
        Shiny.onInputChange("escKey", Math.random());
    }

    if (event.key === 'Enter' || event.keyCode === 13) {
        Shiny.onInputChange("retKey", Math.random());
    }

});
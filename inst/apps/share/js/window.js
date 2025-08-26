shinyjs.uishape = function (elementId) {
    var element = document.getElementById(elementId);
    if (element) {
        var width = element.offsetWidth;
        var height = element.offsetHeight;
        Shiny.onInputChange(elementId + '_uiwidth', width);
        Shiny.onInputChange(elementId + '_uiheight', height);
    }
};

shinyjs.imgshape = function (elementId) {
    var element = document.getElementById(elementId);
    if (element) {
        var width = element.offsetHeight * (element.naturalWidth / element.naturalHeight);
        var height = element.offsetHeight;
        Shiny.onInputChange(elementId + '_imgwidth', width);
        Shiny.onInputChange(elementId + '_imgheight', height);
    }
};

$(window).resize(function () {
    var w = $(this).width();
    var h = $(this).height();
    var obj = { width: w, height: h };
    Shiny.onInputChange("win_resize", obj);
});

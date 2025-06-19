window.addEventListener("keydown", function (event) {
    if (event.code === "Escape") {
        Shiny.onInputChange("escKey", Math.random());
    }

    if (event.code === "Enter") {
        Shiny.onInputChange("retKey", Math.random());
    }

    if (event.code === "KeyN") {
        Shiny.onInputChange("nKey", Math.random());
    }

    if (event.code === "KeyQ") {
        Shiny.onInputChange("qKey", Math.random());
    }

    if (event.code === "KeyW") {
        Shiny.onInputChange("wKey", Math.random());
    }

    if (event.code === "KeyE") {
        Shiny.onInputChange("eKey", Math.random());
    }

    if (event.code === "KeyR") {
        Shiny.onInputChange("rKey", Math.random());
    }

    if (event.code === "KeyA") {
        Shiny.onInputChange("aKey", Math.random());
    }

    if (event.code === "KeyS") {
        Shiny.onInputChange("sKey", Math.random());
    }

    if (event.code === "Space") {
        Shiny.onInputChange("spaceKey", Math.random());
    }

    if (event.code === "ArrowLeft") {
        Shiny.onInputChange("leftKey", Math.random());
    }

    if (event.code === "ArrowRight") {
        Shiny.onInputChange("rightKey", Math.random());
    }

    if (event.code === "ArrowUp") {
        Shiny.onInputChange("upKey", Math.random());
    }

    if (event.code === "ArrowDown") {
        Shiny.onInputChange("downKey", Math.random());
    }
});
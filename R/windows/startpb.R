startpb <-
function(min=0, max=1)
{
    if (dopb()) {
        control <- getOption("pboptions")
        pb <- switch(control$type,
            fct = fctProgressBar(min = min, max = max,
                initial = control$initial,
                style = control$style, width = control$txt.width,
                char = control$char,
                fct = control$fct,
                label_name = control$label_name),
            timer = timerProgressBar(min = min, max = max,
                initial = control$initial,
                style = control$style, width = control$txt.width,
                char = control$char,
                min_time = control$min_time, file = control$file),
            txt = txtProgressBar(min = min, max = max,
                initial = control$initial,
                style = control$style, width = control$txt.width,
                char = control$char, file = control$file),
            win = winProgressBar(min = min, max = max,
                initial = control$initial,
                title = control$title, label = control$label,
                width = control$gui.width),
            tk = tcltk::tkProgressBar(min = min, max = max,
                initial=control$initial,
                title = control$title, label = control$label,
                width = control$gui.width))
    } else {
        pb <- NULL
    }
    invisible(pb)
}

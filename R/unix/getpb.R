getpb <-
function(pb)
{
    if (dopb()) {
        progress.bar <- getOption("pboptions")$type
        rval <- switch(progress.bar,
            fct = getFctProgressBar(pb),
            timer = getTxtProgressBar(pb),
            txt = getTxtProgressBar(pb),
            tk = tcltk::getTkProgressBar(pb))
    } else {
        rval <- NULL
    }
    rval
}


fctProgressBar <- function (min = 0, max = 1, initial = 0, char = "=", width = NA,
          title = "", label, style = 1, fct = function(txt) cat(txt), label_name = "")
{
    calling <- title
    try({
        if (length(label_name) == 1 && is.character(label_name) && label_name != "" && exists(label_name, parent.frame(3)) &&
            !is.null(get(label_name, parent.frame(3))))
            calling <- as.character(get(label_name, parent.frame(3)))
        else
            calling <- deparse(sys.call(-3))
        if (all(is.na(calling)))
            calling <- deparse(sys.call(-3))
        if (nchar(calling) > 20)
            calling <- paste0(substr(calling, 1, 16), " ...", collapse = "")
    }, silent = TRUE)
    if (!(inherits(fct, "function")) || !("txt" %in% names(formals(fct))))
        stop("'fct' must be \"\" or a function with a formal argument txt")
    if (!style %in% 1L:5L)
        style <- 3
    .val <- initial
    .killed <- FALSE
    .nb <- 0L
    .pc <- -1L
    nw <- nchar(char, "w")
    if (is.na(width)) {
        width <- getOption("width")
        if (style == 5L)
            width <- width - 10L
        width <- trunc(width/nw)
    }
    if (max <= min)
        stop("must have 'max' > 'min'")
    up3 <- function(value) {
        if (!is.finite(value) || value < min || value > max)
            return()
        .val <<- value
        nb <- round(width * (value - min)/(max - min))
        pc <- round(100 * (value - min)/(max - min))
        if (nb == .nb && pc == .pc)
            return()
        fct(txt = paste(calling, paste(c("|", rep.int(char, nb), rep.int(" ",
                                                                    nw * (width - nb)), sprintf("| %3d%%", pc)), collapse = "")
        ))
        .nb <<- nb
        .pc <<- pc
    }
    up4 <- function(value) {
        if (!is.finite(value) || value < min || value > max)
            return()
        .val <<- value
        nb <- round(width * (value - min)/(max - min))
        pc <- round(100 * (value - min)/(max - min))
        if (nb == .nb && pc == .pc)
            return()
        fct(txt = paste(paste(c("|", rep.int(char, nb), rep.int(" ",
                                                                nw * (width - nb)), sprintf("| %3d%%", pc)), collapse = "")
        ))
        .nb <<- nb
        .pc <<- pc
    }
    up1 <- function(value) {
        if (!is.finite(value) || value < min || value > max)
            return()
        .val <<- value
        nb <- round(width * (value - min)/(max - min))
        if (.nb < nb) {
            fct(txt = strrep(char, nb - .nb))
        }
        else if (.nb > nb) {
            fct(txt = paste("\r", strrep(" ", .nb * nw), "\r", strrep(char,
                                                          nb), sep = ""))
        }
        .nb <<- nb
    }
    up2 <- function(value) {
        if (!is.finite(value) || value < min || value > max)
            return()
        .val <<- value
        nb <- round(width * (value - min)/(max - min))
        if (.nb <= nb) {
            fct(txt = paste("\r", strrep(char, nb), sep = ""))
        }
        else {
            fct(txt = paste("\r", strrep(" ", .nb * nw), "\r", strrep(char,
                                                          nb), sep = ""))
        }
        .nb <<- nb
    }
    up5 <- function(value) {
        if (!is.finite(value) || value < min || value > max)
            return()
        .val <<- value
        nb <- round(width * (value - min)/(max - min))
        pc <- round(100 * (value - min)/(max - min))
        if (nb == .nb && pc == .pc)
            return()
        fct(txt = paste(paste0("\r  |", strrep(" ", nw * width + 6))))
             fct(txt = paste(paste(c("\r  |", rep.int(char, nb), rep.int(" ",
                                                        nw * (width - nb)), sprintf("| %3d%%", pc)), collapse = "")
            ))
        .nb <<- nb
        .pc <<- pc
    }
    getVal <- function() .val
    kill <- function() if (!.killed) {
        fct(txt = "\n")
        .killed <<- TRUE
    }
    kill_log3 <- function() if (!.killed) {
        fct(txt = sprintf("%s done.", calling))
        .killed <<- TRUE
    }
    kill_log4 <- function() if (!.killed) {
        fct(txt = "done.")
        .killed <<- TRUE
    }
    kill <- switch(style, kill, kill, kill_log3, kill_log4, kill)
    up <- switch(style, up1, up2, up3, up4, up5)
    up(initial)
    structure(list(getVal = getVal, up = up, kill = kill), class = c("fctProgressBar", "txtProgressBar"))
}

setFctProgressBar <- function (pb, value, title = "", label = NULL)
{
    if (!inherits(pb, "fctProgressBar"))
        stop(gettextf("'pb' is not from class %s", dQuote("fctProgressBar")),
             domain = NA)
    oldval <- pb$getVal()
    pb$up(value)
    pb$calling <- title
    invisible(oldval)
}

getFctProgressBar <- function (pb)
{
    if (!inherits(pb, "fctProgressBar"))
        stop(gettextf("'pb' is not from class %s", dQuote("fctProgressBar")),
             domain = NA)
    pb$getVal()
}

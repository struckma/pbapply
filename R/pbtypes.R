pbtypes <-
function()
{
    TYPES <- c("timer", "txt", "tk", "none", "fct")
    if (.Platform$OS.type == "windows")
        c(TYPES, "win") else TYPES
}

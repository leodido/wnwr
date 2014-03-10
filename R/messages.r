missing_message <- function(var) paste0("'", deparse(substitute(var)), "'", " arg is missing.")

na_message <- function(var) paste0("'", deparse(substitute(var)), "'", " arg is na.")
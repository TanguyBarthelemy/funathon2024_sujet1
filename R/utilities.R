################################################################################
#######                     Some utilities functions                     #######
################################################################################

# Lecture des fichiers de donnée
get_data <- function(path, erase_zero = TRUE, sep = ";", dec = ".", ...) {
    path <- normalizePath(path, mustWork = TRUE)
    data <- read.csv(
        file = path,
        sep = sep,
        dec = dec,
        header = TRUE,
        encoding = "UTF-8",
        ...
    )
    if (erase_zero) {
        data[data == 0] <- NA_real_
    }
    return(data)
}

# Lecture des fichiers de donnée
write_data <- function(data, path, row.names = FALSE,...) {
    path <- normalizePath(path, mustWork = FALSE)
    write.table(
        x = data,
        file = path,
        quote = FALSE,
        row.names = row.names,
        sep = ";",
        dec = ".",
        na = "",
        ...
    )
    return(invisible(path))
}

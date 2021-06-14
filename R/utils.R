isTRUE <- function(x) identical(x, TRUE)
isFALSE <- function(x) identical(x, FALSE)
isNA <- function(x) identical(x, NA)
isTF <- function(x) is.logical(x) & length(x) == 1L & !anyNA(x)

`%||%` <- function(x, y) if (is.null(x)) y else x


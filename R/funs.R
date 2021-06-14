#' Veto wrappers
#'
#' Veto wrappers
#'
#' @param x A parameter
#' @return None, called for side effects
#' @name veto_wrappers
#' @examples
#' foo <- function(a, b) {
#'   veto_integer(a)
#'   veto_double(b, 2L)
#'   a * prod(b)
#' }
#'
#' foo(1L, c(0.5, 1))
#' try(foo(1L, 0))
NULL

#' @rdname veto_wrappers
#' @param length The expected length
#' @export
veto_character <- function(x, length = NULL) {
  do_veto_class_length(x, "character", length)
}

#' @rdname veto_wrappers
#' @export
veto_integer <- function(x, length = NULL) {
  do_veto_class_length(x, "integer", length)
}

#' @rdname veto_wrappers
#' @export
veto_double <- function(x, length = NULL) {
  do_veto_class_length(x, "double", length, type = TRUE)
}

#' @rdname veto_wrappers
#' @export
veto_numeric <- function(x, length = NULL) {
  do_veto_class_length(x, "numeric", length)
}

#' @rdname veto_wrappers
#' @export
veto_date <- function(x, length = NULL) {
  do_veto_class_length(x, "Date", length)
}

#' @rdname veto_wrappers
#' @export
veto_datetime <- function(x, length = NULL) {
  do_veto_class_length( x, "POSIXt", length)
}

#' @rdname veto_wrappers
#' @param class Character vector of the required class
#' @export
veto_class <- function(x, class, length = NULL) {
  do_veto_class_length(x, class, length)
}

do_veto_class_length <- function(x, class, length = NULL, type = FALSE) {
  sx <- deparse(eval(substitute(substitute(x)), parent.frame()))

  if (!is.character(class)) {
    stop("veto error: class must be a character")
  }

  if (!(is.null(length) || is.integer(length) & length(length) == 1L)) {
    stop("veto error: length must be an integer length 1L or NULL")
  }

  ok <- if (type) {
    class %in% typeof(x)
  } else {
    inherits(x, class)
  }

  msg <- veto_msg(sx, sprintf("must be %s", class))

  if (!is.null(length)) {
    ok <- ok & length(x) == length
    msg$message <- sprintf("%s length %iL", msg$message, length)
  }

  veto(!ok, msg)
}

#' @rdname veto_wrappers
#' @export
veto_length <- function(x, length = 1L) {
  if (!is.integer(length) & length(length) != 1L) {
    stop("veto error: length must be an integer of length 1L")
  }

  ok <- length(x) == length
  msg <- veto_msg(substitute(x), "must be length", length)
  veto(!ok, msg)
}

#' @rdname veto_wrappers
#' @param n The number of
#' @export
veto_nchar <- function(x, n = 1) {
  ok <- identical(nchar(x), n)
  msg <- veto_msg(substitute(x), "must be nchar", length)
  veto(!ok, msg)
}

#' @rdname veto_wrappers
#' @export
veto_true_false <- function(x) {
  ok <- is.logical(x) & length(x) == 1L & !anyNA(x)
  msg <- veto_msg(substitute(x), "must be TRUE or FALSE")
  veto(!ok, msg)
}

#' Veto unless
#'
#' A veto'ing template
#'
#' @param parameter A parameter to check
#' @param condition A condition that evaluates to either `TRUE` or `FALSE`; if
#'   `FALSE` a `veto` will be triggered
#' @param message The `veto` message to display
#' @return None, called for side effects
#' @export
#' @examples
#' foo <- function(x) {
#'   veto_unless(x, is.character(x), "must be a character")
#'   sprintf("x = '%s' is a character", x)
#' }
#'
#' foo("letters")
#' try(foo(1:10))
veto_unless <- function(parameter, condition, message) {
  sub <- substitute(parameter)
  ok <- eval(as.expression(condition), envir = parent.frame(2))

  if (!isTF(ok)) {
    stop("veto error: condition must evaluate to TRUE or FALSE")
  }

  msg <- veto_msg(sub, message)
  veto(!ok, msg)
}

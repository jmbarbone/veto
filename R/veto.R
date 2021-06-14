#' Veto
#'
#' Veto something
#'
#' @param cond A condition, `TRUE` or `FALSE`
#' @param msg A
#' @return None, called for side effects

veto <- function(cond, msg) {
  if (isTRUE(cond)) {
    error <- vetoError(msg)
    signalCondition(error)
    stop(error)
  }
}

vetoError <- function(x = "", ...) {
  UseMethod("vetoError", x)
}

vetoError.default <- function(x = "", ...) {
  structure(
    list(message = paste0(x, collapse = " "), call = NULL),
    class = c("vetoError", "error", "condition")
  )
}

vetoError.vetoMessage <- function(x = "") {
  if (length(x$message) == 0) {
    x$message <- " "
  }

  structure(
    list(message = paste0(x$message, collapse = " "), call = NULL, param = x$param),
    class = c("vetoErrorCall", "vetoError", "error", "condition")
  )
}

#' @exportS3Method
conditionMessage.vetoError <- function(c, ...) {
  if (inherits(c, "vetoErrorCall")) {
    sprintf("<veto %s: %s>", c$param, c$message)
  } else {
    sprintf("<veto %s>", c$message)
  }
}

veto_msg <- function(..., .n = FALSE) {
  ls <- list(...)
  if (.n) ls <- c(ls, list("or NULL"))
  ls <- as.vector(ls, "character")
  msg <- list(param = ls[1], message = ls[-1L])
  class(msg) <- c("vetoMessage", "character")
  msg
}

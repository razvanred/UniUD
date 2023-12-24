plot <- function(x, y, ...) {
  if (is.function(x) && is.null(attr(x, "class"))) {
    if (missing(y)) {
      y <- NULL
    }

    # check for ylab argument
    hasylab <- function(...) {
      !all(is.na(
        pmatch(
          names(list(...)),
          "ylab"
        )
      ))
    }

    if (hasylab(...)) {
      plot.function(x, y, ...)
    } else {
      plot.function(
        x, y,
        ylab = paste(
          deparse(substitute(x)),
          "(x)"
        ),
        ...
      )
    }
  } else {
    UseMethod("plot")
  }
}
plot(function(x) {
  x * x + 1
})

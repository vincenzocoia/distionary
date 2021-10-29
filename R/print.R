#' @export
print.dst <- function(x, ...) {
  cat(paste(class(x)))
  cat("\n\n", names(x[1]), ":\n")
  print(x[[1]])
}

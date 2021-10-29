#' Constructor Function for "parametric" distribution Objects
#'
#' @param l List containing the components of a parametric distribution object.
#' @param variable Type of random variable: "continuous", "discrete",
#'   or "mixed".
#' @param ... Attributes to add to the list.
#' @param class If making a subclass, specify its name here.
#' @export
new_parametric <- function(l, variable, ..., class = character()) {
  new_distribution(
    l,
    variable = variable,
    class    = c(class, "parametric")
  )
}


#' Parametric Distribution Objects
#' @param object Object to be tested
#' @rdname parametric
#' @export
is_parametric <- function(object) inherits(object, "parametric")

#' @rdname parametric
#' @export
is.parametric <- function(object) inherits(object, "parametric")

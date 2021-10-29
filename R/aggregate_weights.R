#' Aggregate discrete values
#'
#' Aggregates discrete values together with their weights
#' into a data frame or tibble.
#'
#' @param y Vector of outcomes.
#' @param weights Vector of weights, one for each of \code{y}.
#' These need not sum to one, but must not be negative.
#' @param sum_to_one Logical; should the weights be normalized
#' to sum to 1? Default is FALSE.
#' @return Data frame with the following columns:
#' \enumerate{
#'   \item \code{location}: Increasing vector of unique values of \code{y}
#'   having positive weight.
#'   \item \code{size}: Weights corresponding to each outcome.
#' }
#' @details
#' For a vector of outcomes \code{y} with a
#' matching vector of \code{weights},
#' \code{aggregate_weights()} provides a single non-zero, non-NA
#' weight per unique value of \code{y}.
#' @export
aggregate_weights <- function(y, weights, sum_to_one = FALSE) {
  stopifnot(identical(length(y), length(weights)))
  na_y <- is.na(y)
  na_w <- is.na(weights)
  na <- na_y | na_w
  clean_y <- y[!na]
  clean_w <- weights[!na]
  zero_w <- clean_w == 0
  cleaner_y <- clean_y[!zero_w]
  cleaner_w <- clean_w[!zero_w]
  if (length(cleaner_y) == 0L) {
    return(make_empty_discontinuities_df())
  }
  if (sum_to_one) {
    cleaner_w <- cleaner_w / sum(cleaner_w)
  }
  df <- stats::aggregate(
    data.frame(size = cleaner_w),
    by = list(location = cleaner_y),
    FUN = sum
  )
  df <- df[order(df[["location"]]), , drop = FALSE]
  convert_dataframe_to_tibble(df)
}

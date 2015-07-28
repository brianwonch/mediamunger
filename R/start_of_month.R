#' First date of calendar month
#'
#' @param date A 'Date' object, or character string in the format "%Y-%m-%d" or "%Y/%m/%d"
#'
#' @return A Date object that is the first date of the respective calendar month.
#' @export
#'
#' @examples start_of_month("2014/02/14")
start_of_month <- function(date) {
    as.Date(format(as.Date(date), "%Y-%m-01"))
}


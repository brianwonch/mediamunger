#' Convert a string to proper case
#'
#' @param str a character string or vector of strings
#'
#' @return string in proper case
#' @export
#'
#' @examples proper("abc")
proper <- function(str){
    paste0(toupper(substr(str, 1, 1)), tolower(substring(str, 2)))
}
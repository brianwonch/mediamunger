#' Return matched regexp group
#'
#' @param string a string or vector of strings
#' @param pattern a regexp string including parens to find a group
#'
#' @return a vector isolating the matching group, or NA where match not found
#' @export
#'
#' @examples return_match(row.names(mtcars), "(^[^\\s]+)")
return_match <- function(string, pattern){
    if(grepl("\\(+.*\\)+", pattern)){
        return(stringr::str_match(string,pattern)[,2])
    }
    else{
        warning("Pattern must include parentheses to find a matching group.")
    }
}

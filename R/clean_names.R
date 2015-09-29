#' Use only Lowercase and Underscore in Object Names
#'
#' @param x an object for which a names attribute will be meaningful
#'
#' @return The object \code{x} with names being only lowercase and underscore
#' @export
#'
#' @examples cleanNames(iris)
#' @seealso setNames
cleanNames <- function(x){
    names_vector <- names(x)
    names_vector <- gsub("\\\\u00ef\\.\\.",
                         "",
                         stringi::stri_escape_unicode(names_vector))
    names_vector <- gsub("_._","_&_",names_vector)
    names_vector <- gsub("\\s|\\/|\\.","_",names_vector)
    names_vector <- gsub("\\(|\\)","",names_vector)
    names_vector <- gsub("__","_",names_vector)
    names_vector <- gsub("_$","",names_vector)
    names_vector <- tolower(names_vector)
    setNames(x, names_vector)
}
#' Display strip creative size
#' @description Given an input vector, strips common elements designating creative sizes for display
#' @param colname field whose elements should be stripped of creative size
#'
#' @return a vector stripped of creative size
#' @export

strip_creative_size <- function(colname){
    #remove numbers with an x in between

    stripped_vector <- gsub("\\d{1,4}x\\d{1,4}",replacement="",x=colname,ignore.case=T)
    #remove trailing spaces with dash
    stripped_vector <- gsub("\\s+\\-\\s$",replacement="",x=stripped_vector)
    #remove week number
    stripped_vector <- gsub("(week|wk)(\\s+)?\\d{1,4}\\s+\\-\\s+",replacement="",x=stripped_vector,ignore.case=T)
    #remove leading and trailing hyphen or underscore characters
    stripped_vector <- gsub("(^(_|-))|((_|-)$)",replacement="",x=stripped_vector)
    #remove empty parens
    stripped_vector <- gsub("\\(\\)",replacement="",x=stripped_vector)
    #remove multispace
    stripped_vector <- gsub("\\s\\s+",replacement=" ",x=stripped_vector)
    stripped_vector <- stringr::str_trim(stripped_vector,side="both")
    return(stripped_vector)
}
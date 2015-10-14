#' Read in text from a SQL text file
#'
#' @param query_filepath a full path to a SQL text file
#'
#' @return a string with clean query text
#' @importFrom magrittr "%>%"
#' @export
#'
read_query <- function(query_filepath) { 
    # read in the query to a list of lines
    readLines(query_filepath, warn = FALSE) %>% 
        
    # strip special characters from each line
    lapply(function(str){
        str = gsub("\t+", "", str, perl = TRUE); # remove all tabs
        str = gsub("^\\s+", "", str, perl = TRUE); # remove leading whitespace
        str = gsub("\\s+$", "", str, perl = TRUE); # remove trailing whitespace
        str = gsub("[ ]+", " ", str, perl = TRUE); # collapse multiple spaces to a single space
        str = gsub("[--]+.*$", "", str, perl = TRUE); # destroy any comments
        return(str)
    }) %>% 
    
    # remove blank and/or comment lines
    base::Filter(f = function(x)x != "", x = .) %>% 
    
    # unlist the result, producing one vector
    unlist() %>% 
    
    
    # paste lines together into one-line string, spaces between.
    paste(collapse = " ") 
}
#' Change numeric columns into strings with commas.
#'
#' @param df input data frame whose numeric columns should be made comma'd strings
#'
#' @return df with numeric columns as pretty strings, using scales::comma.
#' @export
#' @importFrom magrittr "%>%"
#'
#' @examples comma_numeric_cols(mtcars)
comma_numeric_cols = function(df){
    numerics = df %>% sapply(function(x)is.numeric(x)) %>% which %>% names
    df[,numerics] = sapply(df[,numerics], function(x){
        commad = scales::comma(x)
        result = ifelse(grepl("\\.00$", commad), gsub("\\.00","", commad),commad)
        return(result)
    }
    ) %>% as.data.frame
    return(df)
}
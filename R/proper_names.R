#' Convert column names to proper case and convert underscore to spaces
#'
#' @param df a dataframe with column names
#' @param underscore_to_spaces boolean on whether to convert underscore to space in column names
#'
#' @return dataframe with adjusted column names
#' @export
#'
#' @examples proper_names(mtcars, underscore_to_spaces=T)
proper_names = function(df, underscore_to_spaces=TRUE){
    proper = function(x){
        paste0(toupper(substr(x, 1, 1)), tolower(substring(x, 2)))
    }
    nms = proper(names(df))
    if (underscore_to_spaces) nms = gsub("_"," ", nms)
    setNames(df, nms)
}
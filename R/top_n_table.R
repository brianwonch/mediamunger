#' Print a table of top n sorted variables
#'
#' @param df
#' @param arrange_vec Character vector indicating sort order. Strings must include desc() to sort in descending order
#' @param nrow number of rows to return, after sorting
#' @param pretty use scales::comma on numeric values and return a pandoc.table
#' @param ...

#'
#' @return A data.frame with top n cases
#' @export
#'
#' @examples top_n_table(mtcars, nrow = 3, pretty = FALSE)
#' @importFrom pander pandoc.table
#' @importFrom magrittr "%>%"
#' @importFrom scales comma
top_n_table = function(df, nrow=3, pretty=TRUE, ...){
    topn <- df %>%
        as.data.frame %>%
        head(nrow)
    if (pretty) {
        topn <- topn %>%
            comma_numeric_cols %>%
            pandoc.table
    }
    return(topn)
}

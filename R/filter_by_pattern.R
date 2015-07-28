#' Filter a data frame by values matching a pattern
#'
#' @param df A data frame
#' @param colname String column name on which to filter
#' @param pattern Regular expression pattern to match for filtering
#' @param exclude Exclude cases matching pattern? Defaults to FALSE.
#'
#' @return a filtered data frame
#' @export
#'
#' @importFrom dplyr filter_
#' @importFrom lazyeval interp
#' @examples filter_by_pattern(iris, "Species", "v.r")
#'
filter_by_pattern = function(df,
                             colname,
                             pattern,
                             exclude = FALSE,
                             ignore.case = T){

    if(exclude){

        filter_pattern = interp(~ !grepl(pattern,
                                         colname,
                                         ignore.case = ignore.case),
                                colname = as.name(colname)
                                )

    } else {

        filter_pattern = interp(~ grepl(pattern,
                                        colname,
                                        ignore.case = ignore.case),
                                colname = as.name(colname)
                                )

    }

    result = dplyr::filter_(df, filter_pattern)

    return(result)
}

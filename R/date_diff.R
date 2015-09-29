#' Show period-over-period reporting
#'
#' @param df input data frame
#' @param date_col string giving date field colname
#' @param key_colname string giving the "key" name (see \code{\link[tidyr]{gather_}})
#' @param value_colname string giving the "value" name (see \code{\link[tidyr]{gather_}})
#' @param cols_to_keep string vector indicating other columns to keep
#' @importFrom magrittr "%>%"
#' @return df period-over-period and other breakdowns
#' @export

date_diff = function(df,
                     date_col,
                     key_colname = "metric",
                     value_colname = "value",
                     cols_to_keep = NULL) {

    latest_date = df[[date_col]] %>% max %>% as.character

    earliest_date = df[[date_col]] %>% min %>% as.character

    df %>%
        tidyr::gather_(key_colname,
                       value_colname,
                       setdiff(names(df), c(date_col, cols_to_keep))) -> tmp


    #### Spread "value" by unique dates and calculate maxdate value / mindate value
    if (length(unique(tmp[[date_col]])) > 1) {
        tmp <- tmp %>% tidyr::spread_(date_col, value_colname)
        tmp[["percent_change"]] <- 100 * (tmp[[latest_date]] / tmp[[earliest_date]] - 1)
        tmp[["percent_change"]] <- round(tmp[["percent_change"]], 0)
    }

    return(tmp)
}
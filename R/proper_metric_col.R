#' Make a "metric" column proper
#'
#' @param df a dataframe with a column named "metric"
#' @param metric_levels_vec optional character vector indicating the order of metrics as factor.  This must include ALL unique levels in the metric column to avoid NAs
#'
#' @return data frame with "metric" column in proper case
#' @export
#'
#' @examples iris %>% tidyr::gather(metric, value, -Species) %>% proper_metric_col
proper_metric_col = function(df, metric_levels_vec = NULL){
    proper = function(x){
        paste0(toupper(substr(x, 1, 1)), tolower(substring(x, 2)))
    }
    if ("metric" %in% names(df)) {
        df = df %>%
            mutate(metric = proper(gsub("_|\\."," ", metric)))
    }

    if (!is.null(metric_levels_vec)) {
        df = df %>%
            mutate(metric = factor(metric, levels = metric_levels_vec))
    }
    return(df)
}

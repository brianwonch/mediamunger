
#' Get a vector of top campaign names in a sorted dataframe
#'
#' @param df data frame
#' @param week_num fiscal week number for filtering
#' @param n number of top campaigns to select (default is 3)
#' @param sort_on column to sort descending
#' @param week_lookup_colname column name indicating "week"
#' @param aggregate_function summarising function
#'
#' @return data frame
#' @export
#' @importFrom magrittr "%>%"
get_top_campaign_names = function(df,
                           week_num,
                           n = 3,
                           sort_on = "cost",
                           week_lookup_colname = "week",
                           aggregate_function) {
    df %>%
        dplyr::filter_(lazyeval::interp( ~ filter_var == week_num,
                                  filter_var = as.name(week_lookup_colname))) %>%
        dplyr::group_by(campaign, week) %>%
        aggregate_function %>%
        as.data.frame %>%
        dplyr::arrange_(.dots = paste0("desc(", sort_on, ")")) %>%
        head(n) %>%
        with(unique(campaign))

}
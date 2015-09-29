#' Summarise Kenshoo Metrics
#'
#' @param df A data frame with metrics following Kenshoo conventions
#'
#' @return An aggregated data frame with metrics following Kenshoo conventions, aggregated.
#' @importFrom magrittr "%>%"
#' @export
#'
summarise_kenshoo_metrics <- function(df){
    cleanNames(df) %>%
        dplyr::mutate(tot_rank = impressions * avg_pos) %>%
        dplyr::summarise(cost = sum(cost, na.rm = T),
                         impressions = sum(impressions, na.rm = T),
                         clicks = sum(clicks, na.rm = T),
                         cost_per_click = sum(cost, na.rm = T) /
                             sum(clicks, na.rm = T),
                         clickthrough_rate = sum(clicks, na.rm = T) /
                             sum(impressions, na.rm = T),
                         conversions = sum(conversions, na.rm = T),
                         cost_per_order = sum(cost, na.rm = T) /
                             sum(conversions, na.rm = T),
                         conversion_rate = sum(conversions, na.rm = T) /
                             sum(clicks, na.rm = T),
                         rev = sum(rev, na.rm = T),
                         average_order_value = sum(rev, na.rm = T) /
                             sum(conversions, na.rm = T),
                         profit = sum(rev, na.rm = T) -
                             sum(cost, na.rm = T),
                         return_on_ad_spend = sum(rev, na.rm = T) /
                             sum(cost, na.rm = T),
                         tot_rank = sum(tot_rank,na.rm = T)) %>%
        dplyr::mutate(avg_pos = tot_rank / impressions) %>%
        dplyr::select(-tot_rank) %>%
        as.data.frame
}
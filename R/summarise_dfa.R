#' Calculate common display aggregates on a DoubleClick data frame
#'
#' @param df data frame with DoubleClick report data
#'
#' @return summarised tbl_df
#' @importFrom magrittr "%>%"
#' @export
#'

summarise_dfa <- function(df) {
    df %>%
        dplyr::summarise(
            #start_date = min(calendardate),
            cost = sum(media_cost,na.rm = T) + sum(ad_serving_cost,
                                                   na.rm = T) +
                sum(click_command_fee, na.rm = T),
            impressions = sum(impressions,
                              na.rm = T),
            clicks = sum(clicks,
                         na.rm = T),
            orders_total = sum(sales_confirmation_page_transaction_count,
                               na.rm = T),
            orders_clickthrough = sum(
                sales_confirmation_page_click_through_transaction_count,
                na.rm = T
            ),
            orders_viewthrough = sum(
                sales_confirmation_page_view_through_transaction_count,
                na.rm = T
            ),
            revenue_total = sum(sales_confirmation_page_total_revenue,
                                na.rm = T),
            revenue_clickthrough = sum(
                sales_confirmation_page_click_through_revenue,
                na.rm = T),
            revenue_viewthrough = sum(
                sales_confirmation_page_view_through_revenue,
                na.rm = T)
        ) %>%
        dplyr::mutate(
            ctr = clicks / impressions,
            cvr = orders_clickthrough / clicks,
            orders_total = orders_clickthrough + orders_viewthrough,
            revenue_total = revenue_clickthrough + revenue_viewthrough,
            roas_total = (revenue_clickthrough + revenue_viewthrough) / cost,
            roas_viewthrough = revenue_viewthrough / cost,
            roas_clickthrough = revenue_clickthrough / cost
        ) %>%
        as.data.frame
}
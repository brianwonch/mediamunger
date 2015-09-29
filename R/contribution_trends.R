#' Plot contribution trends
#'
#' @param df input data frame
#' @param time_col string giving colname with time values
#' @param campaign_col string giving campaign colname
#' @param grouping string giving field to group_by
#' @param campaign_string string to parse for identifying campaign
#' @param agg_fun aggregation function, typically a dplyr::summarise() function defined elsewhere
#' @param kpis vector of strings giving KPI field names
#' @param final_week_start date vector of length 1
#' @param nweeks number of weeks to include
#' @import ggplot2
#' @importFrom magrittr "%>%"
#' @return plot
#' @export
contribution_trends = function(
    df = display_revised,
    time_col = "week",
    campaign_col = "campaign",
    grouping = "site_dcm",
    campaign_string = "continuity",
    agg_fun = summarise_dfa,
    kpis = c("cost", "clicks", "revenue_clickthrough", "revenue_viewthrough"),
    final_week_start = report_dates$final_week_start,
    nweeks = 7)
{ df %>%
    dplyr::filter_(lazyeval::interp(~time_col >= as.Date(final_week_start - 7*(nweeks-1)),
                             time_col = as.name(time_col))) %>%
    dplyr::filter_(lazyeval::interp(~grepl(campaign_string, campaign_col, ignore.case = T),
                             campaign_col = as.name(campaign_col))) %>%
    dplyr::group_by_(.dots = c(time_col, grouping)) %>%
    agg_fun %>%
    dplyr::select_(.dots = c(time_col, grouping, kpis)) %>%
    tidyr::gather_(key_col = "metric",
                   value_col = "value",
                   gather_cols = setdiff(names(.), c(time_col, grouping))
                   ) %>%
    ggplot(aes_string(x = time_col,
                      y = "value")) +
    geom_bar(stat = "identity", fill = "#5F76B0") +
    geom_bar(stat = "identity", position = "dodge", aes_string(fill = grouping)) +
    facet_wrap(~metric, scales="free_y") +
    theme_icrossing() +
    theme(legend.text = element_text(size=rel(0.8)),
              axis.text.x = element_text(size=rel(0.8)),
              legend.title = element_text(size=rel(0.8)),
          axis.title.x = element_blank(),
          axis.title.y= element_blank()) +
    scale_fill_manual(values = c("#23418E", "#FF8E00", "#C3B8E2", "#FFBC00", "#25AB36", "#532E00",
                                 "#DCA430", "#533D00", "#0B9CFA")) +
    scale_y_continuous(labels = comma)

}


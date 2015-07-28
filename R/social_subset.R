#' Filter/arrange/aggregate/sort Social Data
#'
#' @param df A raw data frame.
#' @param post_type String indicating post grouping.  Must match a name in the field_config and pattern_config lists.
#' @param date_range String indicating date range from which to select cases.  Must match a name in the supplied date_config list.
#' @param date_field A string giving the field name that identifies cases by date.
#' @param return_type A string to specify "table" or "chart" for use in KPI field selection with field_config_list.
#' @param date_config_list A list that names several seq.Date's, e.g. last_month
#' @param field_config_list A list that specifies KPI field names for each post_type, for use in charting, sort order, and table display.
#' @param agg_fun A HARDCODED dplyr::summarise() function providing all aggregates for a particular data set.
#' @param group_vec A string vector providing categorical field names by which to group the final data set.
#'
#' @return A subsetted and filtered data frame
#' @export
#' @importFrom lazyeval interp
#' @importFrom dplyr summarise select filter group_by rbind_list
#' @importFrom magrittr "%>%" "%<>%"
#' @examples social_subset(fb_df, post_type = "clicks", date_range = "fytd", date_field = "start_date", return_type = "table", parsing_config_list = parsing_configs, date_config_list = date_configs, field_config_list = field_configs, agg_fun = fb_summarise, group_vec = "ad_set_name")
social_subset = function(
    df,
    post_type = c("engagement",
                  "offer",
                  "clicks",
                  "road_south",
                  "biltmore",
                  "fanning"),
    date_range = c("current_month_to_date",
                   "last_month",
                   "last_two_months",
                   "fytd",
                   "fytd_last_month",
                   "trended_months"),
    date_field,
    return_type = c("table", "chart"),
    parsing_config_list,
    date_config_list,
    field_config_list,
    agg_fun,
    group_vec = NULL
) {
    # Match patterns corresponding to post_type in parsing_config_list
    pattern_configs = parsing_config_list[[post_type]]

    filtered = df %>%
        filter_by_pattern("campaign_name",
                          pattern_configs[["campaign_name_pattern"]]
        ) %>%
        rbind_list(filter_by_pattern(df, "ad_set_name",
                                     pattern_configs[["ad_set_name_pattern"]])
        )


    # if the resulting data should also NOT match specified patterns, filter out those bad cases
    if (grepl("exclusion", names(pattern_configs))) {
        filtered = filtered %>%
            filter_by_pattern("campaign_name",
                              pattern_configs[["campaign_name_exclusion_pattern"]],
                              exclude = TRUE) %>%
            filter_by_pattern("campaign_name",
                              pattern_configs[["ad_set_name_exclusion_pattern"]],
                              exclude = TRUE)
    }


    dates = match.arg(date_range, names(date_config_list, several.ok = FALSE))
    # Match dates corresponding to date_range in date_config_list
    filtered = filtered %>%
        dplyr::filter_(lazyeval::interp(~ date_field %in% date_config_list[[dates]],
                                        date_field = as.name(date_field)))

    if(is.null(group_vec) || !all(group_vec %in% names(df))) {
        warning("group_vec does not match the data frame names")
    }

    post_type = match.arg(post_type, names(field_config_list), several.ok = FALSE)


    # Traverse the field_config_list for relevant columns to select and sort
    field_config = field_config_list[[post_type]]
    keep_fields = field_config[[match.arg(return_type, names(field_config), several.ok = FALSE)]]
    sort_fields = field_config[["sort_fields"]]

    filtered = filtered %>%
        group_by_(.dots = group_vec) %>%
        agg_fun %>%
        select_(.dots = c(group_vec, keep_fields)) %>%
        ## Convert to data frame to remove groupings when sorting.
        as.data.frame %>%
        arrange_(.dots = sort_fields)
    return(filtered)
}

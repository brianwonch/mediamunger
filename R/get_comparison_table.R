#' Calculate period-over-period report stacking KPIs
#'
#' @param df input data frame
#' @param campaign_name campaign name to report in the table
#' @param campaign_colname column name indicating "campaign"
#' @param group_colname string vector indicating columns to group by
#' @param agg_fun summarising function
#'
#' @return filtered data frame
#' @export
#' @importFrom magrittr "%>%"

get_comparison_table = function(df,
                            campaign_name,
                            campaign_colname,
                            group_colname = "week",
                            agg_fun) {

    if(!campaign_name == "overall"){
        # if the campaign_name is not set as "overall"
        # subset the df to just the rows matching a convention

        df = df[grepl(campaign_name,
                      df[[campaign_colname]],
                      ignore.case=TRUE),]
    }

        # summarise the remaining rows using a function
        # present the resulting df with metrics long
        # and group_col values wide
    df = df %>%
        dplyr::group_by_(.dots = group_colname)

    if(!is.null(agg_fun)){
        df <- df %>%
            agg_fun
    }
    df = df %>%
        tidyr::gather_(
            key_col = "metric",
            value_col = "value",
            gather_cols = setdiff(names(.), group_colname)
        ) %>%
        tidyr::spread_(key_col = group_colname,
                       value_col = "value") %>%
        plyr::rename(replace = c("metric" = campaign_name))
    return(df)
}
#' Merge floodlight and basic-report data frames
#'
#' @param front_end_df dataframe with "Basic" DoubleClick report data
#' @param floodlight_df dataframe with Basic Floodlight report data
#'
#' @return merged data frame
#' @export
join_dcm_frames <- function(front_end_df,floodlight_df){

    #  Floodlight report awkwardly stacks activities.
    #  steps to merge the files
    #  A. melt floodlight metrics into one column
    #     of metric "variable"s and another column of "value"s
    #  B. remove irrelevant metric-activity combinations
    #  C. cast activity-metric combinations wide,
    #     pairing each metric with each relevant Activity as columns
    #  D. since not all ads convert, use a full outer join
    #  E. replace floodlight NA's with 0
    melted_floodlight_df <- reshape2::melt(floodlight_df,
                                           id=c("week",
                                                "date",
                                                "activity",
                                                "campaign",
                                                "site_dcm",
                                                "placement",
                                                "creative",
                                                "creative_pixel_size",
                                                "campaign_id")
    )

    #  REMOVE EXTRANEOUS METRIC-ACTIVITY COMBINATIONS
    #
    #  keep all Sales Confirmation Page metrics,
    #  but when activity isn't "Sales Confirmation Page", only use the Conversions metrics.
    #  "Transaction" metrics are confusing for non-sales confirmation pages
    #  and "Revenue" metrics will be 0 for those.
    melted_floodlight_df_2 <- base::subset(melted_floodlight_df,
                                           activity == "Sales Confirmation Page" |     # keep all metrics for sales activity
                                               (activity != "Sales Confirmation Page"    # but for other activities
                                                & variable %in%                          # keep conversion metrics only
                                                    c("total_conversions","click-through_conversions","view-through_conversions")))

    rownames(melted_floodlight_df_2) <- NULL

    # CAST activity-metric combinations wide, so you have each metric for each activity as a column
    casted_floodlight_df <- reshape2::dcast(data = melted_floodlight_df_2,
                                            formula = week +
                                                date +
                                                campaign +
                                                campaign_id +
                                                site_dcm +
                                                placement +
                                                creative +
                                                creative_pixel_size ~

                                                activity +
                                                variable,
                                            fun.aggregate = sum)

    #  =================================================
    #  JOIN FLOODLIGHT METRICS WITH BASIC DCM METRICS
    #  =================================================
    outer_joined_df <- base::merge(front_end_df,
                                   casted_floodlight_df,
                                   all = TRUE)

    # replace NAs with 0 for numeric columns only
    for(i in 1:length(outer_joined_df)){

        if(is.numeric(outer_joined_df[[i]])){

            outer_joined_df[[i]][is.na(outer_joined_df[[i]])] <- 0

        }

    }
    return(outer_joined_df)
}
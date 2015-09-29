#' Conditional formatting for a time-comparison FlexTable
#'
#' @param flextable An object from ReporteRs::FlexTable()
#' @param data_df Data source for the flextable object
#' @param delta_col_nums vector giving indices of columns that show percentage comparison
#' @param cost_ratio_row_nums vector giving indices of rows that show cost_per_* metrics
#' @param small_delta criterion for highlighting minor relative increases or decreases in decimal form.  Default is 0.1
#' @param big_delta criterion for highlighting major relative increases or decreases in decimal form.  Default is 0.3
#'
#' @return flextable object with formatting applied
#' @export
#'
format_deltas = function(
    flextable,
    data_df,
    delta_col_nums,
    cost_ratio_row_nums,
    small_delta = 0.1,
    big_delta = 0.3
){
    # There might not be cost_ratio row numbers in the dataframe, which would give a NULL input
    # In that case set the number to 0, so no rows will be identified as cost ratios
    if(is.null(cost_ratio_row_nums)) cost_ratio_row_nums = 0
    # Conditional Formatting ---------------
    if (length(delta_col_nums) > 0) {
        # loop through every row in the data_df,
        # checking formatting conditions
        for (i in 1:nrow(data_df)) {
            # We only highlight the period changes
            for (j in delta_col_nums) {
                # Boolean conditions
                if (is.na(data_df[[j]][i]) | is.nan(data_df[[j]][i])) {
                    data_df[i,j] <- 0
                }

                negative = data_df[[j]][i] < 0

                # Does the metric represent a cost?  This should flip the formatting
                # condition because we generally want costs to decrease.
                row_is_cost_ratio <- i %in% c(cost_ratio_row_nums)

                # Is the change > 10%?  This condition is arbitrary.
                delta_is_small <- abs(data_df[i,j]) > small_delta

                # Is the change > 30%?  This condition is arbitrary.
                delta_is_big <- abs(data_df[i,j]) > big_delta

                # Formatting with ReporteRs::FlexTable.
                # Examples at https://davidgohel.github.io/ReporteRs/flextable_examples.html
                # Formatting for unfavorable changes:
                # Volume is down or cost_ratio is up.
                if ((negative &&
                     !row_is_cost_ratio) || (!negative && row_is_cost_ratio)) {
                    if (delta_is_small) {
                        flextable[i,j]  = ReporteRs::textProperties(color = "#990000",
                                                         font.size = 10)
                    }

                    if (delta_is_big) {
                        flextable[i,j] = ReporteRs::cellProperties(
                            background.color = "#F2BCAA",
                            padding = 2)
                    }

                    # Formatting for favorable changes:
                    # Volume is up or cost_ratio is down.
                } else if (delta_is_small) {
                        flextable[i,j]  = ReporteRs::textProperties(color = "#207504",
                                                         font.size = 10)
                } else if (delta_is_big) {
                        flextable[i,j] = ReporteRs::cellProperties(background.color = "#AFDBA0",
                                                        padding = 2)
                }
            }
        }
    }

    return(flextable)
}
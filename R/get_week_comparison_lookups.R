#' Filter a lookup table to relevant dates for WoW and YoY comparisons
#'
#' @param fiscal_calendar dataframe lookup table
#' @param final_week_num fiscal week number as character
#' @param this_fiscal_year fiscal year as character
#'
#' @return filtered data frame with date information
#' @export
#' @importFrom magrittr "%>%"

get_week_comparison_lookups = function(fiscal_calendar,
                                       final_week_num,
                                       this_fiscal_year) {
    # check that inputs can be coerced to numeric
    if(is.na(as.numeric(final_week_num))|
       is.na(as.numeric(this_fiscal_year))) {
        stop("final_week_num and this_fiscal_year should contain only numerals")
    }

    previous_week_num = as.character(as.numeric(final_week_num) - 1)
    previous_fiscal_year = as.character(as.numeric(this_fiscal_year) - 1)

    final_two_weeks <-
        fiscal_calendar %>%
        dplyr::filter(
            current_week_num %in% c(
                as.character(final_week_num),
                as.character(previous_week_num)),
            fiscal_year == as.character(this_fiscal_year)
        )

    final_week_last_year <-
        fiscal_calendar %>%
        dplyr::filter(current_week_num == final_week_num,
                      fiscal_year == previous_fiscal_year)

    return(dplyr::rbind_list(final_two_weeks, final_week_last_year))

}
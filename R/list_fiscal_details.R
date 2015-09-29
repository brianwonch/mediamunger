
#' Produce a list of common fiscal information, using a lookup table
#'
#' @param fiscal_calendar data frame with lookups.  An example is in \code{data(fiscal_calendar)}
#' @param week_num_lookup_colname column name for fiscal week number in fiscal_calendar
#' @param date_lookup_colname column name indicating date in fiscal_calendar
#' @param fiscal_year_lookup_colname column name indicating fiscal year in fiscal_calendar
#' @param current_date defaults to \code{\link[lubridate]{today}}.
#' @importFrom magrittr "%>%"
#'
#' @return list of common fiscal date info
#' @export
#'
#' @examples list_fiscal_details(fiscal_calendar)
list_fiscal_details = function(fiscal_calendar,
                               week_num_lookup_colname = "current_week_num",
                               date_lookup_colname = "current_date",
                               fiscal_year_lookup_colname = "fiscal_year",
                               current_date = NULL) {

    if (is.null(current_date)) current_date = lubridate::today()
    if (class(current_date) != "Date") {
        stop("current_date must be a date")
    }

    if (class(date_lookup_colname) != "character") {
        stop("date_lookup_colname must be a character string")
    }

    if (class(fiscal_year_lookup_colname) != "character") {
        stop("fiscal_year_lookup_colname must be a character string")
    }

    if (class(week_num_lookup_colname) != "character") {
        stop("week_num_lookup_colname must be a character string")
    }


    final_week_start = current_date - (lubridate::wday(current_date) - 1) - 7

    previous_week_start = final_week_start - 7

    last_year_final_week_start = final_week_start - 364

    final_week_details <-
        fiscal_calendar %>%
        dplyr::filter_(lazyeval::interp(
            ~ date_col == final_week_start,
            date_col = as.name(date_lookup_colname)
        ))



    final_week_num <-
        unique(final_week_details[[week_num_lookup_colname]])

    ### current_week_num is a character string so we coerce for math
    previous_week_num <-
        as.character(as.numeric(final_week_num) - 1)


    this_fiscal_year <-
        unique(final_week_details[[fiscal_year_lookup_colname]])

    ### fiscal_year is a character string so we coerce for math
    previous_fiscal_year <-
        as.character(as.numeric(this_fiscal_year) - 1)

    report_dates = list(
        final_week_start = final_week_start,
        final_week_num = final_week_num,
        previous_week_start = previous_week_start,
        previous_week_num = previous_week_num,
        this_fiscal_year = this_fiscal_year,
        previous_fiscal_year = previous_fiscal_year,
        last_year_final_week_start = last_year_final_week_start
    )
    return(report_dates)
}
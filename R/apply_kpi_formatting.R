#' Apply formatting to a KPI flextable, using functions from the scales package
#'
#' @param flextable an object from ReporteRs::FlexTable()
#' @param actuals_cols vector of indices for columns showing actual performance (not deltas)
#' @param delta_cols vector of indices for columns showing pct deltas (all percents)
#' @param comma_rows vector of row indices that should apply scales::comma()
#' @param percent_rows vector of row indices that should apply scales::percent()
#' @param dollar_rows vector of row indices that should apply scales::dollar()
#' @param cents_rows vector of row indices that should apply scales::dollar() with cents
#' @param decimal_rows vector of row indices that should have comma-delimited thousands and also 2 decimal places
#'
#' @return flextable
#' @export
#' @importFrom magrittr "%>%" "%<>%"

apply_kpi_formatting = function(flextable,
                                actuals_cols = NULL,
                                delta_cols = NULL,
                                comma_rows = NULL,
                                percent_rows = NULL,
                                dollar_rows = NULL,
                                cents_rows = NULL,
                                decimal_rows = NULL
                                ){
    if(!is.null(actuals_cols)){
        if(!is.null(comma_rows)){
            flextable[comma_rows, actuals_cols] %<>%
                apply(2, function(r)
                    scales::comma(round(as.numeric(r),0)))

        }

        if(!is.null(percent_rows)){
            flextable[percent_rows, actuals_cols] %<>%
                apply(2, function(r)
                    scales::percent(round(as.numeric(r),4)))

        }

        if(!is.null(dollar_rows)){
            flextable[dollar_rows, actuals_cols] %<>%
                apply(2, function(r)
                    scales::dollar(round(as.numeric(r),2)))

        }

        if(!is.null(cents_rows)){

            flextable[cents_rows, actuals_cols] %<>%
                apply(2, function(r)
                    paste0("$", round(as.numeric(r),2)))

        }

        if(!is.null(decimal_rows)){

            flextable[decimal_rows, actuals_cols] %<>%
                apply(2, function(r)
                    as.character(round(as.numeric(r),3)))

        }


    }

    if(!is.null(delta_cols)){

        flextable[, delta_cols] %<>%
            sapply(function(r)
                paste0(round(as.numeric(r * 100)),"%"))
    }

    return(flextable)
}
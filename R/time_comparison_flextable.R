#' Create a period comparison table and conditionally format as a FlexTable
#'
#' @param week_comparison_df performance dataframe filtered to relevant periods for time comparison (e.g. just last-week, previous-week, and last-week-last-year)
#' @param final_week string identifying the most recent period
#' @param previous_week string identifying the previous period
#' @param campaign_col string identifying the "campaign" column for filtering
#' @param last_year_final_week optional string identifying a third period (e.g. for year-over-year reporting)
#' @param agg_fun summarise function
#' @param campaign_to_filter string or regexp indicating campaign to filter on
#' @param group_colname period colname, typically "week"
#'
#' @return formatted flextable
#' @export
#' @importFrom magrittr "%>%" "%<>%"

time_comparison_flextable <- function(
    week_comparison_df,
    final_week,
    previous_week,
    campaign_to_filter,
    campaign_col = "campaign",
    group_colname = "week",
    last_year_final_week = NULL,
    agg_fun) {
    # Produce period-over-period performance summary
    comparison_table <- get_comparison_table(df = week_comparison_df,
                                            campaign_name = campaign_to_filter,
                                            campaign_colname = campaign_col,
                                            group_colname = group_colname,
                                            agg_fun = agg_fun)


    # Calculate week deltas ----------------------------------------------------

    final_week %<>% as.character
    previous_week %<>% as.character
    if(!is.null(last_year_final_week)) last_year_final_week %<>% as.character

    if (final_week %in% names(comparison_table) &
        previous_week %in% names(comparison_table)) {

        comparison_table[["week_delta"]] <- comparison_table[[final_week]] /
            comparison_table[[previous_week]] - 1

    }

    if (last_year_final_week %in% names(comparison_table)) {

        comparison_table[["year_delta"]] <- comparison_table[[final_week]] /
            comparison_table[[last_year_final_week]] - 1

    }


    # Make metric name and colname text pretty ---------------------------------

    comparison_table <- proper_col(comparison_table, 1) %>% proper_names




    # Find indices for formatting rows and column text in flextable ------------

    numeric_cols <- comparison_table %>%
        sapply(is.numeric) %>%
        unname %>%
        which
    delta_cols <- grep("delta|change", names(comparison_table), ignore.case = T)
    actuals_cols <- setdiff(numeric_cols, delta_cols)
    comma_rows <- get_metric_position_indices(comparison_table[[1]],
                                              "comma")



    percent_rows <- get_metric_position_indices(comparison_table[[1]],
                                                "percent")




    dollar_rows <- get_metric_position_indices(comparison_table[[1]],
                                               "dollar")




    cents_rows <- get_metric_position_indices(comparison_table[[1]],
                                              "cents")



    cost_ratio_rows <- get_metric_position_indices(comparison_table[[1]],
                                                   "cost_per")

    decimal_rows <- get_metric_position_indices(comparison_table[[1]],
                                                   "decimal")


    # Copy the comparison_table as ft ------------------------------------------
    # ft will become a FlexTable for printout

    ft <- comparison_table

    # Format the text of ft numbers using the scales package -------------------
    # Note that this will coerce all columns of ft to character
    ft %<>% apply_kpi_formatting(actuals_cols = actuals_cols,
                                 delta_cols = delta_cols,
                                 comma_rows = comma_rows,
                                 percent_rows = percent_rows,
                                 dollar_rows = dollar_rows,
                                 cents_rows = cents_rows,
                                 decimal_rows = decimal_rows)


    # Set up default properties for ReporteRs::FlexTable() ---------------------
    base_text_prop <- ReporteRs::textProperties(font.size = 10, color = "#3D3234")
    base_par_prop <- ReporteRs::parProperties(text.align = "right")
    base_cell_prop <- ReporteRs::cellProperties(padding = 2)


    # Convert ft to a flextable, using default properties ----------------------
    ft %<>% ReporteRs::FlexTable(
        header.cell.props = ReporteRs::chprop(base_cell_prop,
                                   background.color = "#23418E"),

        header.par.props = ReporteRs::chprop(base_par_prop,
                                  text.align = "center"),

        header.text.props = ReporteRs::chprop(base_text_prop,
                                   font.weight = "bold",
                                   color = "#FFFFFF"),

        body.cell.props = base_cell_prop,

        body.par.props = base_par_prop,

        body.text.props = base_text_prop
    )

    # align the first column text left -----------------------------------------
    ft[, 1] <- ReporteRs::chprop(base_par_prop, text.align = "left")

    # set the first column text bold -------------------------------------------
    ft[, 1] <- ReporteRs::chprop(base_text_prop, font.weight = "bold")

    # shade even-numbered rows light grey --------------------------------------

    ft[which(as.numeric(row.names(comparison_table)) %% 2 == 0),] <-
        ReporteRs::chprop(base_cell_prop, background.color = "#EEEEEE")
    ## equivalent:
    # ft %<>% setZebraStyle(even ="#FFFFFF", odd = "#EEEEEE")

    # apply red-green conditional formatting to standout deltas ----------------
    ft  %<>% format_deltas(data_df = comparison_table,
        delta_col_nums = delta_cols,
        cost_ratio_row_nums = cost_ratio_rows)


    return(ft)

}
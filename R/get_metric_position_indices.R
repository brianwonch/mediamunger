

#' Grep a vector of KPI names matching a convention for formatting
#'
#' @param metrics_vec vector of KPIs, likely colnames from a KPI data frame
#' @param type choose among c("percent", "dollar", "cost_per", "cents", "comma")
#'
#' @return vector
#' @export
#'
get_metric_position_indices = function(metrics_vec, type = "comma") {
    if (type == "percent") {
        idx = grep("rate|ctr|cvr",
                   metrics_vec,
                   ignore.case = T)
    }

    if (type == "dollar") {
        idx = grep("^(cost|spend|profit|media (cost|spend)|(revenue.*)|sales|rev\\.*)$",
                   metrics_vec,
                   ignore.case = T)

    }

    if (type == "cost_per") {
        # don't apply conditional formatting to pure cost or spend,
        # only "cost PER ____" metrics
        idx = grep("cost.+|cp|spend.+",
                   metrics_vec, ignore.case = T)

    }

    if (type == "cents") {
        idx = grep(
            "cpc|cpo|cpa|cost.*per|cpl|cpql|roas|aov|average.*order.*value|roi|roas|return.*on",
            metrics_vec,
            ignore.case = T
        )
    }

    if (type == "decimal") {
        idx = grep(
            "(avg.*pos)|(average.*position)",
            metrics_vec,
            ignore.case = T
        )
    }

    # comma is the catch-all that does not match other conventions
    if (type == "comma") {
        idx = which(!grepl("rate|ctr|cvr|cost|spend|rev|cpc|cpo|cpa|average.*order.*value|profit|cpl|cpql|roi|roas|aov|roas|return",
                    metrics_vec,
                    ignore.case = T))
    }

    # if there is no match for the requested type, set the resulting index to NULL
    if (length(idx) == 0) idx <- NULL

    return(idx)
}


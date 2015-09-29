#' Make a "metric" column proper
#'
#' @param colname String giving the name of the column whose values should be made characters in proper case.
#' @param levels_vec an optional vector of strings giving levels of a factor.
#' @param df a dataframe with a column named "metric"
#'
#' @return data frame with "metric" column in proper case
#' @export
#'
#' @examples proper_col(CO2, "Treatment")
proper_col <- function(df, colname, levels_vec = NULL){
    proper <- function(x){
        paste0(toupper(substr(x, 1, 1)), tolower(substring(x, 2)))
    }

    simpleCap <- function(x) {
        s <- strsplit(x, " ")[[1]]
        paste(toupper(substring(s, 1,1)), substring(s, 2),
              sep="", collapse=" ")
    }

    df[[colname]] <- proper(gsub("_|\\."," ", df[[colname]]))
    df[[colname]] <- gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2",
                          df[[colname]],
                          perl=TRUE)

    if (!is.null(levels_vec)) {
        df[[colname]] <- factor(df[[colname]], levels = levels_vec)
    }
    return(df)
}
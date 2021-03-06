% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/get_comparison_table.R
\name{get_comparison_table}
\alias{get_comparison_table}
\title{Calculate period-over-period report stacking KPIs}
\usage{
get_comparison_table(df, campaign_name, campaign_colname,
  group_colname = "week", agg_fun)
}
\arguments{
\item{df}{input data frame}

\item{campaign_name}{campaign name to report in the table}

\item{campaign_colname}{column name indicating "campaign"}

\item{group_colname}{string vector indicating columns to group by}

\item{agg_fun}{summarising function}
}
\value{
filtered data frame
}
\description{
Calculate period-over-period report stacking KPIs
}


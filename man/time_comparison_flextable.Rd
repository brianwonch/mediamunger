% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/time_comparison_flextable.R
\name{time_comparison_flextable}
\alias{time_comparison_flextable}
\title{Create a period comparison table and conditionally format as a FlexTable}
\usage{
time_comparison_flextable(week_comparison_df, final_week, previous_week,
  campaign_to_filter, campaign_col = "campaign", group_colname = "week",
  last_year_final_week = NULL, agg_fun)
}
\arguments{
\item{week_comparison_df}{performance dataframe filtered to relevant periods for time comparison (e.g. just last-week, previous-week, and last-week-last-year)}

\item{final_week}{string identifying the most recent period}

\item{previous_week}{string identifying the previous period}

\item{campaign_to_filter}{string or regexp indicating campaign to filter on}

\item{campaign_col}{string identifying the "campaign" column for filtering}

\item{group_colname}{period colname, typically "week"}

\item{last_year_final_week}{optional string identifying a third period (e.g. for year-over-year reporting)}

\item{agg_fun}{summarise function}
}
\value{
formatted flextable
}
\description{
Create a period comparison table and conditionally format as a FlexTable
}


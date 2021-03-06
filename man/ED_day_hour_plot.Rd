% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ED_day_hour.R
\name{ED_day_hour_plot}
\alias{ED_day_hour_plot}
\title{ED_day_hour_plot}
\usage{
ED_day_hour_plot(
  startDay,
  endDay,
  df,
  hospital_name = "Hospital Name",
  plot_chart = T
)
}
\arguments{
\item{startDay}{Date for which the analysis starts}

\item{endDay}{Date for which analysis ends}

\item{df}{Dataframe representing ED stays with start_ime and end_time specifying stay start and end of time in ED}

\item{hospital_name}{Name of hospital for which the analysis is being done}

\item{plot_chat}{Plots chart if set to TRUE, else returns dataframe of plot data}
}
\value{
A plot or dataframe with mean, Q1, Q3, max and min occupancy for each hour of the week
}
\description{
ED_day_hour_plot
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ED_day_hour.R
\name{ED_day_hour}
\alias{ED_day_hour}
\title{ED_day_hour}
\usage{
ED_day_hour(startDay, endDay, df)
}
\arguments{
\item{startDay}{Date for which the analysis starts}

\item{endDay}{Date for which analysis ends}

\item{df}{Dataframe representing ED stays with start_ime and end_time specifying stay start and end of time in ED}
}
\value{
A dataframe with mean, Q1, Q3, max and min occupancy for each hour of the week
}
\description{
ED_day_hour
}

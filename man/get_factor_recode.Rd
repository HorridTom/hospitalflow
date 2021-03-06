% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generic_standardise.R
\name{get_factor_recode}
\alias{get_factor_recode}
\title{get_factor_recode}
\usage{
get_factor_recode(config_path)
}
\arguments{
\item{config_path}{path to the directory containing the config files for this dataset}
}
\value{
a named vector of expressions. There is one element (expression) per factor specified in the config files.
The expressions are calls to readr::fct_recode corresponding to the level recode mapping for specified in the
config files. Their names are the standard column names of the factor.
Thus when unquote-spliced into mutate, the returned vector generates a valid set of named arguments
for a mutate call that will recode the factor variables in the way specified.
}
\description{
get_factor_recode
}

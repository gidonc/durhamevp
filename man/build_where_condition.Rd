% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/database_interactivity.R
\name{build_where_condition}
\alias{build_where_condition}
\title{Constructs where conditions for querying database.}
\usage{
build_where_condition(
  col_name,
  vals,
  existing_condition,
  existing_interpolate_list
)
}
\arguments{
\item{col_name}{The column name for there where condition.}

\item{vals}{The value or values to include in the query.}

\item{existing_condition}{An existing SQL query to append this where condition to.}

\item{existing_interpolate_list}{Existing interpolation list to add this interpolation list to.}
}
\description{
Constructs where conditions for querying database.
}

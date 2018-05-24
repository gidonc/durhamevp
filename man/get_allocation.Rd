% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/database_interactivity.R
\name{get_allocation}
\alias{get_allocation}
\title{Get the articles currently allocated to the user.}
\usage{
get_allocation(con = connect, user_id, allocation_type = "\%")
}
\arguments{
\item{con}{The database connection to .}

\item{user_id}{The userid to check in the database.}
}
\value{
dataframe of the document allocations to the user.
}
\description{
Get the articles currently allocated to the user.
}
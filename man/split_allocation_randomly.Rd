% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/article_assignment.R
\name{split_allocation_randomly}
\alias{split_allocation_randomly}
\title{Divide a set of N*P articles amongst P users, giving N articles each user for coding.}
\usage{
split_allocation_randomly(
  user_ids,
  set,
  allocation_type = "coding",
  allocated_by = "split_random_assignment",
  restrict_to_actual = TRUE,
  make_assignments = FALSE
)
}
\arguments{
\item{user_ids}{The users to allocate the articles amongst (vector with single or multiple user_ids).}

\item{set}{The set of documents which are to be allocated (vector with single or multiple of document_ids).}

\item{allocation_type}{The value to write to the allocation_type field in the database document_allocations table (training, testing, coding, checking, ideal).}

\item{allocated_by}{The value to write to the allocated_by field in the database document_allocations table.}

\item{restrict_to_actual}{Should the restriction to actual users and documents be enforced. Should only be set to FALSE for debugging purposes.}

\item{make_assignments}{Actually make changes to the database (TRUE) or only create a proposed set of changes as a dataframe (FALSE)}
}
\description{
\code{allocate_number_randomly} divides a set of articles amongst a set of users all users get the same number of articles to code, with the articles being divided randomly amongst users. Each article is coded exactly once, and all users receive the same number of articles for coding (the function checks that the number of articles is a multiple of the number of users).
By default the articles and user ids are checked against the database and codes which do not correspond to existing documents and/or users are ignored.
}

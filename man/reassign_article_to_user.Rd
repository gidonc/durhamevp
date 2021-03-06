% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/article_assignment.R
\name{reassign_article_to_user}
\alias{reassign_article_to_user}
\title{Reassign article allocation to a new user.}
\usage{
reassign_article_to_user(
  user_doc_id,
  user_id,
  allocation_type,
  allocated_by,
  allocation_date = as.character(Sys.Date()),
  status = "NEW",
  coding_complete = 0
)
}
\arguments{
\item{user_doc_id}{Id of the document to be assigned or a vector of document ids.}

\item{user_id}{Id of the user the document is to be assigned to or a vector of user ids.}

\item{allocation_type}{Type of allocation (training, testing, coding, checking, ideal).}

\item{allocation_date}{Date allocation made (usually today).}

\item{status}{Status of document coding (generally 'NEW' for newly reassigned documents).}
}
\description{
\code{reassign_article_to_user} reassigns and otherwise modifies a specific existing document allocation to a specific user, and otherwise modifies the allocation parmeters (updating allocation_type, allocated_by, allocation_date, status and coding_complete fields.
}

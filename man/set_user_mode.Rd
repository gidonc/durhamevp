% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/database_interactivity.R
\name{set_user_mode}
\alias{set_user_mode}
\title{Sets user mode}
\usage{
set_user_mode(user_id, new_mode)
}
\arguments{
\item{user_id}{Id of the user whose mode to set or a vector of user ids.}

\item{new_mode}{The new mode (one of: training, testing, coding, checking, ideal) to set as the users mode(s). For multiple users either a single value should be given to be assigned to all identified users, or a vector of values should be given of the same length as the vector of user_ids.}
}
\description{
Sets the users mode to a new value (training, testing, coding, checking, ideal).
}

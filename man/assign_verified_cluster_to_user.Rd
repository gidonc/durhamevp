% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clustering.R
\name{assign_verified_cluster_to_user}
\alias{assign_verified_cluster_to_user}
\title{Assign verified (and combined) clusters to user for reallocation to clusters.}
\usage{
assign_verified_cluster_to_user(
  verified_cluster_id,
  user_id,
  allocation_date = as.character(Sys.Date()),
  last_updated = as.character(Sys.time()),
  completed = 0
)
}
\arguments{
\item{verified_cluster_id}{verified_cluster_id of the verified cluster to be assigned.}

\item{user_id}{Id of the user the the cluster is to be assigned to.}

\item{allocation_date}{Date allocation made (usually today).}

\item{last_updated}{Date record last updated (usually today).}

\item{completed}{Is the cluster verification complete (usually 0 \link{i.e. not complete})}
}
\description{
\code{assign_verfied_cluster_to_user} assigns a verfied cluster to a user for combination.
}

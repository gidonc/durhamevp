% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/database_interactivity.R
\name{add_event_id_from_clustering}
\alias{add_event_id_from_clustering}
\title{Adds the event_id which uniquely identifies events (grouping together different event reports) from the clustering table.}
\usage{
add_event_id_from_clustering(
  evp_coding_download,
  event_id_from_clusterattempts = c(401:420)
)
}
\arguments{
\item{evp_coding_download}{The result of a \code{get_coding()}.}

\item{event_id_from_clusterattempts}{Which cluster attempt ids should be used to generate the event_id? Default is 401 to 420 (the second clustering)}
}
\description{
Adds the event_id which uniquely identifies events (grouping together different event reports) from the clustering table.
}

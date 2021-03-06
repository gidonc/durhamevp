% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/report_scoring_utility.R
\name{get_data_for_comparison}
\alias{get_data_for_comparison}
\title{Matches elements from two document allocations (presumably different codings of the same document).
Returns a list of matched event reports, tags and attributes.}
\usage{
get_data_for_comparison(model_user_doc_id, user_user_doc_id)
}
\arguments{
\item{model_user_doc_id}{the user_doc_id of the first set of codings to compare}

\item{user_user_doc_id}{the user_doc_id of the second set of codings to compare}
}
\value{
A list of length 3 containing 1. dataframe of matched event report ids 2. dataframe of matched tag ids 3. dataframe of matched attributes.
}
\description{
The similarity is measured using the Levenshtein distance between all of their fields of the serialized record.
For every pair of events (and consequently, for any tags and attributes associated with these events), we use the \code{get_sorted} function to find the best match.
}

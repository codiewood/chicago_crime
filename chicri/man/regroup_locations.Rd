% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/multiclass_classification.R
\name{regroup_locations}
\alias{regroup_locations}
\title{Relevels the `Location Description` factor variable by grouping categories and otherising}
\usage{
regroup_locations(data, threshold)
}
\arguments{
\item{data}{Data frame to be used}

\item{threshold}{Threshold count below which category will be converted to other.}
}
\value{
Data frame with modified factor
}
\description{
Relevels the `Location Description` factor variable by grouping categories and otherising
}

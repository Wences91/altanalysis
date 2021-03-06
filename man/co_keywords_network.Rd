% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/co_keywords_network.R
\name{co_keywords_network}
\alias{co_keywords_network}
\title{co_keywords_network}
\usage{
co_keywords_network(keywords, keywords_mentions, binary = TRUE,
  min_co = 1)
}
\arguments{
\item{keywords}{Keywords data.frame from altmetric_keywords}

\item{keywords_mentions}{Altmetric.com mentions with keywords from altmetric_keywords}

\item{binary}{Boolean that indactes if the occurrence count carried is binary or not}

\item{min_co}{To filter the network on the basis of a minimum of co-occurrences}
}
\description{
This function gets the keywords and keywords mentions from altmetric_keywords and obtains a co-occurrence network of keywords. It returns a list wit two data.frame that correspond to the nodes and edges of the network.
}

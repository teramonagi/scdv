% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scdv.R
\name{gmm}
\alias{gmm}
\title{Gaussian Mixture Model clustering}
\usage{
gmm(wv, k, args = list())
}
\arguments{
\item{wv}{Word vectors(word embedding expression)}

\item{k}{The number of clusters}

\item{args}{Parameters for GMM model ( parameters of ClusterR::center_scale and ClusterR::GMM )}
}
\description{
Gaussian Mixture Model clustering
}

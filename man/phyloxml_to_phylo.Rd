% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/phyloxml_to_phylo.R
\name{phyloxml_to_phylo}
\alias{phyloxml_to_phylo}
\alias{phyloxml2phylo}
\title{Coerces a \code{phyloxml} to \code{multiphylo}}
\usage{
phyloxml_to_phylo(x, labvar = "name")

phyloxml2phylo(x, labvar = "name")
}
\arguments{
\item{x}{An object of class \link{phyloxml}}

\item{labvar}{Character scalar. The name of the variable to be used to
label the nodes and tips of the tree. By default is \code{name}.}
}
\value{
An object of class \link[ape:multiphylo]{ape::multiphylo}.
}
\description{
This function takes the \code{phyloxml} object, which is essentially a list, and
returns a object of class \link[ape:multiphylo]{ape::multiphylo}
}

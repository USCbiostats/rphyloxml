# rm(list = ls())

library(xml2)

rooted      <- "true"
name        <- "Alcohol dehydrogenases"
description <- "contains examples of commonly used elements"

#' There must be a way of doing this using the post-order traversal
#' so it is easier to write down the "recursion".
#'
#' It would be nice to do so by matching stuff
#' @import xml2
#' @import ape
new_clade <- function(x) {
  clade <- xml2::xml_new_root("clade")
  if (is.list(x))
    for (i in 1:length(x))
      xml2::xml_add_child(clade, new_clade(x[[i]]))

  return(clade)
}

# new_clade(list(list(1, 2)))
#
# doc  <- xml2::xml_new_root("phylogeny", rooted = rooted)
# xml2::xml_add_child(doc, "name", name)
# xml2::xml_add_child(doc, "description", description)
# doc

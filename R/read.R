#' phyloXML XML Schema
#' @export
phyloXML_xsd <- function() {
  xml2::read_xml(system.file("phyloxml/phyloxml.xsd", package="rphyloxml"))
}

#' PhyloXML namespace
#' An object of class `xml_namespace`
#' @export
phyloXML_ns  <- xml2::xml_ns(phyloXML_xsd())

fillstill <- function(x) {
  if (is.list(x)) {
    lapply(x, fillstill)
  } else {
    ifelse(length(x), unname(x), NA)
  }
}

#' Using the XML Schema, parses the clade elements other than clades.
#' @noRd
clade_nodes <- function(x) {

  # Listing nodes (attributes)
  nodes <- setdiff(names(x), "clade")

  if (!length(nodes))
    return(NULL)

  ans        <- vector("list", length(nodes))
  names(ans) <- nodes
  for (n in nodes) {

    # Finding definition
    def  <- xml2::xml_find_all(phyloXML_xsd(), sprintf("//xs:element[@name='%s']", n))

    # If it was not successful
    if (!length(def))
      stop("The attribute `", n, "` is not part of the phyloXML schema.", call.=FALSE)

    # We are not processing this yet
    type <- attr(xml2::as_list(def)[[1]], "type")

    ans[[n]] <- x[[n]][[1]]
  }

  do.call(cbind, ans)

}

supportedAnnotations <- c(
  "name",
  "branch_length",
  "width",
  "color",
  "events",
  "date"
  )

empty_ann <- structure(
  rep(list(list(NA)), length(supportedAnnotations)),
  class     = "data.frame",
  names     = supportedAnnotations,
  row.names = 1L
  )


#' A recursive function that goes through the list created by xml2::as_list
#' recursively to retrieve information about the nodes.
#'
#' @param x A list. Could be either the entire tree (how it starts), or a subtree.
#' @param id An environment with `id`. This is to keep track of the nodes ids.
#' @param parentif Integer scalar. Id of the parent when calling recursively
#'
#' @noRd
#'
clade_to_dat <- function(x, id = list2env(list(id=0L)), parentid=NULL) {

  # Increasing the id. Since it is an environment, then this will be changed
  # no matter the place it is called, so parallel clades will share the same
  # increment.
  id[["id"]] <- id[["id"]] + 1L

  # Fetching attributes
  # y <- clade_nodes(x)
  # y <- dplyr::bind_cols(list(data.frame(id=id[["id"]], isleaf=FALSE, parent = id[["id"]] - 1L), y))
  y <- data.frame(
    id            = id[["id"]],
    isleaf        = FALSE,
    parent        = fillstill(parentid),
    branch_length = as.double(fillstill(x[["branch_length"]])[[1]])
  )

  # Storing clade information
  id[["node.meta"]][[id[["id"]]]] <- x[setdiff(names(x), "clade")]

  # Are there any clades?
  clades <- which(names(x) == "clade")
  if (length(clades)) {
    clades <- do.call(rbind, lapply(x[clades], clade_to_dat, id = id, parentid = y$id))
  } else {
    y[["isleaf"]] <- TRUE
  }

  return(rbind(y, clades))

}

recode_tree <- function(x) {
  n     <- nrow(x)
  nleaf <- sum(x$isleaf)
  ans   <- vector("integer", n)

  ans[which(x$isleaf)]  <- 1L:nleaf
  ans[which(!x$isleaf)] <- (nleaf + 1L):n

  ans
}







#
# set.seed(1)
# x <- ape::rtree(10)
# f <- tempfile()
# xml2::write_xml(write_phyloxml(x), file=f)
# dat <- read_phyloxml(f)

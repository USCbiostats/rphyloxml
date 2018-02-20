#' phyloXML XML Schema
#' @export
phyloXML_xsd <- function() {
  xml2::read_xml(system.file("phyloxml/phyloxml.xsd", package="rphyloxml"))
}

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

    # If it was not successfull
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
  class="data.frame",
  names = supportedAnnotations,row.names=1L
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
    branch_length = fillstill(x[["branch_length"]])[[1]]
  )

  # Storing clade information
  id[["node.meta"]][[id[["id"]]]] <- x[setdiff(names(x), "clade")]

  # for (a in supportedAnnotations)
  #   empty_ann[[a]][1] <- list(fillstill(x[[a]]))

  # y <- cbind(y, empty_ann)

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


#' Coerces a `phyloxml` to `multiphylo`
#'
#' This function takes the `phyloxml` object, which is essentially a list, and
#' returns a object of class [ape::multiphylo]
#'
#' @param x An object of class [phyloxml]
#' @param labvar Character scalar. The name of the variable to be used to
#' label the nodes and tips of the tree. By default is `name`.
#' @return An object of class [ape::multiphylo].
#' @export
phyloxml2phylo <- function(x, labvar = "name") {

  ntrees <- length(x)
  ans    <- structure(vector("list", ntrees), class="multiPhylo")

  for (p in 1L:ntrees) {

    N    <- length(unique(x[[p]][[".Data"]][["id"]]))
    leaf <- x[[p]][[".Data"]][["isleaf"]]
    node <- which(!leaf)
    leaf <- which(leaf)

    ans[[p]] <- structure(
      with(
        x[[p]][[".Data"]],
        list(
          edge        = unname(cbind(parent, id))[-1L,],
          tip.label   = x[[p]][[".Data"]][[labvar]][leaf],
          edge.length = branch_length[-1L],
          Nnode       = length(node),
          node.label  = x[[p]][[".Data"]][[labvar]][node]
        )
      ),
      class = "phylo"
    )
  }

  return(ans)

}

#' @rdname phyloxml2phylo
#' @export
phyloxml_to_phylo <- phyloxml2phylo


#' Read phyloXML files
#' @param x Either a character scalar (path to a phyloXML file) or an `xml` object.
#' @export
#'
#' @details
#' `phyloxml` objects can be coerced as [ape::multiphylo] using the
#' [phyloxml2phylo] function.
#'
#' @examples
#' library(ape)
#' set.seed(1)
#'
#' x <- rtree(10)
#' x2 <- write_phyloxml(x)
#' read_phyloxml(x2)
#' @return A list of class `phyloxml` in which each element represents a
#' tree:
#' A `data.frame` with the following elements:
#'
#' \item{id}{Integer, the id of the node.}
#' \item{name}{Character, the name of the node (can be `NA`).}
#' \item{branch_length}{Numeric, the length of the branch (can be `NA`).}
#' \item{iselaf}{Logical, whether it is leaf (tip) or not.}
#' \item{parent}{Integer, the id of the parent node.}
#'
#' @aliases phyloxml phyloxml-class
read_phyloxml <- function(x) UseMethod("read_phyloxml")

#' @export
#' @rdname read_phyloxml
read_phyloxml.character <- function(x) {
  # Reading XML, and validating
  doc <- xml2::read_xml(x)

  read_phyloxml(doc)
}

#' @export
#' @rdname read_phyloxml
read_phyloxml.default <- function(x) {

  # Coercing into a list
  dat <- xml2::as_list(x)

  # Procesing the data
  trees_pos <- which(names(dat) == "phylogeny")
  ans       <- vector("list", length(trees_pos))

  # Storage environment
  env <- list2env(list(id=0L, node.meta=list()))

  for (i in seq_along(trees_pos)) {
    # Retrieving the topology and data
    ans[[i]]            <- clade_to_dat(dat[[trees_pos[i]]][["clade"]], id = env)

    newid               <- recode_tree(ans[[i]])
    ans[[i]]$id         <- newid
    ans[[i]]$parent[-1] <- newid[ans[[i]]$parent[-1]]

    rownames(ans[[i]]) <- ans[[i]]$id

    # Getting attributes
    attrs <- attributes(dat[[trees_pos[[i]]]])
    ans[[i]] <- c(
      list(.Data = ans[[i]]),
      list(node.meta = env[["node.meta"]]),
      attrs[setdiff(names(attrs), "names")]
      )
  }

  # Checking names
  tnames <- vector("character", length(ans))
  for (i in seq_along(trees_pos)) {
    tname <- dat[[trees_pos]][["name"]]
    if (length(tname))
      tnames[i] <- tname
    else
      tnames[i] <- sprintf("unnamed_tree%03i", i)
  }

  # Returning
  structure(
    ans,
    names = tnames,
    class = "phyloxml"
  )
}

#
# set.seed(1)
# x <- ape::rtree(10)
# f <- tempfile()
# xml2::write_xml(write_phyloxml(x), file=f)
# dat <- read_phyloxml(f)

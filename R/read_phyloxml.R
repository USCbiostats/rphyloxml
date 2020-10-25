#' Read phyloXML files
#' @param x Either a character scalar (path to a phyloXML file) or an `xml` object.
#' @param ... Further arguments passed to [xml2::read_xml]
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
read_phyloxml <- function(x, ...) UseMethod("read_phyloxml")

#' @export
#' @rdname read_phyloxml
read_phyloxml.character <- function(x, ...) {
  # Reading XML, and validating
  doc <- xml2::read_xml(x, ...)

  read_phyloxml(doc)
}

#' @export
#' @rdname read_phyloxml
read_phyloxml.default <- function(x, ...) {

  # Extracting only the phylogenies
  dat <- xml2::xml_find_all(x, xpath = "//phy:phylogeny", ns = phyloXML_ns)

  # Coercing into a list
  dat <- xml2::as_list(dat)

  # Procesing the data
  ans <- vector("list", length(dat))

  # Storage environment
  env <- list2env(list(id=0L, node.meta=list()))

  for (i in seq_along(dat)) {
    # Retrieving the topology and data
    ans[[i]]            <- clade_to_dat(dat[[i]][["clade"]], id = env)

    newid               <- recode_tree(ans[[i]])
    ans[[i]]$id         <- newid
    ans[[i]]$parent[-1] <- newid[ans[[i]]$parent[-1]]

    rownames(ans[[i]]) <- ans[[i]]$id

    # Getting attributes
    attrs <- attributes(dat[[i]])
    ans[[i]] <- c(
      list(edges = ans[[i]]),
      list(node.meta = env[["node.meta"]]),
      attrs[setdiff(names(attrs), "names")]
    )
  }

  # Checking names
  tnames <- vector("character", length(ans))
  for (i in seq_along(dat)) {
    tname <- dat[[i]][["name"]]
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

#' @export
print.phyloxml <- function(x, ...) {
  cat("A list with ", length(x), " phylogenetic trees.\n")
  invisible(x)
}

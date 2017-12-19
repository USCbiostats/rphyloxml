
#' @import xml2
#' @import ape
NULL

#' Lists the offspring of each node in a edgelist
#' @noRd
list_offspring <- function(edgelist) {

  n <- max(edgelist)
  ans <- vector("list", n)

  for (i in 1L:n)
    ans[[i]] <- edgelist[edgelist[,1] == i, 2]

  ans

}

#' Prepares a peeling sequence to write down the tree
#'
#' @noRd
getseq <- function(tree) {

  ord <- ape::postorder(tree)
  off <- list_offspring(tree$edge)
  lab <- c(tree$tip.label, if (length(tree$node.label)) {
    tree$node.label
  } else {
    rep(NA, ape::Nnode(tree))
  })

  list(
    ord       = ord,
    offspring = off,
    labels    = lab
  )

}

#' Coerce into phyloXML
#'
#' Coerce phylogenetic trees to phyloXML documents, objects of class `xml_document`.
#'
#' @param tree A phylogenetic tree.
#' @param ... Further arguments to be passed to the method (see details).
#' @param name Character scalar. Name of the tree.
#' @param description Character scalar. Description of the tree.
#'
#' @details
#' So far, this function has a method for [phylo][ape::as.phylo] objects
#' from the `ape` package. The following elements are supported:
#'
#' - Branch lengths as stored in `edge.length`
#' - Labels, which are exported as `name`, as stored in `tip.label` and, if
#' available, `Node.label`.
#' - Tree topology as stored in `edges`
#'
#' @return An XML document (see [xml2::read_xml])
#' @export
#' @references
#' Han M.V. and Zmasek C.M. (2009) "phyloXML: XML for evolutionary biology and
#' comparative genomics"  BMC Bioinformatics, 10:356 \url{doi:10.1186/1471-2105-10-356}
#' @examples
#'
#' # Random tree from the ape package ------------------------------------------
#' set.seed(1)
#' x <- ape::rtree(5)
#' x
#'
#' write_phyloxml(x)
#' @seealso [validate_phyloxml]
write_phyloxml <- function(tree, ...) UseMethod("write_phyloxml")

#' @rdname write_phyloxml
#' @param xmlns Character scalar. Location of the default namespace
#' (see the [XML Namespaces](https://www.w3schools.com/xml/xml_namespaces.asp)
#' section in w3schools).
#' @param digits Integer scalar. Precision to use when writing numbers.
#' @export
write_phyloxml.phylo <- function(
  tree,
  name = "A phylogenetic tree",
  description = "Some description",
  xmlns = "http://www.phyloxml.org",
  digits = 20,
  ...
) {

  # Obtaining the peeling sequence. We create the  tree by working the tree
  # out using a post order traversal.
  pseq <- getseq(tree)

  n   <- ape::Nnode(tree) + ape::Ntip(tree)
  ans <- vector("list", n)
  hasLengths <- length(tree$edge.length)

  for (j in pseq$ord) {

    # Getting the node
    i <- tree$edge[j, 2]

    # Creating clade
    ans[[i]] <- xml2::xml_new_root("clade") # , id = i

    # Adding label
    if (!is.na(pseq$labels[i]))
      xml2::xml_add_child(
        ans[[i]], "name", pseq$labels[i]
      )

    # Adding branch lengths
    if (hasLengths)
      xml2::xml_add_child(
        ans[[i]], "branch_length",
        sprintf(paste0("%.",digits,"f"),tree$edge.length[j])
      )

    # Has offspring?
    for (o in pseq$offspring[[i]])
      xml2::xml_add_child(ans[[i]], ans[[o]])

  }

  # Adding all together
  i <- tree$edge[j,1]
  ans[[i]] <- xml2::xml_new_root("clade")

  # Adding label
  if (!is.na(pseq$labels[i]))
    xml2::xml_add_child(
      ans[[i]], "name", pseq$labels[i]
    )

  for (o in pseq$offspring[[i]])
    xml2::xml_add_child(ans[[i]], ans[[o]])

  # Creating main document
  phylogeny <- xml2::xml_new_root(
    "phylogeny",
    rooted = ifelse(ape::is.rooted(tree), "true", "false")
  )

  # Adding features
  xml2::xml_add_child(phylogeny, "name", name)
  xml2::xml_add_child(phylogeny, "description", description)
  dots <- list(...)
  for (n in names(dots))
    do.call(xml2::xml_add_child, c(list(phylogeny), n, dots[[n]]))

  xml2::xml_add_child(phylogeny, ans[[i]])

  doc <- xml2::xml_new_root(
    "phyloxml",
    `xmlns:xsi`="http://www.w3.org/2001/XMLSchema-instance",
    xmlns = xmlns,
    `xsi:schemaLocation`="http://www.phyloxml.org http://www.phyloxml.org/1.10/phyloxml.xsd"
  )

  xml2::xml_add_child(doc, phylogeny)

  if (!(err <- validate_phyloxml(doc)))
    stop("Ups! Something went wrong. The validation of the PhyloXML document failed:",
         err, call. = FALSE)

  doc

}

#' Validate a `phyloXML` object using the phyloxml.xsd schema.
#'
#' A wrapper of [xml2::xml_validate]. Performs validation of phyloXML
#' objects using the phyloxml.xsd schema.
#'
#' @param x Either an object of class `xml_document` or a path to a file.
#'
#' @details
#' The schema can be found in \Sexpr{system.file("phyloxml/phyloxml.xsd", package="rphyloxml")}.
#' The latest version of the schema can be downloaded from \url{http://www.phyloxml.org/}
#'
#' @return `TRUE` or `FALSE`
#' @export
validate_phyloxml <- function(x) {


  if (inherits(x, "xml_document")) {
    f <- tempfile()
    xml2::write_xml(x, f, options = c("format"))
  } else {

    if (!file.exists(f))
      stop("The document", f, "does not exists.", call. = FALSE)
    f <- x
  }

  xml2::xml_validate(
    xml2::read_xml(f),
    phyloXML_xsd()
  )
}

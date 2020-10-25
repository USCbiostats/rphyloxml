new_phylo <- function(
  edge,
  tip.label,
  Nnode,
  edge_length = NULL,
  node.label  = NULL,
  root_edge   = NULL
) {

  structure(
    c(
      list(edge = edge),
      # Since edge_length is optional
      if (length(edge_length))
        list(edge_length = edge_length)
      else
        NULL,
      list(
        tip.label  = tip.label,
        Nnode      = Nnode,
        node.label = node.label
      ),
      # Since root_edge is optional
      if (length(root_edge))
        list(root_edge = root_edge)
      else
        NULL
    ),
    class = "phylo"
  )
}

#' Edgelist to phylo object
#' @param x An object of class [matrix].
#' @param edge_length A vector with branch lengths  (optional).
#' @param root_edge A numeric scalar with the length for the root node (optional).
#' @export
#' @importFrom stats complete.cases
edgelist_to_phylo <- function(
  x,
  edge_length = NULL,
  root_edge   = NULL
) {

  # Must be a two column matrix
  if (ncol(x) != 2L)
    stop(
      "`x` must be a two-column matrix. It has ", ncol(x), " instead.",
      call. = FALSE
    )

  # Dropping incomplete cases
  x <- x[stats::complete.cases(x), , drop = FALSE]

  # Retrieving the labels
  if (!inherits(x, "integer")) {

    label <- sort(unique(as.vector(x)))

    # Recasting as an integer vector
    x <- matrix(match(as.vector(x), label), ncol=2L)

  } else {

    label <- range(as.vector(x), na.rm = TRUE)
    label <- label[1]:label[2]

  }

  # Computing degrees
  nodes <- sort(unique(as.vector(x)))
  ideg <- tabulate(x[,2] - nodes[1L] + 1L, nbins = nodes[length(nodes)] - nodes[1L] + 1)
  odeg <- tabulate(x[,1] - nodes[1L] + 1L, nbins = nodes[length(nodes)] - nodes[1L] + 1)

  # Classifying
  roots <- nodes[ideg == 0 & odeg > 0]
  leafs <- nodes[ideg == 1 & odeg == 0]
  inner <- nodes[ideg == 1 & odeg > 0]

  # Multiple parents
  test. <- which(ideg > 1)
  if (length(test.))
    stop("Multiple parents are not supported. The following nodes have multiple parents: ",
         paste(nodes[test.], collapse=", "))

  # Finding roots
  if (length(roots) > 1)
    stop("Multiple root nodes are not supported.")
  if (length(roots) == 0)
    stop("Can't find a root node here.")

  # We will not relabel iff:
  # 1. nodes is integer/numeric vector
  # 2. Leafs are continuously labeled from 1 to n
  # 3. Root is n+1
  # 4. Interior nodes are from n+2 to m
  nleafs <- length(leafs)
  test.   <- is.numeric(nodes) &&
    all(sort(leafs) == 1:length(leafs)) &&
    (roots == (nleafs + 1L))

  # Case in which the only inner node is the root!
  test. <- ifelse(
    length(inner) > 0,
    test. & (sort(inner) == (nleafs + 2L):length(nodes)),
    test.
  )

  # Defining the labels:
  #  - Leafs go from 1 to n
  #  - Root node is n + 1
  #  - And the inner goes from n + 2 to length(nodes)
  # This doest it smoothly
  if (!test.) {
    nodes <- c(leafs, roots, inner)

    # Finding indexes and corresponding new labels
    iroots <- which(x[] == roots)
    lroots <- match(roots, nodes)

    ileafs <- which(x[] %in% leafs)
    lleafs <- match(x[ileafs], nodes)

    iinner <- which(x[] %in% inner)
    linner <- match(x[iinner], nodes)

    # Relabeling the edgelist
    x[iroots] <- lroots
    x[ileafs] <- lleafs
    x[iinner] <- linner
  }

  # Returning the `phylo` object
  new_phylo(
    edge        = unname(x),
    edge_length = unname(edge_length),
    tip.label   = unname(label[leafs]),
    Nnode       = length(inner) + 1L,
    node.label  = unname(label[c(roots, inner)]),
    root_edge   = unname(root_edge)
  )
}


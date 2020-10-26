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
phyloxml_to_phylo <- function(x, labvar = "name") {

  ntrees <- length(x)
  ans    <- structure(vector("list", ntrees), class="multiPhylo")

  for (p in 1L:ntrees) {

    N    <- length(unique(x[[p]][["edges"]][["id"]]))
    leaf <- x[[p]][["edges"]][["isleaf"]]
    node <- which(!leaf)
    leaf <- which(leaf)

    leaf_phy_id <- unlist(sapply(x[[p]][["node.meta"]][leaf], "[[", "name"), recursive = TRUE)

    ans[[p]] <- structure(
      with(
        x[[p]][["edges"]],
        list(
          # We drop the first edge b/c it is certain to be empty, by the way
          # it is parsed
          edge        = unname(cbind(parent, id))[-1L,],
          tip.label   = leaf_phy_id,# x[[p]][["edges"]][[labvar]][leaf],
          edge.length = NULL,
          Nnode       = length(node),
          node.label  = x[[p]][["edges"]][[labvar]][node]
        )
      ),
      class = "phylo"
    )

    # Analyzing if there's a branch
    if (!length(x[[p]][["edges"]][["branch_length"]])) {
      ans[[p]] <- ans[[p]][setdiff(names(ans[[p]]), "edge.length")]
    } else {
      ans[[p]]$edge.length <- x[[p]][["edges"]][["branch_length"]][-1L]

      if (!is.na(x[[p]][["edges"]][["branch_length"]][1L]))
        ans[[p]]$root.edge <- x[[p]][["edges"]][["branch_length"]][1L]
    }

  }

  return(ans)

}

#' @rdname phyloxml_to_phylo
#' @export
phyloxml2phylo <- phyloxml_to_phylo

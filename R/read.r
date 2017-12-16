fillstill <- function(x) {
  ifelse(length(x), x, NA)
}

recode_tree <- function(x) {
  n     <- nrow(x)
  nleaf <- sum(x$isleaf)
  ans   <- vector("integer", n)

  ans[which(x$isleaf)]  <- 1L:nleaf
  ans[which(!x$isleaf)] <- (nleaf + 1L):n

  ans
}

clade_to_dat <- function(x, id = NULL) {

  # If no id has been passed, then start it!
  if (!length(id)) {
    id <- new.env()
    id[["id"]] <- 0L
  }

  # Increasing the id. Since it is an environment, then this will be changed
  # no matter the place it is called, so parallel clades will share the same
  # increment.
  id[["id"]] <- id[["id"]] + 1L

  # Fetching attributes
  y <- data.frame(
    id            = id[["id"]],
    name          = fillstill(x[["name"]][[1]]),
    branch_length = as.numeric(fillstill(x[["branch_length"]][[1]])),
    isleaf        = FALSE,
    parent        = id[["id"]] - 1L
  )

  # Are there any clades?
  clades <- which(names(x) == "clade")
  if (length(clades)) {
    clades <- do.call(
      rbind,
      lapply(x[clades], clade_to_dat, id = id)
    )
  } else {
    y[["isleaf"]] <- TRUE
  }

  return(rbind(y, clades))

}


#' Read phyloXML files
#' @param x Character scalar. Path to a phyloXML file.
#' @export
read_phyloxml <- function(x) {
  doc <- xml2::read_xml(x)
  xml2::as_list(doc)
}

# set.seed(1)
# x <- ape::rtree(10)
# f <- tempfile()
# xml2::write_xml(as_phyloxml(x), file=f)
# dat <- read_phyloxml(f)

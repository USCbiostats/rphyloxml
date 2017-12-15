rm(list = ls(all.names = TRUE))

library(ape)
library(xml2)

set.seed(44)
x <- rtree(4)
x$node.label <- letters[1:Nnode(x)]
plot(x, show.node.label = TRUE)


ans <- getseq(x)
ans$po
str(ans$off)


ans <- as_phyloxml(x)

f <- tempfile()
xml2::write_xml(ans, f, options = c("format"))
cat(readLines(f), sep="\n")

# Validating the document
xml2::xml_validate(xml2::read_xml(f), xml2::read_xml("inst/phyloxml/phyloxml.xsd"))

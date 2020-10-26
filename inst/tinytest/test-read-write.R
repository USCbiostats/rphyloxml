newick <- "(A:0.1,B:0.2,(C:0.3,D:0.4):0.5);"
tree <- ape::read.tree(text = newick)

phyloxml <- write_phyloxml(tree)
tree2 <- read_phyloxml(phyloxml)
tree2 <- phyloxml2phylo(tree2)[[1]]

# plot(tree2)
# plot(tree)

expect_equal(tree2$edge, tree$edge)
expect_equal(tree2$tip.label, tree$tip.label)
expect_equal(tree2$edge.length, tree$edge.length)

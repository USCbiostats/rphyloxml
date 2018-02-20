A list with as many trees as specified. Each element hold the following structures:

-   A data frame describing the tree with `m` observations:
    a.  Parent
    b.  Offspring
    c.  Branch length
    
    This is used mostly to import the data to `phylo` format

-   A list of length `m` with metadata of each branch:
    a.  Length
    b.  Unit
    etc.
    
-   A list of length `n` with metadata for each node. It could have the following structure:
    a.  Node id.
    b.  Taxonomy:
        *   A list
        
The general substructure depends on the PhyloXML format itself. We will only store it as it was recorded
        

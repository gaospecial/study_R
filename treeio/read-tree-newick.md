使用 `read.tree()` 读取 LTP newick 格式的树文件时出现了错误。
初步定为可能是因为其不适应多行造成的，于是做如下实验。

多行格式是否影响树的读取？
--------------------------

**Generate tree**

``` r
oneline_tree <- "((t2:0.04,t1:0.34):0.89,(t5:0.37,(t4:0.03,t3:0.67):0.9):0.59); "
multiline_tree <- "((t2:0.04,
t1:0.34
):0.89,
(t5:0.37,
(t4:0.03,
t3:0.67
):0.9)
:0.59
); "

writeLines(oneline_tree, "tree1.newick")
writeLines(multiline_tree, "tree2.newick")
```

**Read tree**

``` r
library(ggtree)
```

    ## Registered S3 method overwritten by 'treeio':
    ##   method     from
    ##   root.phylo ape

    ## ggtree v2.4.0  For help: https://yulab-smu.github.io/treedata-book/
    ## 
    ## If you use ggtree in published research, please cite the most appropriate paper(s):
    ## 
    ## [36m-[39m Guangchuang Yu. Using ggtree to visualize data on tree-like structures. Current Protocols in Bioinformatics, 2020, 69:e96. doi:10.1002/cpbi.96
    ## [36m-[39m Guangchuang Yu, Tommy Tsan-Yuk Lam, Huachen Zhu, Yi Guan. Two methods for mapping and visualizing associated data on phylogeny using ggtree. Molecular Biology and Evolution 2018, 35(12):3041-3043. doi:10.1093/molbev/msy194
    ## [36m-[39m Guangchuang Yu, David Smith, Huachen Zhu, Yi Guan, Tommy Tsan-Yuk Lam. ggtree: an R package for visualization and annotation of phylogenetic trees with their covariates and other associated data. Methods in Ecology and Evolution 2017, 8(1):28-36. doi:10.1111/2041-210X.12628

``` r
(tr1 <- read.tree("tree1.newick"))
```

    ## 
    ## Phylogenetic tree with 5 tips and 4 internal nodes.
    ## 
    ## Tip labels:
    ##   t2, t1, t5, t4, t3
    ## 
    ## Rooted; includes branch lengths.

``` r
(tr2 <- read.tree("tree2.newick"))
```

    ## 
    ## Phylogenetic tree with 5 tips and 4 internal nodes.
    ## 
    ## Tip labels:
    ##   t2, t1, t5, t4, t3
    ## 
    ## Rooted; includes branch lengths.

结果是不影响。

与 tip label 的格式有关吗？
---------------------------

``` r
quote_tree <- "(('t2':0.04,
't1':0.34
):0.89,
('t5':0.37,
('t4':0.03,
't3':0.67):
0.9):
0.59)
; "
writeLines(quote_tree, "quote_tree.newick")
(read.tree(file = "quote_tree.newick"))
```

    ## 
    ## Phylogenetic tree with 5 tips and 4 internal nodes.
    ## 
    ## Tip labels:
    ##   t2, t2, t2, t2, t2
    ## 
    ## Rooted; includes branch lengths.

这样的话，`tip.label` 就有问题了。 这个问题是与多行 newick
格式中出现带引号的 tip label 有关的。

从 `read.tree()` 源码入手
-------------------------

``` r
ape::read.tree
```

    ## function (file = "", text = NULL, tree.names = NULL, skip = 0, 
    ##     comment.char = "", keep.multi = FALSE, ...) 
    ## {
    ##     if (!is.null(text)) {
    ##         if (!is.character(text)) 
    ##             stop("argument `text' must be of mode character")
    ##         tree <- text
    ##     }
    ##     else {
    ##         tree <- scan(file = file, what = "", sep = "\n", quiet = TRUE, 
    ##             skip = skip, comment.char = comment.char, ...)
    ##     }
    ##     if (identical(tree, character(0))) {
    ##         warning("empty character string.")
    ##         return(NULL)
    ##     }
    ##     tree <- gsub("[ \t]", "", tree)
    ##     tree <- gsub("''", "", tree)
    ##     single_quotes <- function(x, start = 1L) {
    ##         z <- unlist(gregexpr("'", x))
    ##         if (length(z)%%2) 
    ##             stop("wrong number of single quotes around labels")
    ##         l <- length(z)/2
    ##         tmp <- strsplit(x, "'")[[1]]
    ##         ind_orig <- 2L * (1L:l)
    ##         tmp_label <- paste0("@_", start:(start + l - 1), "_@")
    ##         orig_label <- tmp[ind_orig]
    ##         names(orig_label) <- tmp_label
    ##         for (i in 1:l) tmp[2 * i] <- tmp_label[i]
    ##         tmp <- paste0(tmp, collapse = "")
    ##         list(tmp, orig_label)
    ##     }
    ##     z <- grepl("'", tree)
    ##     if (any(z)) {
    ##         Ntree <- length(tree)
    ##         tmp_label <- vector("list", Ntree)
    ##         for (i in 1:Ntree) {
    ##             if (z[i]) {
    ##                 TMP <- single_quotes(tree[i])
    ##                 tree[i] <- TMP[[1]]
    ##                 tmp_label[[i]] <- TMP[[2]]
    ##             }
    ##         }
    ##     }
    ##     y <- unlist(gregexpr(";", tree))
    ##     if (identical(y, nchar(tree))) {
    ##         Ntree <- length(y)
    ##         STRING <- character(Ntree)
    ##         for (i in 1:Ntree) {
    ##             STRING[i] <- gsub("\\[[^]]*\\]", "", tree[i])
    ##         }
    ##     }
    ##     else {
    ##         tree <- unlist(strsplit(tree, NULL))
    ##         y <- which(tree == ";")
    ##         Ntree <- length(y)
    ##         x <- c(1, y[-Ntree] + 1)
    ##         if (is.na(y[1])) 
    ##             return(NULL)
    ##         STRING <- character(Ntree)
    ##         for (i in 1:Ntree) {
    ##             tmp <- paste0(tree[x[i]:y[i]], collapse = "")
    ##             STRING[i] <- gsub("\\[[^]]*\\]", "", tmp)
    ##         }
    ##     }
    ##     STRING <- gsub("^_+", "", STRING)
    ##     STRING <- gsub("_+$", "", STRING)
    ##     getTreeName <- function(x) {
    ##         res <- rep("", length(x))
    ##         i <- regexpr("\\(", x)
    ##         s <- i > 1
    ##         if (any(s)) 
    ##             res[s] <- substr(x[s], 1, i[s] - 1)
    ##         res
    ##     }
    ##     tmpnames <- getTreeName(STRING)
    ##     if (is.null(tree.names) && any(nzchar(tmpnames))) 
    ##         tree.names <- tmpnames
    ##     colon <- grep(":", STRING)
    ##     if (!length(colon)) {
    ##         obj <- lapply(STRING, .cladoBuild)
    ##     }
    ##     else if (length(colon) == Ntree) {
    ##         obj <- lapply(STRING, .treeBuild)
    ##     }
    ##     else {
    ##         obj <- vector("list", Ntree)
    ##         obj[colon] <- lapply(STRING[colon], .treeBuild)
    ##         nocolon <- (1:Ntree)[!1:Ntree %in% colon]
    ##         obj[nocolon] <- lapply(STRING[nocolon], .cladoBuild)
    ##     }
    ##     for (i in 1:Ntree) {
    ##         if (z[i]) {
    ##             tmp_lab <- tmp_label[[i]]
    ##             tip.label <- obj[[i]]$tip.label
    ##             node.label <- obj[[i]]$node.label
    ##             ind <- match(tip.label, names(tmp_lab))
    ##             ind2 <- which(!is.na(ind))
    ##             if (length(ind2)) {
    ##                 tip.label[ind2] <- tmp_lab[ind[ind2]]
    ##                 tmp_lab <- tmp_lab[-ind[ind2]]
    ##             }
    ##             ind <- match(node.label, names(tmp_lab))
    ##             ind2 <- which(!is.na(ind))
    ##             if (length(ind2)) {
    ##                 node.label[ind2] <- tmp_lab[ind[ind2]]
    ##                 tmp_lab <- tmp_lab[-ind[ind2]]
    ##             }
    ##             if (length(tmp_lab)) {
    ##                 for (j in 1:length(tmp_lab)) {
    ##                   node.label <- gsub(names(tmp_lab)[j], tmp_lab[j], 
    ##                     node.label)
    ##                   tip.label <- gsub(names(tmp_lab)[j], tmp_lab[j], 
    ##                     tip.label)
    ##                 }
    ##             }
    ##             obj[[i]]$tip.label <- tip.label
    ##             obj[[i]]$node.label <- node.label
    ##         }
    ##     }
    ##     if (Ntree == 1 && !keep.multi) 
    ##         obj <- obj[[1]]
    ##     else {
    ##         if (!is.null(tree.names)) 
    ##             names(obj) <- tree.names
    ##         class(obj) <- "multiPhylo"
    ##     }
    ##     obj
    ## }
    ## <bytecode: 0x00000000155b13c0>
    ## <environment: namespace:ape>

经查可能与 `single_quotes()` 这个函数的功能有关。

考虑到这种情况仅出现在 **多行** 且 **带引号** 的时候，
那么最简单的解决方法就是把多行变成单行。

``` r
file <- xfun::magic_path("quote_tree.newick")
tree <- scan(file = file, what = "", sep = "\n", quiet = TRUE)
tree_collapsed <- paste0(tree, collapse = "")
read.tree(text = tree)
```

    ## 
    ## Phylogenetic tree with 5 tips and 4 internal nodes.
    ## 
    ## Tip labels:
    ##   t2, t2, t2, t2, t2
    ## 
    ## Rooted; includes branch lengths.

``` r
read.tree(text = tree_collapsed)
```

    ## 
    ## Phylogenetic tree with 5 tips and 4 internal nodes.
    ## 
    ## Tip labels:
    ##   t2, t1, t5, t4, t3
    ## 
    ## Rooted; includes branch lengths.

这样就搞定了。

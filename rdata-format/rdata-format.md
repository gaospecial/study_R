`bibliometrix` 使用 `load()` 函数来读取 R 数据，读取 `*.Rds` 时会出错。

问题和解答来自这里：
<https://stackoverflow.com/questions/21370132/r-data-formats-rdata-rda-rds-etc>

**问题**

> What are the main differences between .RData, .Rda and .Rds files?
>
> More specifically:
>
> Are there differences in compression etc? When should each type be
> used? How to convert one type to another?

**解答**

`Rda` is just a short name for `RData`. You can just `save()`, `load()`,
`attach()`, etc. just like you do with RData.

`Rds` stores a single R object. Yet, beyond that simple explanation,
there are several differences from a “standard” storage. Probably this
R-manual Link to `readRDS()` function clarifies such distinctions
sufficiently.

So, answering your questions:

The difference is not about the compression, but serialization (See this
page) Like shown in the manual page, you may wanna use it to restore a
certain object with a different name, for instance. You may `readRDS()`
and `save()`, or `load()` and `saveRDS()` selectively.

下面是几个例子。

先分别保存一个数据到 `rda` 和 `rds` 格式（两个语句语法有差异）。

``` r
# create a data d, and save it
d <- 1:5
save(d, file = "rda.Rda")
saveRDS(d, file = "rds.rds")
rm(d)
```

然后再读取他们。

首先 `load()`，`load()`
直接将原来的变量导入，而变量的名称则被其当做返回值赋值给了 `a`。

``` r
a <- load("rda.rda")

a
```

    ## [1] "d"

``` r
d
```

    ## [1] 1 2 3 4 5

``` r
rm(a,d)
```

其次看 `readRDS()`, 它将变量的值赋给了 `c`。

``` r
c <- readRDS("rds.rds")
c
```

    ## [1] 1 2 3 4 5

``` r
rm(c)
```

使用 `load()` 读取 `.rds` 数据会出错。

``` r
load("rds.rds")
```

    ## Warning in readChar(con, 5L, useBytes = TRUE): truncating string with embedded
    ## nuls

    ## Warning: file 'rds.rds' has magic number 'X'
    ##   Use of save versions prior to 2 is deprecated

    ## Error in load("rds.rds"): 復原文件幻数出错(文件可能有损坏)-- 没有载入任何数据

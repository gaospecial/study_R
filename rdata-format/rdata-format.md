在 `bibliometrix` 中使用 `load()` 函数来读取 R 数据，读取 `*.Rds`
时会出错。

要弄清楚这个问题，首先需要了解 R 数据格式的内涵

R 数据格式（.RData, .Rda, .Rds）
--------------------------------

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
e <- 8
save(d, file = "rda.Rda")
save.image(file = "image.RData")
saveRDS(d, file = "rds.rds")
rm(d,e)
```

然后再读取他们。

首先 `load()`，`load()`
直接将原来的变量导入，而变量的名称则被其当做返回值赋值给了 `a`。

``` r
a <- load("rda.rda")
```

`load()` 可以载入单个变量，也可以载入多个变量。

``` r
a
```

    ## [1] "d"

``` r
d
```

    ## [1] 1 2 3 4 5

如果想把 load() 的变量改个名字，则可以这样操作。

``` r
eval(parse(text = paste0("c <- ",a)))
c
```

    ## [1] 1 2 3 4 5

``` r
rm(list=ls())
```

``` r
a <- load("image.RData")
a
```

    ## [1] "d" "e"

``` r
d
```

    ## [1] 1 2 3 4 5

``` r
e
```

    ## [1] 8

这时候如果还想要通过上面的方法改变变量的名称，那就有问题了。

``` r
(cmd <- parse(text = paste0("c <- ",a)))
```

    ## expression(c <- d, c <- e)

``` r
eval(cmd)
c
```

    ## [1] 8

在多次赋值后，最后一个值被赋给了 `c` 变量，同时没有任何提示。

``` r
rm(list=ls())
```

如果是一个空的环境的话，保存后会出现什么情况呢？

``` r
save.image(file = "empty.RData")
a <- load("empty.RData")
a
```

    ## character(0)

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

load() .RData 时如何找到你要的对象？
------------------------------------

首先保存 3 个对象到 .RData 中

``` r
rm(list = ls())
(a <- 1)
```

    ## [1] 1

``` r
(b <- "a")
```

    ## [1] "a"

``` r
(c <- 1:4)
```

    ## [1] 1 2 3 4

``` r
class(a)
```

    ## [1] "numeric"

``` r
class(b)
```

    ## [1] "character"

``` r
class(c)
```

    ## [1] "integer"

``` r
save.image(file = "three_obj.RData")
```

``` r
(var <- load("three_obj.RData"))
```

    ## [1] "a" "b" "c"

``` r
(var_class <- sapply(var, function(v) eval(parse(text = paste0("class(", v,")")))))
```

    ##           a           b           c 
    ##   "numeric" "character"   "integer"

``` r
grep("character", var_class, fixed = TRUE)
```

    ## [1] 2

Subsequently, we can find the only character variable.

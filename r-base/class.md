我们构造一个常用的 `ggplot` 对象。

``` r
library(ggplot2)
p <- ggplot()
```

大概可以有以下几种情况获得一些对象信息。

``` r
class(p)
```

    ## [1] "gg"     "ggplot"

``` r
typeof(p)
```

    ## [1] "list"

``` r
attr(p,"class")
```

    ## [1] "gg"     "ggplot"

那么如何判断它的 class 呢？

方法一：最方便的 `is()`
-----------------------

``` r
is.ggplot(p)
```

    ## [1] TRUE

``` r
is(p,"ggplot")
```

    ## [1] TRUE

这可能是最简便的方法了。`methods("is")` 是一个 S3
泛型函数，可以用于检测对象的类。 还有其它一些函数列在下面。

``` r
methods("is")
```

    ## Warning in .S3methods(generic.function, class, envir): function 'is' appears not
    ## to be S3 generic; found functions that look like S3 methods

    ##  [1] is.array                is.atomic               is.call                
    ##  [4] is.character            is.complex              is.Coord               
    ##  [7] is.data.frame           is.double               is.element             
    ## [10] is.empty.model          is.environment          is.expression          
    ## [13] is.facet                is.factor               is.finite              
    ## [16] is.function             is.ggplot               is.ggproto             
    ## [19] is.infinite             is.integer              is.language            
    ## [22] is.leaf                 is.list                 is.loaded              
    ## [25] is.logical              is.matrix               is.mts                 
    ## [28] is.na                   is.na.data.frame        is.na.numeric_version  
    ## [31] is.na.POSIXlt           is.na<-                 is.na<-.default        
    ## [34] is.na<-.factor          is.na<-.numeric_version is.name                
    ## [37] is.nan                  is.null                 is.numeric             
    ## [40] is.numeric.Date         is.numeric.difftime     is.numeric.POSIXt      
    ## [43] is.numeric_version      is.object               is.ordered             
    ## [46] is.package_version      is.pairlist             is.primitive           
    ## [49] is.qr                   is.R                    is.raster              
    ## [52] is.raw                  is.recursive            is.relistable          
    ## [55] is.single               is.stepfun              is.symbol              
    ## [58] is.table                is.theme                is.ts                  
    ## [61] is.tskernel             is.unsorted             is.vector              
    ## see '?methods' for accessing help and source code

方法二：最直观的 `class()`
--------------------------

``` r
"ggplot" %in% class(p)
```

    ## [1] TRUE

因为一个对象的 Class 不止一种，所以不能直接用数值比较。 但是加上一个
`any()` 或许也可以达到同样的效果。

``` r
class(p) == "gg"
```

    ## [1]  TRUE FALSE

``` r
any(class(p) == "gg")
```

    ## [1] TRUE

方法三：最严谨的 `inherits()`
-----------------------------

``` r
inherits(p, "ggplot")
```

    ## [1] TRUE

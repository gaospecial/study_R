生成 4 sets椭圆
---------------

``` r
library(VennDiagram)
```

    ## Loading required package: grid

    ## Loading required package: futile.logger

    ## 
    ## Attaching package: 'VennDiagram'

    ## The following object is masked _by_ '.GlobalEnv':
    ## 
    ##     ell2poly

``` r
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.2.1 --

    ## v ggplot2 3.1.1       v purrr   0.3.2  
    ## v tibble  2.1.1       v dplyr   0.8.0.1
    ## v tidyr   0.8.3       v stringr 1.4.0  
    ## v readr   1.3.1       v forcats 0.4.0

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(cowplot)
```

    ## 
    ## Attaching package: 'cowplot'

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     ggsave

``` r
sets <- 4
ellipse_4d_parameters <- list(c(0.65, 0.47, 0.35, 0.20,  45),
                              c(0.35, 0.47, 0.35, 0.20, 135),
                              c(0.50, 0.57, 0.33, 0.15,  45),
                              c(0.50, 0.57, 0.35, 0.15, 135))
ellipse_4d_coordinations <- lapply(1:sets,function(i){
  x <- ellipse_4d_parameters[[i]]
  do.call("ell2poly",as.list(c(x,n.sides=3000))) %>% 
    data.frame() %>% 
    mutate(group=paste("set",i,sep=""))
  })
df <- do.call("rbind",ellipse_4d_coordinations)
head(df)
```

    ##           x         y group
    ## 1 0.8974874 0.7174874  set1
    ## 2 0.8971906 0.7177830  set1
    ## 3 0.8968928 0.7180776  set1
    ## 4 0.8965939 0.7183711  set1
    ## 5 0.8962939 0.7186634  set1
    ## 6 0.8959929 0.7189547  set1

使用`ggplot2`的`geom_polyon()`函数画图。

``` r
ggplot(df,aes(x,y)) + geom_polygon(aes(group=group)) + coord_equal()
```

![](materials_files/figure-markdown_github/unnamed-chunk-2-1.png)

可以使用其它映射改变填充色，线条属性等。

``` r
ggplot(df,aes(x,y)) +
  geom_polygon(aes(group=group),color="grey",fill=rep(1:4,each=3001),alpha=1/4,size=2,lty="dashed") + 
  # geom_text(aes(label=group),check_overlap = T) +  
  coord_equal()
```

![](materials_files/figure-markdown_github/unnamed-chunk-3-1.png)

colorfulVennPlot的问题
----------------------

colorfulVennPlot可以按不同区块着色，但是遗憾的是4个椭圆的参数有问题，导致出现了不该有的色块。

先生成一个4 sets演示数据。

``` r
set.seed(20190702)
tank <- 1:100
sets <- lapply(c(10,20,30,40), function(x)sample(tank,x))
names(sets) <- LETTERS[1:4]
```

分别使用`VennDiagram`和`colorfulVennPlot`绘图。

``` r
library(VennDiagram)
venn.diagram(x=sets,filename = "vennDiagram.png")
```

    ## [1] 1

``` r
library(colorfulVennPlot)
Colors <- c('red', 'yellow', 'green', 'pink', 'darkgreen','blue','lightblue','tan',   'yellowgreen','orange','purple','white','grey','plum','brown')
regions <- seq(15)
names(regions) <- c('1000', '0100', '1100', '0010', '1010', '0110', '1110', '0001', '1001', '0101', '1101', '0011', '1011', '0111', '1111')

plotVenn4d(1:15)
```

![](materials_files/figure-markdown_github/unnamed-chunk-6-1.png)

关于椭圆的生成函数
------------------

在数学上，椭圆是平面上到两个固定点的距离之和为常数的点之轨迹。

中心位于点 (*h*, *k*)的主轴平行于 *x* 轴的椭圆由如下方程指定:

(*x* − *h*)<sup>2</sup>/*a*<sup>2</sup> + (*y* − *k*)<sup>2</sup>/*b*<sup>2</sup> = 1

这个椭圆可以参数化表达为：

*x* = *h* + *a* \* *c**o**s*(*t*)
*y* = *k* + *b* \* *s**i**n*(*t*)
在`VennDiagram`中绘制椭圆的函数是：

``` r
ell2poly <- function (x, y, a, b, rotation=0, n.sides=360) {
    rotation <- rotation * pi/180
    theta <- 2 * pi/n.sides
    angles <- seq(0, 2 * pi, theta)
    x.coord <- vector(length = n.sides + 1, mode = "numeric")
    x.coord[1] <- x + a * cos(rotation)
    y.coord <- vector(length = n.sides + 1, mode = "numeric")
    y.coord[1] <- y + a * sin(rotation)
    for (i in 1:n.sides) {
        x.coord[i + 1] <- x + a * cos(angles[i + 1]) * cos(rotation) - 
            b * sin(angles[i + 1]) * sin(rotation)
        y.coord[i + 1] <- y + a * cos(angles[i + 1]) * sin(rotation) + 
            b * sin(angles[i + 1]) * cos(rotation)
    }
    return(list(x = x.coord, y = y.coord))
}
```

在`colorfulVennPlot`中绘制椭圆的函数是：

``` r
xyangle <- function(x, y, directed = FALSE, deg = TRUE){
    if (missing(y)) {
        y <- x[, 2]
        x <- x[, 1]
    }
    out <- atan2(y, x)
    if (!directed)
        out <- out%%pi
    if (deg)
        out <- out * 180/pi
    out
}
draw1ellipse <- function(x, y, a = 1, b = 1, angle = 0, 
                         segment = c(0,360),
                         arc.only = TRUE, 
                         nv = 100, 
                         deg = TRUE, 
                         border = NULL, 
                         col = NA, 
                         lty = 1, 
                         lwd = 1, ...){
    if (deg) {
        angle <- angle * pi/180
        segment <- segment * pi/180
    }
    z <- seq(segment[1], segment[2], length = nv + 1)
    xx <- a * cos(z)
    yy <- b * sin(z)
    alpha <- xyangle(xx, yy, directed = TRUE, deg = FALSE)
    rad <- sqrt(xx^2 + yy^2)
    xp <- rad * cos(alpha + angle) + x
    yp <- rad * sin(alpha + angle) + y
    if (!arc.only) {
        xp <- c(x, xp, x)
        yp <- c(y, yp, y)
    }

    list(x=xp, y=yp)
}
```

下面分别比较一下两个函数。

``` r
two <- list(draw1ellipse(5,5,5,10,45),ell2poly(5,5,5,10,45))
names(two) <- c("draw1ellipse","ell2poly")

plots <- lapply(two, function(e)ggplot(aes(x=x,y=y),data = data.frame(e)) + geom_polygon() + coord_equal())


plot_grid(plotlist = plots)
```

![](materials_files/figure-markdown_github/unnamed-chunk-9-1.png)

两个函数生成的图形其实是一样的。那之所以`colorfulVennPlot`的图形有毛病，纯粹是作者选择椭圆参数时没有细化造成的。

``` r
parameters <- data.frame(x =   c( 0,   0,  0,   5),
                         y =   c( 0,   5,  5,   5),
                         a =   c(10,  10,  5,   5),
                         b =   c( 5,   5, 10,  10)，
                         rot = c( 0,   0,  0,   0))
ellipse_coordinations <- lapply(1:4, function(i){
  args <- as.list(parameters[i,])
  names(args) <- c("x","y","a","b","rotation")
  do.call("ell2poly",args) %>% data.frame() %>%
    mutate(group=LETTERS[[i]])
})
df <- do.call("rbind",ellipse_coordinations)
ggplot(df,aes(x,y)) + 
  geom_polygon(aes(group=group,fill=group,color=group),alpha=0.5)  + 
  # geom_text(aes(label=group),check_overlap = T) +
  coord_equal()
```

![](materials_files/figure-markdown_github/unnamed-chunk-10-1.png)

这样，可以很明显的看出来这个韦恩图的椭圆形参数存在的问题。

分区着色
--------

在`colorfulVennPlot`中，不同区域的着色是靠椭圆交点的坐标限制多边形而获得的。
这些交点和色块的中心点都是预定义的（被作者称为**硬核代码**，虽然不好看，但是能用）。

如果要更换新的椭圆，那么需要解析新椭圆之间交点的坐标。

``` r
# Hard-coded crossover regions
regions <- rbind(
  data.frame(i = 1, j = 2, x = c(rep(-8.66025,3), rep(8.66025, 3)), y = 2.5,
    TF = c('0100', '1000', '1100', '0101', '1001', '1101')),
  data.frame(i = 1, j = 3, x = c(rep(0.01184, 2),rep(4.988876, 4),rep(-4.988876,4)), y = c(rep(-4.99997,2),rep(4.3333,8)),
    TF = c('1000', '1010', '0101', '0111', '1101', '1111', '0100', '0110', '1100', '1110')),
  data.frame(i = 1, j = 4, x = c(3.6852,8,9.648,rep(0,4)), y = c(-4.648,-2.9987,1.3147, rep(5, 4)),
    TF = c('1001', '1001', '1001', '0110', '0111', '1110', '1111')),
  data.frame(i = 2, j = 3, x = c(rep(-4.472,4), rep(4.472,4), rep(-4.472,3),rep(4.472,4)), y = c(rep(0.52786,8), rep(9.472, 7)),
    TF = c('1000', '1010', '1100', '1110', '1001', '1011', '1101', '1111', '0010', '0100', '0110', '0001', '0011', '0101', '0111')),
  data.frame(i = 2, j = 4, x = c(rep(0.667,8), rep(10, 2)), y = c(rep(9.989, 4), rep(0.011, 4), rep(4.969,2)),
    TF = c('0010', '0011', '0110', '0111', '1010', '1011', '1110', '1111', '0001', '0101')),
  data.frame(i = 3, j = 4, x = 2.5, y = c(rep(-3.66025, 3),rep(13.66, 3)), TF = c('1001', '1010', '1011', '0001', '0010', '0011'))
)

# Midpoints for hard-coded crossover regions
midpoints <- data.frame(
  x = c(-4.37352, -6.04042, -6.04042, -0.43516, -0.32341, -2.19859, -2.19859, 5.65737, 6.16114, 7.03031, 6.04042, 2.54627, 2.54627, 2.53192, 2.53192),
  y = c(-0.65737, 5.43516, 2.45373, 11.04042, -2.03031, 7.19859, 2.46808, 9.36716, -1.16074, 5.31864, 2.45373, 11.04042, -1.04042, 7.19859, 2.46808),
  TF = c('1000', '0100', '1100', '0010', '1010', '0110', '1110', '0001', '1001', '0101', '1101', '0011', '1011', '0111', '1111'))
```

### 求解交点坐标

交点坐标的求解，有以下几个方法：

1.  用鼠标指向交点，获得对于的坐标；
2.  用方程求解；
3.  用形状交集来找到交点（<a href="https://codeday.me/bug/20190207/626983.html" class="uri">https://codeday.me/bug/20190207/626983.html</a>）；

``` python
import numpy as np
from shapely.geometry.polygon import LinearRing

def ellipse_polyline(ellipses, n=100):
    t = linspace(0, 2*np.pi, n, endpoint=False)
    st = np.sin(t)
    ct = np.cos(t)
    result = []
    for x0, y0, a, b, angle in ellipses:
        angle = np.deg2rad(angle)
        sa = np.sin(angle)
        ca = np.cos(angle)
        p = np.empty((n, 2))
        p[:, 0] = x0 + a * ca * ct - b * sa * st
        p[:, 1] = y0 + a * sa * ct + b * ca * st
        result.append(p)
    return result

def intersections(a, b):
    ea = LinearRing(a)
    eb = LinearRing(b)
    mp = ea.intersection(eb)

    x = [p.x for p in mp]
    y = [p.y for p in mp]
    return x, y

ellipses = [(1, 1, 2, 1, 45), (2, 0.5, 5, 1.5, -30)]
a, b = ellipse_polyline(ellipses)
x, y = intersections(a, b)
plot(x, y, "o")
plot(a[:,0], a[:,1])
plot(b[:,0], b[:,1])
```

当前，`shapely`在`Windows`系统下安装不成功（缺少`geos_c.dll`库）。

虽然`python`的方法暂时不能用，但是却启发我想到了一个*求交集*的方案。
基于此，自创了一个通过对坐标取交集找交点的方法。

``` r
# 4个椭圆共有14个交点
# 每两个椭圆可能有1,2,4个交点
library(VennDiagram)
library(tidyverse)
library(cowplot)
sets <- 4
parameters <- list(c(0.65, 0.47, 0.35, 0.20,  45),
                   c(0.35, 0.47, 0.35, 0.20, 135),
                   c(0.50, 0.57, 0.33, 0.15,  45),
                   c(0.50, 0.57, 0.35, 0.15, 135))
coordinations <- lapply(1:sets,function(i){
  x <- ellipse_4d_parameters[[i]]
  do.call("ell2poly",as.list(c(x,n.sides=50000))) %>% 
    data.frame() %>% 
    mutate(coord=paste(as.character(round(x,digits = 4)),
                       as.character(round(y,digits = 4)),
                       sep=","),
           group=LETTERS[[i]])
  })

for (i in 1:(sets-1)){
  for (j in (i+1):sets){
    print(paste(paste(LETTERS[[i]],"x",LETTERS[[j]],sep=""),
          paste(t(intersect(coordinations[[i]]$coord,
                            coordinations[[j]]$coord)),
                collapse = ";"),
          sep=":"))
  }
}
```

    ## [1] "AxB:0.5,0.6027;0.5,0.6026;0.4991,0.185;0.4992,0.185;0.4993,0.185;0.4994,0.185;0.4995,0.185;0.4996,0.185;0.4997,0.185;0.4998,0.185;0.4999,0.185;0.5,0.185;0.5001,0.185;0.5002,0.185;0.5003,0.185;0.5004,0.185;0.5005,0.185;0.5006,0.185;0.5007,0.185;0.5008,0.185;0.5009,0.185"
    ## [1] "AxC:0.7557,0.7517;0.3651,0.3171"
    ## [1] "AxD:0.5974,0.6846;0.399,0.4589"
    ## [1] "BxC:0.601,0.4589;0.4027,0.6846;0.4026,0.6846"
    ## [1] "BxD:0.6343,0.3084;0.2308,0.7536"
    ## [1] "CxD:0.5014,0.764;0.306,0.5686;0.4986,0.376;0.694,0.5714"

在这里，我尝试了多个不同组合，最终能够找出全部的14个交点的坐标。为了保证交点坐标位置比较准确，位置坐标用小数点后4位有效数字来表示；在此基础上，每个椭圆需要5W个点（5K少了）。

根据上面的结果，我们编一段硬代码。

``` r
s <- "AxB:0.5,0.6027;0.4995,0.185
AxC:0.7557,0.7517;0.3651,0.3171
AxD:0.5974,0.6847;0.399,0.4589
BxC:0.601,0.4589;0.4027,0.6846
BxD:0.6343,0.3084;0.2308,0.7536
CxD:0.5014,0.764;0.306,0.5686;0.4986,0.376;0.694,0.5714"
crossing_points <- str_split(s,"\n") %>% 
  unlist() %>% 
  str_split(":") %>% 
  do.call("rbind",.) %>% 
  data.frame() %>% 
  separate(X1,c("set1","set2"),sep="x") %>% 
  separate_rows(X2,sep = ";") %>% 
  separate(X2,c("x","y"),sep = ",") %>% 
  mutate(x=as.numeric(x),y=as.numeric(y))
head(crossing_points)
```

    ##   set1 set2      x      y
    ## 1    A    B 0.5000 0.6027
    ## 2    A    B 0.4995 0.1850
    ## 3    A    C 0.7557 0.7517
    ## 4    A    C 0.3651 0.3171
    ## 5    A    D 0.5974 0.6847
    ## 6    A    D 0.3990 0.4589

下面作图验证。

``` r
df <- do.call(rbind,coordinations)
ggplot(df,aes(x,y)) + 
  geom_polygon(aes(group=group,color=group,fill=group),alpha=0.5) +
  geom_point(data = crossing_points,color="black",size=2) +
  geom_text(aes(label=paste(x,y,sep=",")),data = crossing_points,vjust=1.2,hjust=0) +
  coord_equal()
```

![](materials_files/figure-markdown_github/unnamed-chunk-15-1.png)

显然，节点的位置定位还是非常精确的。
这样的话，就可以用上述椭圆和节点去改造`colorfulVennPlot`，得到一个完美的4D韦恩图。

继续看它的代码，其中定义region的几行如下：

``` r
regions <- rbind(
  data.frame(i = 1, j = 2, x = c(rep(-8.66025,3), rep(8.66025, 3)), y = 2.5,
    TF = c('0100', '1000', '1100', '0101', '1001', '1101')),
  data.frame(i = 1, j = 3, x = c(rep(0.01184, 2),rep(4.988876, 4),rep(-4.988876,4)), y = c(rep(-4.99997,2),rep(4.3333,8)),
    TF = c('1000', '1010', '0101', '0111', '1101', '1111', '0100', '0110', '1100', '1110')),
  data.frame(i = 1, j = 4, x = c(3.6852,8,9.648,rep(0,4)), y = c(-4.648,-2.9987,1.3147, rep(5, 4)),
    TF = c('1001', '1001', '1001', '0110', '0111', '1110', '1111')),
  data.frame(i = 2, j = 3, x = c(rep(-4.472,4), rep(4.472,4), rep(-4.472,3),rep(4.472,4)), y = c(rep(0.52786,8), rep(9.472, 7)),
    TF = c('1000', '1010', '1100', '1110', '1001', '1011', '1101', '1111', '0010', '0100', '0110', '0001', '0011', '0101', '0111')),
  data.frame(i = 2, j = 4, x = c(rep(0.667,8), rep(10, 2)), y = c(rep(9.989, 4), rep(0.011, 4), rep(4.969,2)),
    TF = c('0010', '0011', '0110', '0111', '1010', '1011', '1110', '1111', '0001', '0101')),
  data.frame(i = 3, j = 4, x = 2.5, y = c(rep(-3.66025, 3),rep(13.66, 3)), TF = c('1001', '1010', '1011', '0001', '0010', '0011'))
)
regions
```

    ##    i j         x        y   TF
    ## 1  1 2 -8.660250  2.50000 0100
    ## 2  1 2 -8.660250  2.50000 1000
    ## 3  1 2 -8.660250  2.50000 1100
    ## 4  1 2  8.660250  2.50000 0101
    ## 5  1 2  8.660250  2.50000 1001
    ## 6  1 2  8.660250  2.50000 1101
    ## 7  1 3  0.011840 -4.99997 1000
    ## 8  1 3  0.011840 -4.99997 1010
    ## 9  1 3  4.988876  4.33330 0101
    ## 10 1 3  4.988876  4.33330 0111
    ## 11 1 3  4.988876  4.33330 1101
    ## 12 1 3  4.988876  4.33330 1111
    ## 13 1 3 -4.988876  4.33330 0100
    ## 14 1 3 -4.988876  4.33330 0110
    ## 15 1 3 -4.988876  4.33330 1100
    ## 16 1 3 -4.988876  4.33330 1110
    ## 17 1 4  3.685200 -4.64800 1001
    ## 18 1 4  8.000000 -2.99870 1001
    ## 19 1 4  9.648000  1.31470 1001
    ## 20 1 4  0.000000  5.00000 0110
    ##  [ reached 'max' / getOption("max.print") -- omitted 34 rows ]

``` r
library(ggrepel)
ggplot(df,aes(x,y)) + 
  geom_polygon(aes(group=group,fill=group,color=group),alpha=0.5)  + 
  # geom_text(aes(label=group),check_overlap = T) +
  geom_point(data=regions) + 
  geom_text(aes(label=paste(round(x,2),round(y,2),sep=",")),
            data = regions,vjust=1.2,hjust=0) +
  coord_equal(xlim = c(min(df$x),max(df$x)*1.3))
```

![](materials_files/figure-markdown_github/unnamed-chunk-17-1.png)

看看region是个啥
----------------

``` r
regions %>% arrange(TF)
```

    ##    i j         x        y   TF
    ## 1  1 2 -8.660250  2.50000 0100
    ## 2  1 3 -4.988876  4.33330 0100
    ## 3  2 3 -4.472000  9.47200 0100
    ## 4  1 2  8.660250  2.50000 0101
    ## 5  1 3  4.988876  4.33330 0101
    ## 6  2 3  4.472000  9.47200 0101
    ## 7  2 4 10.000000  4.96900 0101
    ## 8  1 2 -8.660250  2.50000 1000
    ## 9  1 3  0.011840 -4.99997 1000
    ## 10 2 3 -4.472000  0.52786 1000
    ## 11 1 2  8.660250  2.50000 1001
    ## 12 1 4  3.685200 -4.64800 1001
    ## 13 1 4  8.000000 -2.99870 1001
    ## 14 1 4  9.648000  1.31470 1001
    ## 15 2 3  4.472000  0.52786 1001
    ## 16 3 4  2.500000 -3.66025 1001
    ## 17 1 2 -8.660250  2.50000 1100
    ## 18 1 3 -4.988876  4.33330 1100
    ## 19 2 3 -4.472000  0.52786 1100
    ## 20 1 2  8.660250  2.50000 1101
    ##  [ reached 'max' / getOption("max.print") -- omitted 34 rows ]

每个多边形由3-4个顶点决定。`regions`保存的是顶点的坐标。

### 计算多边形的中心点

``` r
library(sf)
```

    ## Linking to GEOS 3.6.1, GDAL 2.2.3, PROJ 4.9.3

``` r
library(ggplot2)

# I transform to utm because st_centroid is not recommended for use on long/lat 
nc <- st_read(system.file('shape/nc.shp', package = "sf")) %>% 
  st_transform(32617)
```

    ## Reading layer `nc' from data source `D:\R\R_LIBS_USER\sf\shape\nc.shp' using driver `ESRI Shapefile'
    ## Simple feature collection with 100 features and 14 fields
    ## geometry type:  MULTIPOLYGON
    ## dimension:      XY
    ## bbox:           xmin: -84.32385 ymin: 33.88199 xmax: -75.45698 ymax: 36.58965
    ## epsg (SRID):    4267
    ## proj4string:    +proj=longlat +datum=NAD27 +no_defs

``` r
# using rgeos
# sp_cent <- gCentroid(as(nc, "Spatial"), byid = TRUE)

# using sf
sf_cent <- st_centroid(nc)
```

    ## Warning in st_centroid.sf(nc): st_centroid assumes attributes are constant
    ## over geometries of x

``` r
# plot both together to confirm that they are equivalent
ggplot() + 
  geom_sf(data = nc, fill = 'white') +
  # geom_sf(data = sp_cent %>% st_as_sf, color = 'blue') + 
  geom_sf(data = sf_cent, color = 'red') 
```

![](materials_files/figure-markdown_github/unnamed-chunk-19-1.png)

使用sf可以计算两个多边形之间的交集。

``` r
opar <- par(mfrow = c(1, 2))
a <- st_polygon(list(cbind(c(0,0,7.5,7.5,0),c(0,-1,-1,0,0))))
b <- st_polygon(list(cbind(c(0,1,2,3,4,5,6,7,7,0),c(1,0,.5,0,0,0.5,-0.5,-0.5,1,1))))
plot(a, ylim = c(-1,1))
title("intersecting two polygons:")
plot(b, add = TRUE, border = 'red')
(i <- st_intersection(a,b))
```

    ## GEOMETRYCOLLECTION (POINT (1 0), LINESTRING (4 0, 3 0), POLYGON ((5.5 0, 7 0, 7 -0.5, 6 -0.5, 5.5 0)))

``` r
## GEOMETRYCOLLECTION (POINT (1 0), LINESTRING (4 0, 3 0), POLYGON ((5.5 0, 7 0, 7 -0.5, 6 -0.5, 5.5 0)))
plot(a, ylim = c(-1,1))
title("GEOMETRYCOLLECTION")
plot(b, add = TRUE, border = 'red')
plot(i, add = TRUE, col = 'green', lwd = 2)
```

![](materials_files/figure-markdown_github/unnamed-chunk-20-1.png)

计算两个椭圆的交集。

``` r
# 需要对坐标精度进行调整，否则会报告多边形未闭合
a <- st_polygon(list(cbind(round(coordinations[[1]]$x,6),round(coordinations[[1]]$y,6))))
b <- st_polygon(list(cbind(round(coordinations[[2]]$x,6),round(coordinations[[2]]$y,6))))
c
```

    ## function (...)  .Primitive("c")

``` r
i <- st_intersection(a,b)
d1 <- st_difference(a,b)
d2 <- st_difference(b,a)
plot(a)
plot(b, add=TRUE)
plot(i, add=TRUE,col="green")
plot(d1, add=TRUE,col="red")
plot(d2, add=TRUE,col="blue")
```

![](materials_files/figure-markdown_github/unnamed-chunk-21-1.png)

这个函数可以计算多边形的交集等参数。可以用来定位椭圆的各个部分。
对于多个图形的交集，可以先得到两个图形的交集，然后在与其他图形计算交集。这里有一个循环或者递归的过程。
查看
<a href="https://cran.r-project.org/web/packages/sf/index.html" class="uri">https://cran.r-project.org/web/packages/sf/index.html</a>
获得更多有用的信息。

这个韦恩图总共分为14个部分，分别是A,B,C,D,AB,AC,AD,BC,BD,CD,ABC,ACD,BCD和ABCD。
他们的计算方法分别是：

A,B,C,D：*A* = *A*<sub>0</sub> − *B*<sub>0</sub> − *C*<sub>0</sub> − *D*<sub>0</sub>
ABCD：*A**B**C**D* = *i**n**t**e**r**s**e**c**t*(*i**n**t**e**r**a**s**e**c**t*(*i**n**t**e**r**s**e**c**t*(*A*<sub>0</sub>, *B*<sub>0</sub>), *C*<sub>0</sub>), *D*<sub>0</sub>)
ABC,ACD,BCD：*A**B**C* = *i**n**t**e**r**a**s**e**c**t*(*i**n**t**e**r**s**e**c**t*(*A*<sub>0</sub>, *B*<sub>0</sub>), *C*<sub>0</sub>) − *A**B**C**D*
AB,AC,AD,BC,BD,CD：*A**B* = *i**n**t**e**r**s**e**c**t*(*A*<sub>0</sub>, *B*<sub>0</sub>) − *C*<sub>0</sub> − *D*<sub>0</sub>

``` r
sets <- 4
parameters <- list(c(0.65, 0.47, 0.35, 0.20,  45),
                   c(0.35, 0.47, 0.35, 0.20, 135),
                   c(0.50, 0.57, 0.33, 0.15,  45),
                   c(0.50, 0.57, 0.35, 0.15, 135))
ellipses <- lapply(parameters,function(x){
  do.call(ell2poly,as.list(x)) %>% 
    data.frame() %>% 
    mutate(x=round(x,4),y=round(y,4))
})

polygons <- lapply(ellipses,function(x)st_polygon(list(as.matrix(x))))
```

使用这个方法，找到了A的区域。

``` r
par(mfrow=c(2,2))
A <- st_difference(st_difference(st_difference(polygons[[1]],polygons[[2]]),polygons[[3]]),polygons[[4]])
plot(A,main="A")

ABCD <- st_intersection(st_intersection(st_intersection(polygons[[1]],polygons[[2]]),polygons[[3]]),polygons[[4]])
plot(ABCD,main="ABCD")

ABC <- st_difference(st_intersection(st_intersection(polygons[[1]],polygons[[2]]),polygons[[3]]),polygons[[4]])
plot(ABC,main = "ABC")

AB <- st_difference(st_intersection(polygons[[1]],polygons[[2]]),st_union(polygons[[3]],polygons[[4]]))
plot(AB, main = "AB")
```

![](materials_files/figure-markdown_github/unnamed-chunk-23-1.png)

将 `Grid` 的 `polygons` 转变成 `ggplot` 的 `polygon`
----------------------------------------------------

`polygons` 其实是多个 `polygon` 组成的列表。

``` r
typeof(AB)
```

    ## [1] "list"

``` r
class(AB)
```

    ## [1] "XY"      "POLYGON" "sfg"

与此同时，它又是一个 XY 坐标构成的向量。所以可以通过以下几步把 polygons
转变成 data.frame 数据。

``` r
M <- unlist(AB) %>% matrix(ncol=2)
colnames(M) <- c("x","y")
df <- data.frame(M)
head(df)
```

    ##        x       y
    ## 1 0.5000 0.18505
    ## 2 0.4979 0.18510
    ## 3 0.4937 0.18530
    ## 4 0.4896 0.18560
    ## 5 0.4855 0.18590
    ## 6 0.4814 0.18640

``` r
ggplot(df,aes(x,y)) + geom_polygon(fill="firebrick")
```

![](materials_files/figure-markdown_github/unnamed-chunk-25-1.png)

加上中心点。

``` r
center <- st_centroid(AB)
typeof(center)
```

    ## [1] "double"

``` r
class(center)
```

    ## [1] "XY"    "POINT" "sfg"

``` r
# 转化为 data.frame
center_df <- matrix(center,ncol = 2) %>% data.frame()
colnames(center_df) <- c("x","y")
```

中心点也是 `XY` 对象，不过类型不是 list，而是 double。

``` r
ggplot(df,aes(x,y)) + geom_polygon(fill="firebrick") +
  geom_point(data = center_df, size=4, color="white") + 
  geom_label(data = center_df, label = "AB\n(50%)",alpha=0.5, label.size = NA)
```

![](materials_files/figure-markdown_github/unnamed-chunk-27-1.png)

upsetR 计算各个区域的成员
-------------------------

UpsetR 可以绘制任意多列表的交叉引用信息。

``` r
df <-  data.frame(id=1:100,GroupA=sample(c("A1","A2","A3"),100,T),GroupB=sample(c("B1","B2","B3"),100,T))
count_by_multi_group <- df %>% group_by(GroupA,GroupB) %>% summarise(count=n())
expressionInput <- count_by_multi_group$count
names(expressionInput) <- apply(count_by_multi_group, 1, FUN = function(x)paste(x[1:2],collapse = "&"))

library(UpSetR)
upset(fromExpression(expressionInput), order.by = "freq", nsets=6)
```

![](materials_files/figure-markdown_github/unnamed-chunk-28-1.png)

开始确定参数
------------

``` r
library(sf)
A <- st_difference(st_difference(st_difference(polygons[[1]],polygons[[2]]),polygons[[3]]),polygons[[4]])
B <- st_difference(st_difference(st_difference(polygons[[2]],polygons[[1]]),polygons[[3]]),polygons[[4]])
C <- st_difference(st_difference(st_difference(polygons[[3]],polygons[[1]]),polygons[[2]]),polygons[[4]])
D <- st_difference(st_difference(st_difference(polygons[[4]],polygons[[1]]),polygons[[3]]),polygons[[2]])
AB <- st_difference(st_intersection(polygons[[1]],polygons[[2]]),st_union(polygons[[3]],polygons[[4]]))
AC <- st_difference(st_intersection(polygons[[1]],polygons[[3]]),st_union(polygons[[2]],polygons[[4]]))
AD <- st_difference(st_intersection(polygons[[1]],polygons[[4]]),st_union(polygons[[3]],polygons[[2]]))
BC <- st_difference(st_intersection(polygons[[3]],polygons[[2]]),st_union(polygons[[1]],polygons[[4]]))
BD <- st_difference(st_intersection(polygons[[4]],polygons[[2]]),st_union(polygons[[3]],polygons[[1]]))
CD <- st_difference(st_intersection(polygons[[3]],polygons[[4]]),st_union(polygons[[1]],polygons[[2]]))
ABC <- st_difference(st_intersection(st_intersection(polygons[[1]],polygons[[2]]),polygons[[3]]),polygons[[4]])
ABD <- st_difference(st_intersection(st_intersection(polygons[[1]],polygons[[2]]),polygons[[4]]),polygons[[3]])
ACD <- st_difference(st_intersection(st_intersection(polygons[[1]],polygons[[4]]),polygons[[3]]),polygons[[2]])
BCD <- st_difference(st_intersection(st_intersection(polygons[[4]],polygons[[2]]),polygons[[3]]),polygons[[1]])
ABCD <- st_intersection(st_intersection(st_intersection(polygons[[1]],polygons[[2]]),polygons[[3]]),polygons[[4]])
```

计算多边形的参数。

``` r
ggpolygons <- list(A=A,B=B,C=C,D=D,AB=AB,AC=AC,AD=AD,BC=BC,BD=BD,CD=CD,ABC=ABC,ABD=ABD,ACD=ACD,BCD=BCD,ABCD=ABCD)
polygon_names <- names(ggpolygons)
ggpolygons_df <- lapply(1:length(ggpolygons), function(i){
  df <- unlist(ggpolygons[[i]]) %>% matrix(ncol = 2) %>% data.frame()
  colnames(df) <- c("x","y")
  df$group <- polygon_names[[i]]
  return(df)
})
data_ploygons <- do.call(rbind,ggpolygons_df)
```

计算多边形中心点。

``` r
center_df <- lapply(ggpolygons, st_centroid) %>% unlist %>% matrix(byrow = T,ncol=2) %>% data.frame()
center_df$group <- polygon_names
colnames(center_df) <- c("x","y","group")
data_centers <- center_df
```

查看一下结果：

``` r
ggplot(data_ploygons,aes(x,y,fill=group)) +
  geom_polygon(show.legend = F) +
  geom_text(aes(label=group),data=data_centers) +
  coord_equal() +
  ggtree::theme_tree()
```

![](materials_files/figure-markdown_github/unnamed-chunk-32-1.png)

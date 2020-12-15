library(reshape2)
library(tidyr)

(messy <- matrix(rnorm(8),ncol=2,dimnames = list(LETTERS[1:4],letters[1:2])))
(messy.df <- as.data.frame(messy) %>% tibble::rownames_to_column())

#' gather只能用于data.frame，
#' melt是S3函数，支持data.frame，array，matrix和table，list等不同类型。
#' 而且，melt对默认参数的设定比较智能化，通常不需要写参数即可使用。
gather(messy.df,key,value,-rowname)
# gather(messy) # 会出错

#' 值得注意的是，不管是`tidyr::gather`还是`reshape2::melt`,
#' 它们输出的数据都是data.frame。
# 这里可以看到，melt的返回也都是data.frame
(a <- melt(messy.df))
(b <- melt(messy))
class(a)
class(b)

#' 将数据变回去时，分别对应两种做法。
#' 先看`spread()`:
spread(a,variable,value)
spread(b,Var2,value)
#' reshape2中可以使用`dcast()`和`acast()`将data.frame变回去。
# dcast() 返回data.frame
dcast(a,rowname~variable)
dcast(b,Var1~Var2)
# acast()返回vector/matrix/array
acast(a,rowname~variable)
acast(b,Var1~Var2)



#' 我发现只要 dat 的取值是连续数字，
#' 去掉任一个元素（设为NA）都不会影响 dist 的数值。


# 将任一元素设为NA，再计算distance
distance_one_na <- function(x){
  dist <- vector("list",length(x))
  for (i in seq_along(x)) {
    dd <- x
    dd[[i]] <- NA
    dat <- matrix(dd, 2)
    dist[[i]] <- as.numeric(dist(dat))
  }
  a <- unlist(dist)
  if (var(a) == 0){
    return(unique(a))
  }
  else{
    return(a)
  }
}

#' 以100以内的数值为例，可以得出：

nums <- seq(4,10000,by = 2)
dist <- vector("list",length(nums))
dist_na <- vector("list",length(nums))
for (i in seq_along(nums)){
  d <- seq(1,nums[[i]])
  mat <- matrix(d,2)
  dist[[i]] <- dist(mat)
  dist_na[[i]] <- distance_one_na(d)
}
identical(do.call("rbind",dist),do.call("rbind",dist_na))

#' 不过，如果是随机生成的数值，则不会出现这种情况（虽然也会有数值相同的情况）：
set.seed(1)
x <- runif(10)
distance_one_na(x)


#' 如此说来，欧几里得距离对于自然数组成的矩阵缺失单个数值真的不敏感。
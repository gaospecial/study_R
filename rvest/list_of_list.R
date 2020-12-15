a <- list(id=1,title="great idea",abstract="First,\n;Second:\nand then do it.")
a
write.csv(a,file = "a.csv")
write.csv(a,file = "a.csv",row.names = F)
aaa <- list(a,a,a)
b <- do.call("rbind",aaa)
as_tibble(b)
as_tibble(data.frame(b))
(a_safe <- lapply(a, function(x)gsub("\\s"," ",x)))
(aaa_safe <- lapply(aaa,function(a)lapply(a,function(x)gsub("\\s"," ",x))))

#' 使用`unlist()`把`list`转变成一般向量。向量的名称可以重复。
#' 有一个重要的参数“recursive”。
unlist(aaa,recursive = F)
(c <- unlist(aaa_safe))
names(c)

#' 但是使用名称只能访问第一个元素
c["id"]
c["id"] <- "id1"
c

names(c) <- NULL

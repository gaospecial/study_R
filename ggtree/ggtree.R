# ggtree实例
# 对土壤分离株进行绘图
##### 安装ggtree
# BiocInstaller::biocLite("ggtree")
library(ggtree)
setwd("H:/Spring_Work/35. Data/2018-4-3 ggtree")

# 读取MEGA6输出的newick树
tree <- read.tree(file = "isolates_maximum-tree.nwk")
tree

# tax文件是一个包含分类信息的文本文件
tax <- read.delim("isolates_classification.txt",as.is = TRUE)
rownames(tax) <- tax$sequence_identifier

tree$tip.label <- tax[tree$tip.label,"full_name"]

p <- ggtree(tree,size=1)+geom_tiplab(align = TRUE,offset = 0.01,fontface="italic")+xlim(0,0.5)


# 使用rownames作为grouping，多出来一个0
groupinfo <- split(rownames(tax),tax$phylum)
tree <- groupOTU(tree,groupinfo)
ggtree(tree,aes(color=group))+geom_tiplab(align = TRUE)+xlim(0,0.5)+theme(legend.position = "right")+geom_label(aes(label=node))

ggtree(tree,aes(color=group))+geom_tiplab(align = TRUE)+xlim(0,0.5)+
  theme(legend.position = "right")+
  geom_hilight(27,fill = "firebrick",alpha = .5) + 
  geom_cladelabel(27,"Proteobacteria",fontsize = 6,angle = 90, offset.text = 0.05)



# groupOTU usage troubleshooting
###################################

# Use tax$sequence_identifier as grouping vector，多出来一个0，分类也不正确
groupinfo1 <- split(tax$sequence_identifier,tax$class)
tree1 <- groupOTU(tree,groupinfo1)
p1 <- ggtree(tree1,aes(color=group))+geom_tiplab()+xlim(0,0.5)+theme(legend.position = "right")

# 使用rownames作为grouping，多出来一个0
groupinfo2 <- split(rownames(tax),tax$class)
tree2 <- groupOTU(tree,groupinfo2)
p2 <- ggtree(tree2,aes(color=group))+geom_tiplab()+xlim(0,0.5)+theme(legend.position = "right")

# 使用vector id作为grouping，多出来一个0
groupinfo3 <- split(as.vector(tax$sequence_identifier),tax$class)
tree3 <- groupOTU(tree,groupinfo3)
p3 <- ggtree(tree3,aes(color=group))+geom_tiplab()+xlim(0,0.5)+theme(legend.position = "right")

# 使用tree tip_label作为grouping，分类信息显然是错的，但是不会多出来一个0.
# tree tip_label和rownames(tax)的class都是character，长度也一样。但是分组结果有以上不同，不知道是什么原因。
groupinfo4 <- split(tree$tip.label,tax$class)
tree4 <- groupOTU(tree,groupinfo4)
p4 <- ggtree(tree4,aes(color=group))+geom_tiplab()+xlim(0,0.5)+theme(legend.position = "right")


multiplot(p1,p2,p3,p4,ncol=4)


# 随机树按分类着色
###################################

# 生成一个随机树和分类信息
set.seed(0)
tr <- rtree(24)
d <- data.frame(name = sample(tr$tip.label), phylum = sample(LETTERS[1:3],Ntip(tr),replace = TRUE))
rownames(d) <- d$name
d
# 这里使用tree tip.label作为第一个参数则分类个数正常，但是分类错误
g1 <- split(tr$tip.label,d$phylum)
tr1 <- groupOTU(tr, g1)
ggtree(tr1,aes(color=group),size=1) + geom_tiplab() + theme(legend.position = 'right')

# 直接使用tax中的id则分类出错，为什么？
g2 <- split(d$name,d$phylum)
tr2 <- groupOTU(tr, g2)
ggtree(tr2,aes(color=group),size=1) + geom_tiplab() + theme(legend.position = 'right')

# 使用vector则分类正确
g3 <- split(as.vector(d$name),d$phylum)
tr3 <- groupOTU(tr, g3)
ggtree(tr3,aes(color=group),size=1) + geom_tiplab() + theme(legend.position = 'right')

# 使用rownames则分类正确
g4 <- split(rownames(d),d$phylum)
tr4 <- groupOTU(tr, g4)
ggtree(tr4,aes(color=group),size=1) + geom_tiplab() + theme(legend.position = 'right')






##### 改变ggtree绘图的重要参数
# tiplab右对齐
ggtree(tree,aes(color=group))+geom_tiplab(align = TRUE)+theme(legend.position = 'right')

# 调整label溢出
ggtree(tree,aes(color=group))+geom_tiplab(align = TRUE,fontface="italic")+xlim(NA,0.5)+theme(legend.position = 'right')

# 加粗线条和字体
ggtree(tree,aes(color=group),size=1)+geom_tiplab(align = TRUE,fontface="bold")+xlim(NA,0.5)

# 环形布局
ggtree(tree,aes(color=group),size=1,layout="fan")+
  geom_tiplab2(align = TRUE,fontface="bold")+
  theme(legend.position = 'right') + 
  xlim(0,0.7)



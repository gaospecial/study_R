# 安装

[详细安装流程点这里](https://benjjneb.github.io/dada2/dada-installation.html)


```
## 安装依赖
source("http://bioconductor.org/biocLite.R")
biocLite(suppressUpdates = FALSE)
biocLite("ShortRead", suppressUpdates = FALSE)

## 安装DADA2
library("devtools")
devtools::install_github("benjjneb/dada2")
```

# 开始使用 DADA2 

## 使用的DADA2版本

```
library(dada2);
packageVersion("dada2")
```

本文档使用的是1.9.0版本。

## 导入测序结果

将测序文件放到一个文件夹中。


```
## path是测序文件所在的文件夹
## 根据实际情况设置
path <- "/path/to/rawdata"
## 查看文件列表
list.files(path)
```

根据文件名匹配，分配文件到正向和反向测序结果。

```
## 我的文件命名是runid-sample_R1.fq.gz
fnFs <- sort(list.files(path,pattern = "_R1.fq.gz",full.names = TRUE))
fnRs <- sort(list.files(path,pattern = "_R2.fq.gz",full.names = TRUE))
## 正则表达式"[-_]"：将文件名在"-"或者"_"处分开，取第二个部分作为样品名
sample.names <- sapply(strsplit(basename(fnFs),"[-_]"),`[`,2)
```

## 查看测序质量

图中每个位置上测序质量的分布以灰度热图显示，黑色表示高频率。线条是统计值，其中绿色的平均值，橙色是中位数，橙色虚线是25%和75%分位数。

默认设置下，将读取50万条reads计算得到上述结果。

```
## 查看前两个正/反向测序结果的质量
plotQualityProfile(fnFs[1:2])
plotQualityProfile(fnRs[1:2])
```

## 过滤

几个关键参数的说明：
- truncLen
- maxN
- maxEE
- truncQ
- rm.phix
- compress
- multithread

```
## 指定输出文件
filtFs <- file.path(path,"filtered",paste0(sample.names,"_F_filt.fastq.gz"))
filtRs <- file.path(path,"filtered",paste0(sample.names,"_R_filt.fastq.gz"))

## 运行过滤程序
out <- filterAndTrim(fnFs,filtFs,fnRs,filtRs,truncLen = c(230,150),maxN = 0,maxEE = 2,truncQ = 2,rm.phix = FALSE,compress = TRUE,multithread = FALSE) # multithread在Windows下面不支持
```
## 学习错误率

`plotErrors`绘制每碱基观测错误率随测序质量的变化情况。

```
errF <- learnErrors(filtFs, multithread=TRUE)
errR <- learnErrors(filtRs, multithread=TRUE)
## 可视化Error rates
plotErrors(errF, nominalQ=TRUE)

```

图中，灰色点是对应测序质量下错误率的观测值；黑色线是机器学习程序收敛后估计的错误率；红色线是Q-score名义上的错误率。

## 去重复


```
derepFs <- derepFastq(filtFs, verbose=TRUE)
derepRs <- derepFastq(filtRs, verbose=TRUE)
## Name the derep-class objects by the sample names
names(derepFs) <- sample.names
names(derepRs) <- sample.names
```

## 推断


```
dadaFs <- dada(derepFs, err=errF, multithread=TRUE)
dadaRs <- dada(derepRs, err=errR, multithread=TRUE)

```

## 合并序列


```
mergers <- mergePairs(dadaFs, derepFs, dadaRs, derepRs, verbose=TRUE)

```

## Sequence Table


```
seqtab <- makeSequenceTable(mergers)
dim(seqtab)

```

## 去除嵌合体


```
## 去除嵌合体
seqtab.nochim <- removeBimeraDenovo(seqtab, method="consensus", multithread=TRUE, verbose=TRUE)
dim(seqtab.nochim)

## 统计去除嵌合体后序列所占比例
sum(seqtab.nochim)/sum(seqtab)

```

## 观察序列变化


```
getN <- function(x) sum(getUniques(x))
track <- cbind(out, sapply(dadaFs, getN), sapply(dadaRs, getN), sapply(mergers, getN), rowSums(seqtab.nochim))
# If processing a single sample, remove the sapply calls: e.g. replace sapply(dadaFs, getN) with getN(dadaFs)
colnames(track) <- c("input", "filtered", "denoisedF", "denoisedR", "merged", "nonchim")
rownames(track) <- sample.names
head(track)
```

下面是一个典型的结果。

```
## input filtered denoisedF denoisedR merged nonchim
## NP1    42964    38111     38095     38094  38080   38080
## NP2    45687    40386     40373     40356  40349   40349
## NP3    44151    38784     38780     38779  38775   38775
## NP4    47441    42750     42747     42742  42739   42723
## NP5    45763    40212     40199     40200  40190   40190
## NP6    48357    42785     42779     42753  42750   42750
## NP7    41723    36571     36549     36555  36535   36535
## NP8    45743    40162     40145     40151  40138   40138
```

## 获取物种分类

DADA2维护了包括 [Silva, RDP, GreenGenes和UNITE](https://benjjneb.github.io/dada2/training.html) 的物种数据库。其中UNITE是真菌ITS数据库。只需要从上述链接下载即可。

```
taxa <- assignTaxonomy(seqtab.nochim, "~/tax/silva_nr_v128_train_set.fa.gz", multithread=TRUE)

```

当分配到物种时，将使用100%匹配。


```
taxa <- addSpecies(taxa, "~/tax/silva_species_assignment_v128.fa.gz")

```

## 评估实验准确性

使用Mock组评估准确性。


```
unqs.mock <- seqtab.nochim["Mock",]
unqs.mock <- sort(unqs.mock[unqs.mock>0], decreasing=TRUE) # Drop ASVs absent in the Mock
cat("DADA2 inferred", length(unqs.mock), "sample sequences present in the Mock community.\n")

## DADA2 inferred 20 sample sequences present in the Mock community.

mock.ref <- getSequences(file.path(path, "HMP_MOCK.v35.fasta"))
match.ref <- sum(sapply(names(unqs.mock), function(x) any(grepl(x, mock.ref))))
cat("Of those,", sum(match.ref), "were exact matches to the expected reference sequences.\n")

## Of those, 20 were exact matches to the expected reference sequences.
```

至此，DADA2已经完成了它的使命，接下来可以转换到另外的分析工具了。

下面以phyloseq为例展示一下下游分析。

# 使用 phyloseq


安装方法不再赘述。


```
library(phyloseq); packageVersion("phyloseq")

## 1.23.1 

library(ggplot2); packageVersion("ggplot2")

## 2.2.1
```

设置一个ggplot主题

```
## 设置一个ggplot主题
## 每次使用一个自定义主题是个不错的想法
theme_set(theme_bw())

```

整合 meta 信息，seqtab和分类信息为一个 phyloseq 对象。


```
## 使用一个meta.txt文件存放实验的meta信息
meta <- read.delim("meta.txt", TRUE)
## 将meta的rownames指定为样品名称
## 此样品名与DADA2分析中的sample.names对应
rownames(meta) <- meta$id
```

现在可以通过下面的命令，完成这一操作。下面的参数并非必须参数。


```
ps <- phyloseq(
            otu_table(seqtab.nochim,taxa_are_rows = FALSE),
            sample_data(meta),
            tax_table(taxa))
            
## 去掉 Mock 组
ps <- prune_samples(sample_names(ps) != "Mock", ps)

```

## α多样性分析


```
## Day 和 When 都是样品的 meta 信息
plot_richness(ps, x="Day", measures=c("Shannon", "Simpson"), color="When")

```

## 标度分析


```
# Transform data to proportions as appropriate for Bray-Curtis distances
ps.prop <- transform_sample_counts(ps, function(otu) otu/sum(otu))

## NMDS 分析
ord.nmds.bray <- ordinate(ps.prop, method="NMDS", distance="bray")

## NMDS 结果可视化
plot_ordination(ps.prop, ord.nmds.bray, color="When", title="Bray NMDS")

```
## 样品群落结构分析

这里展示top20菌群的分类信息。

``` r
top20 <- names(sort(taxa_sums(ps), decreasing=TRUE))[1:20]
ps.top20 <- transform_sample_counts(ps, function(OTU) OTU/sum(OTU))
ps.top20 <- prune_taxa(top20, ps.top20)
plot_bar(ps.top20, x="Day", fill="Family") + facet_wrap(~When, scales="free_x")
```



# 参考资料

1. 这篇文章在上述操作之外，还展示了树、网络、差异分析等的方法，强烈推荐：https://f1000research.com/articles/5-1492
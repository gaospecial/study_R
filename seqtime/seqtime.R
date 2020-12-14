#' ---
#' title: seqtime - time series generated with different ecological models
#' author: "gaospecial@gmail.com"
#' output: html_document
#' date: 2018-11-28
#' ---
#' 

library(seqtime)

N <- 2
A <- generateA(N, c=0.5, d=-0.5)

A <- modifyA(A, perc = 70, strength = "uniform", mode = "negpercent")

y <- round(generateAbundances(N, mode = 5))
names(y) <- c(1:length(y))
barplot(y,main="Initial species abundances",xlab="Species",ylab="Abundance")

# convert initial abundances in proportions (y/sum(y)) and run without a noise term (sigma=-1)
out.ricker=ricker(N,A=A,y=(y/sum(y)),K=rep(0.1,N), sigma=-1,tend=500)
tsplot(out.ricker,main="Ricker")

ricker.noise=identifyNoisetypes(out.ricker, smooth=TRUE)


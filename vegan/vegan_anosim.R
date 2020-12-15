## 与卜容燕交流

library(vegan)

data(dune)
data(dune.env)

## anosim example
dune.dist <- vegdist(dune)
dune.ano <- with(dune.env, anosim(dune.dist, Management))
summary(dune.ano)
plot(dune.ano)

## adonis2 examples
## default test by terms
adonis2(dune ~ Management*A1, data = dune.env)
## overall tests
adonis2(dune ~ Management*A1, data = dune.env, by = NULL)

### Example of use with strata, for nested (e.g., block) designs.
dat <- expand.grid(rep=gl(2,1), NO3=factor(c(0,10)),field=gl(3,1) )
dat
Agropyron <- with(dat, as.numeric(field) + as.numeric(NO3)+2) +rnorm(12)/2
Schizachyrium <- with(dat, as.numeric(field) - as.numeric(NO3)+2) +rnorm(12)/2
total <- Agropyron + Schizachyrium
dotplot(total ~ NO3, dat, jitter.x=TRUE, groups=field,
        type=c('p','a'), xlab="NO3", auto.key=list(columns=3, lines=TRUE) )

Y <- data.frame(Agropyron, Schizachyrium)
mod <- metaMDS(Y, trace = FALSE)
plot(mod)
### Ellipsoid hulls show treatment
with(dat, ordiellipse(mod, field, kind = "ehull", label = TRUE))
### Spider shows fields
with(dat, ordispider(mod, field, lty=3, col="red"))

### Incorrect (no strata)
perm <- how(nperm = 199)
adonis2 (Y ~ NO3, data = dat, permutations = perm)

## Correct with strata
setBlocks(perm) <- with(dat, field)
adonis2(Y ~ NO3, data = dat, permutations = perm)


## 
adonis(dune~Management*A1,data = dune.env)
adonis(dune~A1*Management,data = dune.env)
adonis(dune~A1*Management,data = dune.env,by=NULL)
adonis(dune~Management*A1,data = dune.env,by=NULL)

adonis2(dune~Management*A1,data = dune.env)
adonis2(dune~A1*Management,data = dune.env)
adonis2(dune~Management*A1,data = dune.env,by="margin")
adonis2(dune~A1*Management,data = dune.env,by="margin")
adonis2(dune~A1*Management,data = dune.env,by=NULL)
adonis2(dune~Management*A1,data = dune.env,by=NULL)

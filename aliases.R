rm.na <- function(v) v[!is.na(v)]
a.n <- as.numeric
a.m <- as.matrix
a.df <- as.data.frame
a.f <- as.factor

h <- utils::head
tl <- utils::tail
sm <- summary
cd <- setwd
pwd <- getwd
lss <- dir

#From stack overflow question on .Rprofile
#Use last(x) instead of x[length(x)], works on matrices too
#last <- function(x) { tail(x, n = 1) }
last <- function(x) UseMethod("last",x)

last.list <- function(x) { x[[length(x)]] }
last.vector <- function(x) { tail(x, n = 1) }
last.numeric <- function(x) { tail(x, n = 1) }
last.double <- function(x) { tail(x, n = 1) }
last.matrix <- function(x) { tail(x, n = 1) }
last.data.frame <- function(x) { tail(x, n = 1) }

i.p <- function(pkg,...) install.packages(pkg,dep=T,...)

install.all <- function()
  {
    packs <- c('ggplot2','mgcv','fields','mvtnorm','SamplerCompare','statmod','MCMCpack','gsl','numDeriv','truncnorm','glmnet','spatstat','yaImpute','readImage','png','stringr','akima','R.matlab','gridExtra','formula.tools','profr','maxLik')
    i.p(packs)
    install.inla()
    install.EBImage()
  }

install.inla <- function(testing=T)
  {
    i.p('pixmap')
    source("http://www.math.ntnu.no/inla/givemeINLA.R")
    inla.upgrade(testing)
  }

install.EBImage <- function()
  {
    source("http://bioconductor.org/biocLite.R")
    biocLite("EBImage")
  }


install.ggplot2.dev <- function()
  {
    library(devtools)
    dev_mode()
    install_github("ggplot2") 

  }

seqdef <- seq(-3,3,l=100)

ssqrt <- function(s) sign(s)*sqrt(abs(s))
sdiag <- function(v) Diagonal(length(v),v)

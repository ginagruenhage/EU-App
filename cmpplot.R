#Comparison Plots
#Some visual tools to compare what you got and what you should be getting
#Simon Barthelmé (University of Geneva), 2012


cmpplot <- function(x,...) UseMethod("cmpplot",x)

cmpplot.matrix <- function(A,B,flatten=F,type=1,...)
  {
    name.a <- deparse(substitute(A))
    name.b <- deparse(substitute(B))

    if (flatten)
      {
        cmpplot.numeric(as.numeric(A),as.numeric(B),type=type,...)
      }
    else
      {
        if (type == 1)
          {
            A <- Matrix(A)
            B <- Matrix(B)
            ttl <- sprintf("Absolute difference between %s and %s",name.a,name.b)
            image(abs(A-B),sub=ttl,colorkey=T)
          }
        else if (type == 2)
          {
            if (is.symmetric(A) & is.symmetric(B))
              {
                sym <- T
                vA <- eigen(A)$val
                vB <- eigen(B)$val
                ylab <- "Eigenvalue"
              }
            else
              {
                sym <- F
                vA <- svd(A)$d
                vB <- svd(B)$d
                ylab <- "Singular value"
              }
            ttl <- sprintf('Spectrum of %s vs. spectrum of %s',name.a,name.b)
            ind <- 1:length(vA)
            ylim <- range(c(vA,vB))
            plot(ind,vA,ylim=ylim,pch=20,xlab="Index",ylab=ylab,main=ttl)
            points(ind,vB,pch=3,col="red")
            legend("topright",c(name.a,name.b),pch=c(20,3),col=c('black','blue'),fill="white")
          }
      }
  }

cmpplot.numeric <- function(a,b,type=1,stretch=F)
  {
    name.a <- deparse(substitute(a))
    name.b <- deparse(substitute(b))
    if (type==1)
      {
        if (stretch)
          {
            xlim <- range(c(a,b))
            ylim <- xlim
          }
        else
          {
            xlim <- range(a)
            ylim <- range(b)
          }
        
        
        plot(a,b,xlab=name.a,ylab=name.b,xlim=xlim,ylim=ylim,col="blue",pch=20)
        abline(0,1)
        m <- lm(b ~ a)
        msg <- sprintf('Regression line: y = %.2f * x + %.2f',coef(m)[2],coef(m)[1])
        mtext(msg)
      }
    else if (type == 2)
      {
        ylim <- range(c(a,b))
        plot(a,xlab="Index",ylab="Value",pch=20,ylim=ylim)
        pointsb(b,pch=3)
        legend("topright",c(name.a,name.b),pch=c(20,3),col=c('black','blue'),bg="white")

      }
    else if (type == 3)
      {
        a <- rank(a);b <- rank(b);
        name.a <- paste(name.a,"(rank)")
        name.b <- paste(name.b,"(rank)")
        plot(a,b,xlab=name.a,ylab=name.b)
        abline(0,1)
      }

  }

is.symmetric <- function(A)
  {
    if (nrow(A) == ncol(A))
      {
        all(A==t(A))
      }
    else
      {
        FALSE
      }
  }

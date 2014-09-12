#Various utility functions 

#require(rimage)
#require(ggplot2)
#require(e1071)
#require(mgcv)

mycols = c(rgb(31,119, 180,max=255), rgb( 255,127,14,max=255), rgb( 44, 160, 44,max=255),rgb(214, 39,40,max=255),rgb(148,103,189,max=255),rgb( 140,86,75,max=255),rgb(227,119,194,max=255),rgb(127,127,127,max=255),rgb( 188,189,34,max=255),rgb(23,190,207,max=255))


theme_cmds_2d <- theme_classic() + theme( text = element_text(family="serif",size=7), axis.line = element_blank(),panel.border = element_rect(linetype = "solid",fill=NA, colour = "black"),legend.position="bottom",legend.key=element_rect(fill="white",colour="white"),legend.margin = unit(0,"cm"),plot.margin=unit(c(0,0,0,0),"cm"),strip.background=element_blank(),strip.text.x=element_blank()) 

theme_cmds_1d <- theme_classic() + theme( text = element_text(family="serif",size=7), axis.line = element_blank(), panel.border = element_rect(linetype = "solid", fill=NA, colour = "black"), plot.margin=unit(c(0,0,0,0),"cm"))

source("/home/gina/EigeneDateien/Work/PHD/utility_functions/cmpplot.R")

## Show the first 5 rows and first 5 columns of a data frame or matrix
hh <- function(d) d[1:5,1:5]

## Paste without a separator.
glue <- function(...) paste(...,sep="")

##A better seq
seql <- function(a,b,l=100) seq(a,b,l=l)

#From ?%in%
"%w/o%" <- function(x, y) x[!x %in% y] #--  x without y



# improved list of objects (from stack overflow)
.ls.objects <- function (pos = 1, pattern, order.by,
                        decreasing=FALSE, head=FALSE, n=5) {
    napply <- function(names, fn) sapply(names, function(x)
                                         fn(get(x, pos = pos)))
    names <- ls(pos = pos, pattern = pattern)
    obj.class <- napply(names, function(x) as.character(class(x))[1])
    obj.mode <- napply(names, mode)
    obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
    obj.prettysize <- napply(names, function(x) {
                           capture.output(print(object.size(x), units = "auto")) })
    obj.size <- napply(names, object.size)
    obj.dim <- t(napply(names, function(x)
                        as.numeric(dim(x))[1:2]))
    vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
    obj.dim[vec, 1] <- napply(names, length)[vec]
    out <- data.frame(obj.type, obj.size, obj.prettysize, obj.dim)
    names(out) <- c("Type", "Size", "PrettySize", "Rows", "Columns")
    if (!missing(order.by))
        out <- out[order(out[[order.by]], decreasing=decreasing), ]
    if (head)
        out <- head(out, n)
    out
}


.ls.envir <- function (envir, pattern, order.by,
                        decreasing=FALSE, head=FALSE, n=5) {
    napply <- function(names, fn) sapply(names, function(x)
                                         fn(get(x, envir=envir)))
    names <- ls(envir, pattern = pattern)
    obj.class <- napply(names, function(x) as.character(class(x))[1])
    obj.mode <- napply(names, mode)
    obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
    obj.prettysize <- napply(names, function(x) {
                           capture.output(print(object.size(x), units = "auto")) })
    obj.size <- napply(names, object.size)
    obj.dim <- t(napply(names, function(x)
                        as.numeric(dim(x))[1:2]))
    vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
    obj.dim[vec, 1] <- napply(names, length)[vec]
    out <- data.frame(obj.type, obj.size, obj.prettysize, obj.dim)
    names(out) <- c("Type", "Size", "PrettySize", "Rows", "Columns")
    if (!missing(order.by))
        out <- out[order(out[[order.by]], decreasing=decreasing), ]
    if (head)
        out <- head(out, n)
    out
}


# shorthand
lsos <- function(..., n=10) {
    .ls.objects(..., order.by="Size", decreasing=TRUE, head=TRUE, n=n)
}

plot.symbols <- function()
  {
    plot(1:25,1:25,pch=1:25)
  }

seql <- function(a,b,l) seq(a,b,l=l)
seq.in.range <- function(v,l=100)
  {
    seq(min(v),max(v),l=l)
  }

seq_in_range <- seq.in.range

plotfun.grid <- function(fun,x=seqdef,y=seqdef)
  {
    gr <- expand.grid(xval=x,yval=y)
    gr <- adply(gr,1,function(v) fun(a.n(v)))
    names(gr)[3] <- 'zval'
    ggplot(gr,aes(xval,yval,z=zval))+geom_tile(aes(fill=..z..))+geom_contour()
  }




#Compute a pairwise distance matrix between the rows of X
distance.matrix <- function(X)
  {
    X <- t(X) #Easier to work with columns
    norm2 <- colSums(X^2)
    D2 <- -2*t(X)%*%X + outer(norm2,rep(1,length(norm2))) + outer(rep(1,length(norm2)),norm2)
    diag(D2) <- 0
    sqrt(D2)
  }

#Compute a  guess for the lambda parameter of a Gaussian kernel, according to a heuristic given by Alex Smola here:
#http://blog.smola.org/post/940859888/easy-kernel-width-choice
svm.lambda.heuristic <- function(X)
  {
    ns <- min(dim(X)[1],1000)
    s <- sample(1:dim(X)[1],2*ns,rep=T)
    X1 <- X[s[1:ns],]
    X2 <- X[s[(ns+1):(2*ns)],]
    d <- sqrt(rowSums((X1-X2)^2))
    (1/quantile(d,c(.1,.5,.9)))^2
  }


svm.optlambda <- function(X,y,k=10)
  {
    if (length(y) != dim(X)[1])
      {
        stop('length(y) must be equal to the number of lines in X')
      }
      
    ls <- svm.lambda.heuristic(X)
    m <- list()
    for (ind in seq(along=ls))
      {
        m[[ind]] <- svm(X,as.factor(y),gamma=ls[ind],cross=k)
      }
    acc <- sapply(1:length(m),function(ind) m[[ind]]$tot.accuracy)
    best <- which.max(acc)
    list(m=m[[best]],acc=acc[[best]])
  }

aroc <- function(x,y,plot=F,integrate.interp=T)
  {
    beta <- seq(quantile(x,.001),quantile(x,.999),l=100)
    hits <- sapply(beta,function(b) sum((x>b)&(y==1))/sum(y==1))
    fa <- sapply(beta,function(b) sum((x>b)&(y!=1))/sum(y!=1))
    if (plot)
      {
        plot(fa,hits,xlim=c(0,1),ylim=c(0,1),type="l",col="blue")
        abline(0,1)
      }
    interp.failed <- F
    if (integrate.interp==T)
      {
        fun <- approxfun(x=c(1,fa,0),y=c(1,hits,0))
        val <- try(integrate(fun,0,1)$value,silent=T)
        if (!is.numeric(val))
          {
            interp.failed <- T
            warning('Integration via interpolation failed')
          }
            
      }
    if (integrate.interp==F|interp.failed)
      {
        val <- sum(-diff(fa)*filter(hits,c(1/2,1/2))[1:(length(hits)-1)])
      }
    val
  }


flatness.patch <- function(patch,bw=T,log=F)
  {
    patch <- patch/sd(patch)
    d <- if(bw) sqrt(length(patch)) else sqrt(length(patch)/3)
    gr <- expand.grid(x=1:d,y=1:d)
    if (!bw)
      {
        patch <- matrix(patch,length(patch)/3,3)
        patch <- colMeans(patch)
      }
    gr$z <- patch
    if (log)
      {
        gr$z <- evalq(log(z-min(z)+1),gr)
        m <- lm(z ~ x + y,dat=gr)
      }
    else
      {
        m <- lm(z ~ x + y,dat=gr)
      }

    sqrt(mean(m$residuals^2))
  }

#Assume that vec is a flattened square image, reshape and display it
imagevec <- function(vec,dx=sqrt(length(vec)),dy=sqrt(length(vec)),bw=T,byrow=T)
  {
    require(ReadImages)
    if (bw)
      {
        d <- sqrt(length(vec))
        M <- matrix(vec,dy,dx,byrow=F)
      }
    else
      {
        d <- sqrt(length(vec)/3)
        M <- array(vec,c(dy,dx,3))
      }
    im <- imagematrix(normalize(M))
    plot(im)
  }

center.rows <- function(M)
  {
    means <- rowMeans(M)
    sweep(M,1,means)
  }

#Subtract a vector v from all rows of M 
subtract.from.rows <- function(M,v)
  {
    sweep(M,2,v)
  }


#Some summary stats. for the pred. performance of a model for binary y
perf.summary <- function(m,X,y)
  {
    pred.m <- predict(m,data.frame(X))
    acc <- mean((pred.m > 0) == (y==1))
    lik <- mean(plogis(y*pred.m,log=T))
    roc <- aroc(pred.m,y)
    c(acc=acc,lik=lik,roc=roc)
  }

#Cross-validate a performance measure
cv.perf <- function(X,y,fit.fun,perf.fun=perf.summary,keep=.9,nrep=10)
  {
    n <- dim(X)[1]
    nk <- round(n*keep)
    X <- as.data.frame(X)
    fitandtest <- function()
      {
        ri <- order(runif(n))
        set.fit <- ri[1:nk]
        set.test <- ri[-(1:nk)]
        m <- fit.fun(X[set.fit,],y[set.fit])
        perf.fun(m,X[set.test,],y[set.test])
      }
    replicate(nrep,fitandtest())
  }
#Returns an orthonormal basis for the space orthogonal to span{A}
orth <- function(A)
  {
    s <- svd(A,nu=nrow(A))
    s$u[,-(1:ncol(A))]
  }

#Returns an orthonormal basis for null{A}
null <- function(A)
  {
    s <- svd(A,nv=ncol(A))
    s$v[,-(1:nrow(A))]
  }


#Returns an orthonormal basis for span{A}
span <- function(A)
  {
    s <- svd(A,nu=nrow(A))
    s$u[,(1:ncol(A))]
    
  }

#Returns a random posdef matrix of size n

rposdef <- function(n)
  {
    R <- matrix(rnorm(n^2),n,n)
    t(R)%*%R
  }

pointsr <- function(...)
  {
    points(...,col="red")
  }

pointsb <- function(...)
  {
    points(...,col="blue")
  }

pointsg <- function(...)
  {
    points(...,col="green")
  }


#Compute A\b, given U=chol(A)
cholsolve <- function(U,b)
  {
    backsolve(U,forwardsolve(t(U),b))
  }

#Same as colSums, without the checks (unsafe)
fcolSums <- function(A,na.rm=F)
  {
    dn <- dim(A)
    .Internal(colSums(A, nrow(A), ncol(A), na.rm))
  }

#For a vector v return exp(v)/sum(exp(v))
expnorm <- function(v)
  {
    m <- max(v)
    exp(m)*exp(v-m)/sum(exp(v))
  }

logdet <- function(K)
  {
    2*sum(log(diag(chol(K))))
  }

#Sometimes chud and chdd return matrices with negative diagonal entries, have to take absolute value
logdetchol <- function(U)
  {
    2*sum(log(abs(diag(U))))
  }

replace <- function(u,ind,v)
  {
    u[ind] <- v
    u
  }

repl <- replace


#Extract named elements in a list of lists
#SO: http://stackoverflow.com/questions/5935673/accessing-same-named-list-elements-of-the-list-of-lists-in-r/5936077#5936077
`%c%` <- function(x, n) sapply(x, `[[`, n)


#Sum all the elements in a list named "var"
sumel <- function(l,var)
  {
    Reduce('+',l %c% var)
  }

colQuantile <- function(M,p=.5)
  {
    aaply(M,2,quantile,p=p)
  }


grid.data <- function(x,xb)
  {
    nx <- length(xb)-1
    z <- 1:nx
    x.ind <- approx(xb,1:length(xb),x,method="constant")$y
    z[x.ind]
  }

bin.data <- function(z,nbins)
  {
    b <- seq(min(z)-.0001,max(z)+.001,l=nbins+1)
    bc <- filter(b,c(.5,.5))[-(nbins+1)]
    gz <- grid.data(z,b)
    attr(gz,"bin.centers") <- bc
    gz
  }
#Turn (x,y) data into indexed data 
#(x,y) must be stricly contained within the bounds 
grid.xydata <- function(x,y,xb,yb)
  {
    nx <- length(xb)-1
    ny <- length(yb)-1
    z <- matrix(1:(nx*ny),ny,nx)
    x.ind <- approx(xb,1:length(xb),x,method="constant")$y
    y.ind <- approx(yb,1:length(yb),y,method="constant")$y
    z[cbind(y.ind,x.ind)]
  }


colNorms <- function(X)
  {
    sqrt(colSums(X^2))
  }

#Scale X such that the columns have unit norm
scaleNorm <- function(X)
  {
    scale(X,center=F,scale=colNorms(X))
  }

#Effective sample size
ess <- function(w) (sum(w))^2/(sum(w^2))


#Check against numerical gradient
check.gradient <- function(fun,dfun,x,return.val=F)
  {
    require(numDeriv)
    d <- dfun(x)
    d.check <- grad(fun,x)
    plot(d,d.check)
    abline(0,1)
    if (return.val) list(h=h,h.num=h.check)
  }

check.hessian <- function(fun,hfun,x,return.val=F)
  {
    require(numDeriv)
    h <- hfun(x)
    h.check <- hessian(fun,x)
    plot(a.n(h),a.n(h.check))
    abline(0,1)
    if (return.val) list(h=h,h.num=h.check)
  }

#Mean eff. sample size
meaneff <- function(X)
  {
    effectiveSize(X)/nrow(X)
  }

dirac <- function(i,n)
  {
    v <- rep(0,n)
    v[i] <- 1
    v
  }

posdef <- function(R)
  {
    all(eigen(R)$val>0)
  }

egv <- function(R)
  {
    eigen(R)$val
  }

#Compute the integration constant for a MVN distribution under the exponential parameterisation, given U= chol(Q)
gaussz <- function(U,mu,ldet=NA)
  {
    if (is.na(ldet))
      {
        ldet <- logdetchol(U)
      }
    0.5*(-length(mu)*log(2*pi)-ldet+sum((U%*%mu)^2))
  }

gausszQ <- function(Q,mu)
  {
    a.n(0.5*(-length(mu)*log(2*pi)-log(det(Q))+t(mu)%*%Q%*%mu))
  }

gaussznonexp <- function(U)
  {
    a.n(.5*(log(2*pi)-logdetchol(U)))
  }



gghm.many <- function(x,y,zs,...)
  {
    np <- length(zs)
    ps <- llply(1:np,function(ind) gghm(x,y,zs[[ind]],...)+opts(title=names(zs)[ind]))
    do.call(grid.arrange,ps)
  }
  

gghm <- function(x,y,z,reg=F,superres=T,nres=50)
  {
    if (superres)
      {
        require(akima)
        df <- expand.grid(x=seq.in.range(x,50),y=seq.in.range(y,50))
        df$z <- interpp(x,y,z,df$x,df$y)$z
        reg <- T
      }
    else
      {
        df <- data.frame(x=x,y=y,z=z)
      }
    
    if (reg)
      {
        p <- ggplot(df,aes(x,y))+geom_raster(aes(fill=z),size=0,col=rgb(0,0,0,0))
      }
    else
      {
        p <- ggplot(df,aes(x,y))+geom_point(aes(col=z),pch=15,size=6)
      }
    p+scale_x_continuous(expand=c(0,0))+scale_y_continuous(expand=c(0,0))+guides(fill = guide_colorbar())
  }

eye <- function(n,sparse=F)
  {
    if (sparse)
      {
        sparseMatrix(i=1:n,j=1:n,x=rep(1,n))
      }
    else
      {
        diag(rep(1,n))
      }
  }

#Extract "random effects" summary from an inla object
reff <- function(res.inla)
  {
    r <- res.inla$summary.random
    for (ind in 1:length(r))
      {
        names(r[[ind]])[4:6] <- c('q.lower','median','q.upper')
      }
    r
  }

feff <- function(res.inla)
  {
    df <- as.data.frame(res.inla$summary.fixed)[,1:5]
    names(df)[3:5] <- c('q.lower','median','q.upper')
    df
  }


ggp <- function(...)
  {
    ggdefault(ggplot(...))
  }

ggdefault <- function(p)
  {
    p+theme_bw()+opts(panel.grid.minor = theme_blank(),panel.grid.major = theme_blank(),legend.position="none")
  }

#Workaround, peplaces ggsave when grid.arrange had to be used
pngprint <- function(fname,p,...)
  {
    png(fname,...)
    print(p)
    dev.off()
  }

anyna <- function(v) any(is.na(v))
anyweird <- function(v) any(!is.finite(v))

#Take a function that operates on vectors and make it operate row-wise
rowfun <- function(fun)
{
  function(A) apply(A,1,fun)
}

colfun <- function(fun)
{
  function(A) apply(A,2,fun)
}

pseudosolve <- function(A,b)
  {
    require(corpcor)
    pseudoinverse(A)%*%b
  }

#Add to a dataframe with a time index columns containing what happened previously, used typically to get predictors from the past (e.g. feedback from the previous trial).
#extract.var is a vector of variable names (the variables to extract)
#time.var is the name of the temporal variable
#shift is the time shift (this assumes that the time index is discrete)
#
#
#Example:
#df <- data.frame(trial=1:4,correct=c(T,F,F,T))
#past.predictors(df,time.var="trial",extract.var="correct")
#--> Now contains a new column, "correct.m1", which contains the value of correct on the previous trial

past.predictors <- function(df,extract.var="correct",time.var="trial",shift=1)
  {
    times <- df[,time.var]
    previous.index <- match(times-shift,times)
    for (evar in extract.var)
      {
        new.evar <- glue(evar,'.m',shift)
        df[,new.evar] <- df[previous.index,evar]
      }
    df
  }

#Solve using the generalised inverse of A 
gsolve <- function(A,b)
  {
    a.n(t(A)%*%cholsolve(chol(A%*%t(A)),b))
  }

#Given a list of lists such that the sub-lists all have elements with the same name, return a list with all elements concatenated
#ex. ccl(list(list(a=1,b=2),list(a=3,b=4))) ==> list(a=c(1,3),b=c(2,4))
ccl <- function(lst)
  {
    nms <- names(lst[[1]])
    nls <- lapply(nms,function(nm) lst %c% `nm`)
    names(nls) <- nms
    nls
  }

flipud <- function(M)
  {
    n <- nrow(M)
    M[n:1,]
  }

#test if a string is non-empty
is.nonempty <- function(x,...) UseMethod("is.nonempty",x)
is.nonempty.character <- function(x) if (length(x)>0) (nchar(x) > 0) else FALSE


plotev <- function(M,nev)
  {
    if (missing(nev))
      {
        ev <- eigen(M)$val
        plot(ev,main="Eigenvalues")
        range(ev)
      }
    else
      {
        eig <- eigen(M)
        v <- eig$vec[,nev]
        ev <- eig$val[nev]
        plot(v,main=sprintf("Eigenvector %i, %.3f",nev,ev))
      }
  }

"%d%" <- function(A,w)
  {
    sweep(A,2,w,"*")
  }


#Out of a function f(a,b,c) build a function that takes concatenated arguments
#g(x) = f(x[ind.a],x[ind.b],x[ind.c])
#Ex:
#conc.par(list(x=1,y=2),function(x,y) (x+y)^2)
#conc.par(list(x=1,y=2),function(x,y) (x+y)^2,named=T)

conc.par <- function(inds,f,named=F)
  {

    if (named)
      {
        tmp <- sprintf("%s=par[inds[[%i]]]",names(inds),1:length(inds))
      }
    else
      {
        tmp <- sprintf("par[inds[[%i]]]",1:length(inds))
      }
    tmp <- do.call(function(...) paste(...,sep=","),as.list(tmp))
    eval(parse(text=paste("function(par) f(",tmp,")",sep="")))

  }

mrnorm <- function(nr,nc,m=0,s=1)
  {
    matrix(rnorm(nr*nc,m=m,s=s),nr,nc)
  }

symmetrise <- function(A)
  {
    (t(A)+A)/2
  }

dberror <- function()
  {
    if (is.null(options("error")$error))
      {
        options(error=recover)
      }
    else
      {
        options(error=NULL)

      }

  }

 
eseq <- function(a,b,l)
  {
    delta <- log(b/a)
    a*exp(seq(0,delta,l=l))
  }

as.grid.t <- function(gr)
  {


  }

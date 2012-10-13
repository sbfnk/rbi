vectorsToList <- function(...) {
  return(mapply(c,...,SIMPLIFY=FALSE))
}

listToMatrix <- function(ls) {
  return(do.call(rbind,ls))
}

LGmodel <- function(theta,ys) {
  # The model is given by:
  #   x_1 ~ N(x0,v0)
  # and for i >= 2
  #   x_i ~ N(alpha*x_{i-1},vt)
  #   y_i ~ N(x_i,vo)
  alpha <- theta$alpha
  vt <- theta$vt
  vo <- theta$vo
  x0 <- theta$x0
  v0 <- theta$v0
  
  # square rooted variance parameters (for convenience)
  svo <- sqrt(vo)
  svt <- sqrt(vt)
  sv0 <- sqrt(v0)
  
  # this is a (simple) linear Gaussian model
  name <- "Linear Gaussian"
  
  # mu is the initial distribution
  mu <- list()
  
  # sample from the initial distribution
  mu$s <- function(N=1) {
      return(vectorsToList(rnorm(n=N,mean=x0,sd=sv0)))
  }
  
  # compute the log density of the initial distribution
  mu$ld <- function(x) {
    return(dnorm(x=x,mean=x0,sd=sv0,log=TRUE))
  }
  
  # M is the mutation kernel
  M <- list()
  
  # sample from M_i(x_{i-1},x_i)
  M$s <- function(xim1,i) {
    stopifnot(i > 1)
    if (is.list(xim1)) {
      xim1 <- listToMatrix(xim1)
    }
    return(vectorsToList(rnorm(n=length(xim1),mean=alpha*xim1,sd=svt)))
  }
  
  # evaluate the log density M_i(x_{i-1},\cdot)(x_i)
  M$ld <- function(xi,xim1,i) {
    stopifnot(t > 1)
    if (is.list(xi)) {
      xi <- listToMatrix(xi)
      xim1 <- listToMatrix(xim1)
    }
    return(dnorm(x=xi,mean=alpha*xim1,sd=svt,log=TRUE))
  }
  
  # you can define a model without data, probably to simulate data from it
  if (missing(ys)) {
    n <- 0
    ys <- NULL
  } else {
#     ys <- listToMatrix(ys)
    print(ys)
    n <- length(ys)
  }
  
  # G is the potential function
  G <- list()
  
  # evaluate the log of the potential G_i(x_i)
  G$ld <- function(x,i) {
    if (i == 1) {
      return(rep(0,length(x)))
    }
    if (is.list(x)) {
      x <- listToMatrix(x)
    }
    return(dnorm(x=ys[i],mean=x,sd=svo,log=TRUE))
  }
  
  # sample from the potential (assuming it is a distribution of y_i | x_i)
  G$s <- function(x,i) {
    stopifnot(i > 1)
    if (is.list(x)) {
      x <- listToMatrix(x)  
    }
    return(vectorsToList(rnorm(n=1,x,svo)))
  }
  
  # a model is a list of functions
  model <- list(mu=mu, M=M, G=G, n=n, name=name)
  
  return(model)
}

kalman <- function(theta,ys) {
  n <- length(ys)
  xs <- rep(0,n)
  vs <- rep(0,n)
  
  alpha <- theta$alpha
  alpha2 <- alpha^2
  vt <- theta$vt
  vo <- theta$vo
  x0 <- theta$x0
  v0 <- theta$v0
  
  mutt <- x0
  sigmatt <- v0
  
  ll <- 0;
  lls <- rep(0,n)
  
  xs[1] = mutt;
  vs[1] = sigmatt;
  for (i in 2:n) {
    mutt1 <- alpha*mutt;
    sigmatt1 <- alpha2*sigmatt+vt;
    S <- sigmatt1 + vo;
    ytt1 <- mutt1;
    
    temp <- sigmatt1/S;
    mutt <- mutt1 + temp*(ys[i]-ytt1);
    xs[i] <- mutt;
    sigmatt <- sigmatt1 - temp*sigmatt1;
    vs[i] <- sigmatt;
    ll <- ll + dnorm(ys[i],ytt1,sqrt(S),log=TRUE);
    lls[i] <- ll;
  }
  return(list(xs=xs,vs=vs,ll=ll,lls=lls))
}
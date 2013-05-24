logsumexp <- function(x) {
  return(log(sum(exp(x - max(x)))) + max(x))
}

logmeanexp <- function(x) {
  return(log(mean(exp(x - max(x)))) + max(x))
}

ancestors <- function(ws,N,log) {
  if (log) {
    return(sample(x=length(ws),size=N,replace=TRUE,prob=exp(ws-max(ws))))
  } else {
    return(sample(length(ws),N,TRUE,ws))
  }
}

weightedMeans <- function(xs, lws) {
  n <- length(xs)
  ws <- exp(lws-max(lws))
  ws <- ws / sum(ws)
  d <- length(xs[1])
  means <- rep(0,d)
  for (i in 1:n) {
    means = means + ws[i]*as.numeric(xs[i]);
  }
  return(means);
}

bootstrap_R <- function(model,N) {
  
  n <- model$n
  xs <- vector("list",n)
  lws <- array(rep(0,N*n),c(N,n))
  as <- array(rep(0,N*(n-1)),c(N,n-1))
  lls <- rep(0,n)
  means <- rep(0,n)
  
  ll <- 0
  xs[[1]] <- model$mu$s(N)
  lws[,1] <- model$G$ld(xs[[1]],1)
  ll <- ll + logmeanexp(lws[,1])
  lls[1] <- ll
  means[1] <- weightedMeans(xs[[1]],lws[,1])
  
  for (i in 2:n) {
    as[,i-1] <- ancestors(lws[,i-1],N,log=TRUE)
    xs[[i]] <- model$M$s(xs[[i-1]][as[,i-1]],i)
    lws[,i] <- model$G$ld(xs[[i]],i)
    means[i] <- weightedMeans(xs[[i]],lws[,i])
    
    ll <- ll + logmeanexp(lws[,i])
    lls[i] <- ll
  }
  
  return(list(xs = xs,ancs=as,lws=lws,ll=ll,lls=lls,means=means))
}
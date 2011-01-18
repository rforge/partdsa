####################################################################
###                                                                #
###                THIS is the NEWEST VERSION MARCH 2006           #
###                                                                #
### Addon is a function which takes all available basis functions  #
###  and looks at adding on possible functions by splitting a basis#
###  function into two additional functions                        #
### Addon takes the following parameters:                          #
###  the nxk matrix of basis functions                             #
### Addon returns the following:                                   #
###  the k choose 1 matrices of possible additions                 #
####################################################################


####################################################################
###  The way this is programmed for the multivariate - it does not #
###  do a cv-rss risk between the variables it just looks at the   #
###  empirical to save time                                        #
####################################################################

addon <- function(bas.fx, y, wt, dat, minbuck, real.LIST, opts,x.temp,is.num) {
  m <- ncol(bas.fx)  # number of basis functions
  keep.add <- list()
  for(M in 1:m) {
    best.of.P <- list()
    keep.var.P <- NULL

    k <- length(real.LIST[[M]][[1]])
    for(P in 1:ncol(dat)) {
      
      all.add <- vector("list", length=k)
      keep.var <- NULL
      for(K in 1:k) {
         
        
        split.ors <- assign.obs.own.OR.stmt(dat2=x.temp, n=nrow(dat), 
        p=ncol(dat),ORs=real.LIST[[M]], psi=bas.fx[,M],which.K=K)
        
        get.split <- split.fx(psi=split.ors[[1]], y=y, wt=wt, opts=opts,
                     x.split=dat[,P], n.cut=minbuck,
                     real.num=real.LIST[[M]][[P]][[K]],is.num=is.num[P])
  
        all.add[[K]] <- list(cut.value = get.split$val,new.lt = get.split$new.lt,
                             new.gt = get.split$new.gt, cant.split = get.split$cant.split,
                             Variable.number = P, OR.statement.number = K,
                             Exclude.OR.members = bas.fx[,M] - split.ors[[1]])
        keep.var[K] <- ifelse(get.split$cant.split, NA, get.split$max.g)
      }

      ## if can't split don't consider for best of the Mth BF
      keep.k <- which.max(keep.var)
      if (length(keep.k) == 0)
        keep.k <- 1

      best.of.P[[P]] <- all.add[[keep.k]]
      keep.var.P[P] <- keep.var[keep.k]
    }

    keep.k <- which.max(keep.var.P)
    if (length(keep.k) == 0)
      keep.k <- 1

    keep.add[[M]] <- best.of.P[[keep.k]]
  }
 
  return(keep.add)
}

assign.obs.own.OR.stmt <- function(dat2, n, p, ORs, psi, which.K) {
  ## p = num of variables
  ## n = num of obs
  ## dat2 = matrix with variables and corresponding values (nxp)
  ## ORs = list of basis functions
  
  if(length(ORs[[1]]) > 1) {
    add <- integer(n)
    for(P in 1:p) {
      if(is.numeric(dat2[,P])){
        x <- dat2[,P]
        add <- add + (ORs[[P]][[which.K]][1] < x & x <= ORs[[P]][[which.K]][2])
      } else if(is.factor(dat2[,P])){
        x <- as.numeric(dat2[,P])-1
        ## need to update this as - won't work for non-binary variables.
        ## here is code from the code.2.r splitting function
        ##   in.set <- direction[1:best.ind]
        ##    in.x <- matrix(NA, nrow=length(x.s.k), ncol=length(in.set))
        ##    for(ind in 1:length(in.set))
        ##      in.x[,ind] <- x.s.k == in.set[ind]
        ##    num.in.set <- sum(in.x)
        ##    num.not.in.set <- length(x.s.k) - num.in.set
      
        add <- add + (ORs[[P]][[which.K]][1] <= x & x < ORs[[P]][[which.K]][2])
      }
    }
    
    send.or <- as.numeric(add == p)
  }else  send.or <- psi
  
  return(list(send.or))
}

rss.dsa <- function(x, y, wt, minbuck=10, cut.off.growth=10,
                    MPD=0.1, missing="no", loss.function="default",wt.method="Cox", brier.vec=NULL) {
                    
  p <- ncol(x)
  n <- nrow(x)

  if (is.data.frame(x)) {
    # get the levels of each factor in the input data frame
    var.levels <- lapply(x, function(i) if (is.factor(i)) levels(i) else NULL)
    #is.num remembers the original class for each predictor variable 
    is.num <- unlist(lapply(x,function(i) if (is.factor(i)) 0 else 1))

    # convert any columns that are factors into numeric vectors
    if (any(unlist(lapply(var.levels, function(i) !is.null(i))))) {
      if (FALSE)
        warning('converting some columns from factors to double vectors')
      x <- do.call('data.frame', lapply(x, function(i) {
        if (is.factor(i)) as.numeric(i) else i
      }))
    }
  } else {
    var.levels <- vector('list', length=p)
    is.num <- unlist(lapply(x,function(i) if (is.factor(i)) 0 else 1))
  }
  var.names <- colnames(x)
  if (is.null(var.names))
    var.names <- sprintf('X%d', seq(length=ncol(x)))
   
  #create an imputed x.temp matrix for an initial vector of all 1's
    x.temp <- update.missing(x,x,y,rep(1,dim(x)[1]), is.num=is.num,todo="d", 
   missing=missing)

  ## Start new - with only one basis function - inclusive of all obs
  ## store corresponding basis functions
  keep.bas.fx <- list()

  ## store best RSS for each level (i.e. number of basis fxs)
  keep.rss.risks <- list()

  ## make list for basis fxs (called dynBF - as it is dyname BF holder)
  dynBF <- keep.bas.fx[[1]] <- newLIST(p=p)

  ## f.Io is the best RSS yet
  ## keep.rss.risks is the best

 
  bas.fx <- data.frame(assign.obs.to.bf(dat2=x.temp, n=n, p=p, BFs=dynBF))

  if (is.factor(y)) {
    outcome.class <- "factor"
    loss.fx <- switch(loss.function,
                      gini="gini",
                      entropy="entropy",
                      default="entropy",
                      stop("Incorrect loss function for categorical outcome"))
  } else if (inherits(y,"Surv")) {
    ## only IPCW and Brier are supported currently
    outcome.class <- "survival"
    loss.fx <- switch(loss.function,
                      IPCW="IPCW",
                      Brier="Brier",
                      DR="DR",
                      default="IPCW",
                      stop("Incorrect loss function for censored outcome"))
  } else {
    outcome.class <- "numeric"
    loss.fx <- switch(loss.function,
                      L2="L2",
                      L1="L1",
                      default="L2",
                      stop("Incorrect loss function for continuous outcome"))
  }
  
  opts <- list(loss.fx = loss.fx, outcome.class=outcome.class)
  # in the case of survival, add in these extra two options depending on loss function
  if(opts$loss.fx=="IPCW"){opts$wt.method=wt.method}
  if(opts$loss.fx=="Brier"){opts$brier.vec=brier.vec}
  

  
  
  f.Io <- risk.fx(bas.fx=bas.fx, y=y, wt=wt,opts=opts)

  keep.rss.risks[[1]] <- f.Io
  track.moves <- list()
  track.moves.ind <- 1

  repeat {
    ## DEL(I)
    dbug('starting deletion')
    bas.fx <- data.frame(assign.obs.to.bf(dat2=x.temp, n=n, p=p, BFs=dynBF))
    try.del <- check.del(bas.fx=bas.fx, y=y, wt=wt, real.LIST=dynBF, opts=opts)
    
    ## is it better than the best of the same size?
    ## if yes - replace best and go back to delete
    ## if not go to substitute
### added check for is.na
    if(!is.null(try.del$del.risk) &&
       ((round(keep.rss.risks[[length(try.del$new.bf)]], 5) - round(try.del$del.risk,5)) >=
        MPD * round(keep.rss.risks[[length(try.del$new.bf)]], 5))) {
      dbug('did deletion')
      keep.rss.risks[[length(try.del$new.bf)]] <- round(try.del$del.risk, 5)
      keep.bas.fx[[length(try.del$new.bf)]] <- try.del$new.bf

      ## update what we have  - and go back to delete
      dynBF <- try.del$new.bf
      ### revises x.temp to reflect last "good" deletion move
      x.temp <- update.missing(x.temp=x.temp,x=x,y=y,bas=try.del[[3]],is.num=is.num, todo="d",missing=missing)
      
      track.moves[[track.moves.ind]] <- list()
      track.moves[[track.moves.ind]][[1]] <- "Delete"
      track.moves[[track.moves.ind]][[2]] <- list(try.del$new.bf)
      track.moves[[track.moves.ind]][[3]] <- length(track.moves[[track.moves.ind]][[2]])
      track.moves.ind <- track.moves.ind + 1
    } else {
      ## Finished deletion - going into Substitution
      dbug('starting substitution')
      
      #create new basis functions using x.temp from last good delete move
      bas.fx <- data.frame(assign.obs.to.bf(dat2=x.temp, n=n, p=p, BFs=dynBF))
     
      try.sub.g <- subst(bas.fx=bas.fx, y=y, wt=wt, dat=x, minbuck=minbuck,
                         real.LIST=dynBF, opts=opts, x.temp=x.temp,is.num=is.num)
      
      best.sub <- try.sub.g$best.of.subs
      sub.risk <- try.sub.g$sub.risk
      try.add <- try.sub.g$try.add
      bf1 <- try.sub.g$best.sub$bf.1.binary
      bf2 <- try.sub.g$best.sub$bf.2.binary
      
      
      rm(try.sub.g)
      
      if(!is.null(sub.risk) &&
         ((round(keep.rss.risks[[length(best.sub)]], 5) - round(sub.risk,5)) >=
          MPD * round(keep.rss.risks[[length(best.sub)]], 5))) {
        dbug('did substitution')
        keep.rss.risks[[length(best.sub)]] <- round(sub.risk,5)
        keep.bas.fx[[length(best.sub)]] <- best.sub

        ## update what we have - and go back to delete
        dynBF <- best.sub
        ### update x.temp with this good substitution move
        x.temp <- update.missing(x.temp=x.temp,x=x,y=y, bas=cbind(bf1,bf2),
        is.num=is.num, todo="a",missing)
        
        track.moves[[track.moves.ind]] <- list()
        track.moves[[track.moves.ind]][[1]] <- "Substitution"
        track.moves[[track.moves.ind]][[2]] <- list(best.sub)
        track.moves[[track.moves.ind]][[3]] <- length(track.moves[[track.moves.ind]][[2]])
        track.moves.ind <- track.moves.ind + 1
      } else {
        ## Finished Substitution - going into Addition
        dbug('starting addition')
        
       
        new.add.bf <- assess.risk.on.add(poss.add=try.add, BF.list=dynBF,
                                         dat=x, y=y, wt=wt, opts=opts,x.temp=x.temp,is.num=is.num,missing=missing)

        ## if it's better than what we have replace what we have
        if(!is.na(new.add.bf$add.risk)) {
          if((length(keep.rss.risks) < length(new.add.bf$new.bf)) ||
             is.null(keep.rss.risks[[length(new.add.bf$new.bf)]])) {
            keep.rss.risks[[length(new.add.bf$new.bf)]] <- round(new.add.bf$add.risk, 5)
            keep.bas.fx[[length(new.add.bf$new.bf)]] <- new.add.bf$new.bf
            ##update x.temp to reflect good addition move
            x.temp <- new.add.bf$x.temp
          } else {
            if((round(keep.rss.risks[[length(new.add.bf$new.bf)]],5) - round(new.add.bf$add.risk,5)) >=
               MPD * round(keep.rss.risks[[length(new.add.bf$new.bf)]], 5)) {
              keep.rss.risks[[length(new.add.bf$new.bf)]] <- round(new.add.bf$add.risk, 5)
              keep.bas.fx[[length(new.add.bf$new.bf)]] <- new.add.bf$new.bf
              ##update x.temp to reflect good addition move
              x.temp <- new.add.bf$x.temp
            }

          }
           

          ## update what we have  - no matter if it's better or not
          dynBF <- new.add.bf$new.bf
          track.moves[[track.moves.ind]] <- list()
          track.moves[[track.moves.ind]][[1]] <- "Add"
          track.moves[[track.moves.ind]][[2]] <- list(new.add.bf$new.bf)
          track.moves[[track.moves.ind]][[3]] <- length(track.moves[[track.moves.ind]][[2]][[1]])
          track.moves.ind <- track.moves.ind + 1

          if(((length(dynBF) + 1) >= cut.off.growth + 1)) {
            dbug('cut off growth reached: breaking from loop')
            break
          }
        } else {
          dbug('add.risk is NA: breaking from loop')
          break
        }
      } # end addition
    } # end substitution
  } # end repeat

  

  if (opts$outcome.class=="numeric") {
     fun <- function(i) get.coefs(i, dat=x.temp, y=y, wt=wt)$coef
     get.coef.from.training <- lapply(keep.bas.fx, fun)
  } else if (opts$outcome.class=="factor") {
     fun <- function(i) get.votes(i, dat=x.temp, y=y, wt=wt)$vote
     get.coef.from.training <- lapply(keep.bas.fx, fun)
  }  else if (opts$outcome.class=="survival"){
     if(opts$loss.fx=="IPCW"){
           fun <- function(i) get.coefs(i, dat=x.temp, y=y[,1], wt=wt)$coef
           get.coef.from.training <- lapply(keep.bas.fx, fun)
     ###note that for the case below we'll be recalculating all the coefficients for each value in brier.vec  and so these coefficents
     # will not be used. however they do determine the outputted predicted values for the test set. and we may need to change this since
     # we're just using the first column of wts. y is still on the original scale which may be good.
     }else if(opts$loss.fx=="Brier" ){
           fun <- function(i) get.coefs(i, dat=x.temp, y=as.numeric(y[,1]>brier.vec[1]), wt=wt[,1])$coef
           get.coef.from.training <- lapply(keep.bas.fx, fun) 
           
     }
  }
     

  var.importance <- computeVariableImportance(keep.bas.fx, var.names)
  y.levels <- if (is.factor(y)) levels(y) else NULL

  object <- list(cut.off.growth=cut.off.growth,
                 minbuck=minbuck,
                 MPD=MPD,
                 IkPn=keep.bas.fx,
                 coefficients=get.coef.from.training,
                 IkPn.risks=keep.rss.risks,
                 moves=track.moves,
                 var.levels=var.levels,
                 var.names=var.names,
                 var.importance=var.importance,
                 options=opts,
                 y.levels=y.levels)
  class(object) <- 'dsa'
  
  object
}

trim <- function(object, ...) {
  UseMethod('trim')
}

trim.dsa <- function(object, cut.off.growth, ...) {
  if (cut.off.growth < 1)
    stop('cut off growth must be greater than zero')

  if (cut.off.growth < object$cut.off.growth) {
    n <- min(cut.off.growth, length(object$IkPn))
    object$IkPn <- object$IkPn[1:n]
    n <- min(cut.off.growth, length(object$coefficients))
    object$coefficients <- object$coefficients[1:n]
    n <- min(cut.off.growth, length(object$IkPn.risks))
    object$IkPn.risks <- object$IkPn.risks[1:n]
    n <- min(cut.off.growth, ncol(object$var.importance))
    object$var.importance <- object$var.importance[,1:n]
  } else {
    warning('trim value is larger than current cut off growth')
  }
  object
}

predict.dsa <- function(object, newdata,  ...) {
     
  get.bfs.for.test <- lapply(object$IkPn, get.bfs, dat=newdata)

  if(object$options$outcome.class=="numeric" | object$options$outcome.class=="survival"){
     fun <- function(i) {
     matrix(unlist(get.bfs.for.test[[i]]),
            ncol=ncol(get.bfs.for.test[[i]]),
            nrow=nrow(newdata)) %*%
            unlist(object$coefficients[[i]])
     }
     do.call('cbind', lapply(seq(along=object$IkPn), fun))
  } else if(object$options$outcome.class=="factor"){
     fun <- function(i) {
        x <- matrix(unlist(get.bfs.for.test[[i]][,-1]),
            ncol=(ncol(get.bfs.for.test[[i]])-1),
            nrow=nrow(newdata)) %*%
            unlist(object$coefficients[[i]])
        factor(object$y.levels[round(x)], levels=object$y.levels)
     }
     lapply(seq(along=object$IkPn), fun)
  }	
}





computeVariableImportance <- function(IkPn, var.names) {
  numRefs <- function(BFs) {
    varRefs <- function(BF) {
      anyFinite <- function(x) {
        as.integer(any(is.finite(unlist(x))))
      }
      unlist(lapply(BF, anyFinite))
    }

    result <- rep(0, length(BFs[[1]]))
    for (BF in BFs) {
      result <- result + varRefs(BF)
    }
    result
  }
  vi <- do.call('cbind', lapply(IkPn, numRefs))

  # sanity check var.names
  rownames(vi) <- if (nrow(vi) != length(var.names)) {
    warning('length of var.names not equal to rows of importance matrix')
    paste('X', seq(length=nrow(vi)), sep='')
  } else {
    var.names
  }
  colnames(vi) <- paste('COG=', seq(length=ncol(vi)), sep='')
  vi
}

print.dsa <- function(x, ...) {
  cat('dsa object\n')
  cat(sprintf('Loss function: %s\n', x$options$loss.fx))
  cat(sprintf('Outcome class: %s\n', x$options$outcome.class))
  printCoefficients(x)
  printBasisFunctions(x)
  cat('\nVariable importance matrix:\n')
  print(x$var.importance)
}

printCoefficients <- function(x) {
  cat('Coefficients:\n')
  for (i in seq(along=x$coefficients)) {
    coef <- x$coefficients[[i]]
    if (x$options$outcome.class == "numeric" | x$options$outcome.class == "survival") {
      n <- length(coef)
      ncoef <- if (n > 1) coef[2:n] + coef[1] else coef
      print(ncoef)
    } else if (x$options$outcome.class == "factor") {
      ncoef <- factor(x$y.levels[coef], levels=x$y.levels)
      print(ncoef)
    } else {
      cat(sprintf('[unsupported outcome class: %s]\n',
                  x$options$outcome.class))
      break
    }
  }
  cat('\n')
}

printBasisFunctions <- function(x) {
  M <- length(x$IkPn)
  for (i in 2:M) {
    cat(sprintf('Best %d partitions\n', i))
    BFs <- x$IkPn[[i]]
    for (j in seq(along=BFs)) {
      cat(sprintf('  Partition %d [of %d]:\n', j, i))
      BF <- BFs[[j]]
      for (K in seq(along=BF[[1]])) {
        and <- 0
        for (P in seq(along=BF)) {
          range <- BF[[P]][[K]]
          if (any(is.finite(range))) {
            var.levels <- x$var.levels[[P]]
            if (!is.null(var.levels)) {
              var.vals <- seq(along=var.levels)
              var.results <- var.levels[which(range[1] < var.vals &
                                              var.vals <= range[2])]
              lower <- ''
              upper <-
                if (length(var.results) > 1)
                  sprintf(' is in {%s}', paste(var.results, collapse=', '))
                else
                  sprintf(' is %s', var.results)
            } else {
              lower <-
                if (is.finite(range[1])) sprintf('%f < ', range[1]) else ''
              upper <-
                if (is.finite(range[2])) sprintf(' <= %f', range[2]) else ''
            }
            if (and > 0)
              cat(' && ')
            else
              cat('    ')
            cat(sprintf('(%s%s%s)', lower, x$var.names[P], upper))
            and <- and + 1
          }
        }
        cat('\n')
      }
    }
  }

  invisible(NULL)
}

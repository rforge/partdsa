worker <- function(cv.ind, minbuck, cut.off.growth, MPD, missing,
                   loss.function) {
  x <- data.frame(get('G.x')[get('G.grp.delt') != cv.ind,])
  y <- get('G.y')[get('G.grp.delt') != cv.ind]
  wt <- get('G.wt')[get('G.grp.delt') != cv.ind]

  x.test <- data.frame(get('G.x')[get('G.grp.delt') == cv.ind,])
  y.test <- get('G.y')[get('G.grp.delt') == cv.ind]
  wt.test <- get('G.wt')[get('G.grp.delt') == cv.ind]

  ## this calls a function in algAlone2.R
  ty <- rss.dsa(x=x, y=y, wt=wt, minbuck=minbuck,
                cut.off.growth=cut.off.growth, MPD=MPD,missing=missing,
                loss.function=loss.function)

  x.test <- impute.test(x=x,y=y,x.test=x.test,y.test=y.test,missing=missing)
  pred.test.DSA <- predict(ty, x.test)

  if (ty$options$outcome.class == 'numeric') {
     tmp <- wt.test * ((y.test - pred.test.DSA) ^ 2)
     cv.risk <- apply(tmp, 2, sum) / sum(wt.test)
  } else if (ty$options$outcome.class == 'factor') {
     fun <- function(f) mean(as.numeric(f) != as.numeric(y.test))
     cv.risk <- unlist(lapply(pred.test.DSA, fun))
  }

  c(cv.risk, rep(NA, cut.off.growth - length(cv.risk)))
}

worker.init <- function(lib.loc, a, b, c, d) {
  library(partDSA, lib.loc=lib.loc)
  assign('G.x', a, globalenv())
  assign('G.grp.delt', b, globalenv())
  assign('G.wt', c, globalenv())
  assign('G.y', d, globalenv())
  invisible(NULL)
}

partDSA <- function(x, y, wt=rep(1, nrow(x)), x.test=x, y.test=y, wt.test,
                    control=DSA.control(), sleigh) {
  ## Set up cross validation such that grp.delt contains
  ## the number of the fold for each observation
  missing <- control$missing
  vfold <- control$vfold
  minbuck <- control$minbuck
  cut.off.growth <- control$cut.off.growth
  MPD <- control$MPD
  loss.function <- control$loss.function
  lib.loc <- NULL

 
  missingness <- apply(x, 2, function(z) sum(as.numeric(!is.na(z))))
  if(length(which(missingness == 0)) > 0)
    stop(paste("Some variables in training set are 100% missing:",
               which(missingness == 0)))

  if (length(which(missingness != dim(x)[1])) > 0 && missing == "no")
   stop(paste("Missing values found in dataset and missing is set to 'no'.",
              "Set missing='impute.to.split' to proceed."))

  missingness <- apply(x.test, 2, function(z) sum(as.numeric(!is.na(z))))
  if(length(which(missingness == 0)) > 0)
    stop(paste("Some variables in test set are 100% missing:",
               which(missingness == 0)))

  if(length(which(missingness != dim(x.test)[1])) > 0 && missing == "no")
   stop(paste("Missing values found in dataset and missing is set to 'no'.",
              "Set missing='impute.to.split' to proceed."))

  # handle the default value for wt.test specially
  if (missing(wt.test)) {
    wt.test <- if (missing(x.test)) wt else rep(1, nrow(x.test))
  }

  # Only do cross validation if vfold > 1
  if (vfold > 1) {
    ## Set up cross validation. For the case of a categorical outcome variable,
    ## make sure all folds have the same proportions of levels.
    if (!is.factor(y)) {
      grp.delt <- sample(rep(1:vfold, length=nrow(x)), nrow(x), replace=F)
    } else {
      temp <- do.call("cbind", tapply(1:nrow(x), y, function(z) {
        tmp <- sample(rep(1:vfold, length=length(z)), length(z), replace=F)
        rbind(z, tmp)
      }))
      grp.delt <- temp[,order(temp[1,])][2,]
    }

    if (missing(sleigh) || ! require('nws', quietly=TRUE)) {
      worker.init(lib.loc, x, grp.delt, wt, y)
      test.risk.DSA <- lapply(1:vfold, worker, minbuck, cut.off.growth,
                              MPD, missing, loss.function)
    } else {
      r <- eachWorker(sleigh, worker.init, lib.loc, x, grp.delt, wt, y)
      lapply(r, function(e) if (inherits(e, 'error')) stop(e))
      test.risk.DSA <- eachElem(sleigh, worker, 1:vfold,
                                list(minbuck, cut.off.growth, MPD,
                                     missing, loss.function))
      # test if we got any errors
      lapply(test.risk.DSA, function(e) if (inherits(e, 'error')) stop(e))
    }

    ## DSA - after get the cv-validation results back - find
    ## the  number of partitions that minimizes the RSS
    ## (residual sum of squares). 4 different measures of this.
    ## can look at overall minimum (or 1se+overall min or the
    ## first minimum (or 1se+first min). These 4 numbers are in
    ## pick.k.as.min.DSA, pick.k.se1.DSA, pick.k.DSA, get.k.DSA

    cv.risks <- do.call('cbind', test.risk.DSA)
    mean.cv.risk.DSA <- apply(cv.risks, 1, mean, na.rm=T)
    sd.cv.risk <- apply(cv.risks, 1, sd, na.rm=T)
  } else {
    mean.cv.risk.DSA <- NULL
    sd.cv.risk <- NULL
  }

  ## now go back with test set and full training set and get predictions
  test2.ty <- rss.dsa(x=x, y=y, wt=wt, minbuck=minbuck,
                      cut.off.growth=cut.off.growth, MPD=MPD,missing=missing,
                      loss.function=loss.function)

  x.test <- impute.test(x=x,y=y,x.test=x.test,y.test=y.test,missing=missing)
  pred.test.set.DSA <- predict(test2.ty, x.test)

  if (test2.ty$options$outcome.class == 'numeric') {
     tmp <- wt.test * (as.vector(y.test) - pred.test.set.DSA) ^ 2
     test.set.risk.DSA <- apply(tmp, 2, sum) / sum(wt.test)
  } else if(test2.ty$options$outcome.class == 'factor') {
     fun <- function(f) mean(as.numeric(f) != as.numeric(y.test))
     test.set.risk.DSA <- unlist(lapply(pred.test.set.DSA, fun))
  }

  results <- c(test2.ty,  # inherit all fields from the dsa class
               list(mean.cv.risk.DSA=mean.cv.risk.DSA,
                    sd.cv.risk=sd.cv.risk,
                    pred.test.set.DSA=pred.test.set.DSA,
                    test.set.risk.DSA=test.set.risk.DSA))
  results['moves'] <- NULL  # XXX option to retain 'moves'?
  class(results) <- c('partDSA', class(test2.ty))
  return(results)
}

trim.partDSA <- function(object, cut.off.growth, ...) {
  if (cut.off.growth < 1)
    stop('cut off growth must be greater than zero')

  if (cut.off.growth < object$cut.off.growth) {
    # call the trim method defined in the super class (dsa)
    NextMethod()

    if (!is.null(object$mean.cv.risk.DSA)) {
      n <- min(cut.off.growth, length(object$mean.cv.risk.DSA))
      object$mean.cv.risk.DSA <- object$mean.cv.risk.DSA[1:n]
      n <- min(cut.off.growth, length(object$sd.cv.risk))
      object$sd.cv.risk <- object$sd.cv.risk[1:n]
    }
    n <- min(cut.off.growth, ncol(object$pred.test.set.DSA))
    object$pred.test.set.DSA <- object$pred.test.set.DSA[,1:n]
    n <- min(cut.off.growth, length(object$test.set.risk.DSA))
    object$test.set.risk.DSA <- object$test.set.risk.DSA[1:n]
  } else {
    warning('trim value is larger than current cut off growth')
  }
  object
}

print.partDSA <- function(x, ...) {
  cat('partDSA object\n')
  if (!is.null(x$mean.cv.risk.DSA)) {
    cat(sprintf('%s   %s   %s   %s\n',
                '# partitions', 'mean CV error',
                'sd CV error', 'test risk'))
    for (i in seq(along=x$mean.cv.risk.DSA[!is.na(x$mean.cv.risk.DSA)])) {
      cat(sprintf('%-12d   %-13f   %-11f   %-11f\n',
                  i, x$mean.cv.risk.DSA[i],
                  x$sd.cv.risk[i], x$test.set.risk.DSA[i]))
    }
  } else {
    cat(sprintf('%s   %s\n',
                '# partitions', 'test risk'))
    for (i in seq(along=x$test.set.risk.DSA[!is.na(x$test.set.risk.DSA)])) {
      cat(sprintf('%-12d   %-11f\n',
                  i, x$test.set.risk.DSA[i]))
    }
  }

  cat('\n')
  printCoefficients(x)
  printBasisFunctions(x)
  cat('\nVariable importance matrix:\n')
  print(x$var.importance)
}

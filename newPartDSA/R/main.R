worker <- function(cv.ind, minbuck, cut.off.growth, MPD, missing,
                   loss.function, control=DSA.control(),
                   # XXX default values?
                   wt.method, brier.vec) {
  x <- data.frame(get('G.x')[get('G.grp.delt') != cv.ind,])
  y <- if (inherits(get('G.y'), "Surv"))
    get('G.y')[get('G.grp.delt') != cv.ind,]
  else
    get('G.y')[get('G.grp.delt') != cv.ind]
  wt <- get('G.wt')[get('G.grp.delt') != cv.ind]

  x.test <- data.frame(get('G.x')[get('G.grp.delt') == cv.ind,])
  y.test <- if (inherits(get('G.y'), "Surv"))
    get('G.y')[get('G.grp.delt') == cv.ind,]
  else
    get('G.y')[get('G.grp.delt') == cv.ind]
  wt.test <- get('G.wt')[get('G.grp.delt') == cv.ind]

  ## Create weights for IPCW using assign.surv.wts function and the
  ## chosen wt.method
  if (loss.function == "IPCW") {
    wt <- assign.surv.wts(x, y=Surv(y[,1], y[,2]),
        opts=list(loss.fx=loss.function, wt.method=wt.method))
    wt.test <- assign.surv.wts(x=x.test, y=Surv(y.test[,1],y.test[,2]),
        opts=list(loss.fx=loss.function, wt.method=wt.method))
  }

  ## For Brier, create weights using assign.surv.wts but now we need
  ## different weights depending on the brier.vec cutpoint. therefore
  ## we create a matrix of weights where column i corresponds to the
  ## weights for the ith cutpoint
  if (loss.function == "Brier") {
    wt <- array(NA, c(nrow(x), length(brier.vec)))
    for (r in seq(along=brier.vec)) {
      wt[,r] <- assign.surv.wts(y=Surv(y[,1], y[,2]),
          opts=list(loss.fx=loss.function, wt.method=wt.method),
          T.star=brier.vec[r])
    }
    wt.test <- array(NA, c(nrow(x.test), length(brier.vec)))
    for (r in seq(along=brier.vec)) {
      wt.test[,r] <- assign.surv.wts(y=Surv(y.test[,1], y.test[,2]),
          opts=list(loss.fx=loss.function,wt.method=wt.method),
          T.star=brier.vec[r])
    }
  }

  ## this calls a function in algAlone2.R
  ty <- rss.dsa(x=x, y=y, wt=wt, minbuck=minbuck,
                cut.off.growth=cut.off.growth, MPD=MPD,missing=missing,
                loss.function=loss.function, control=control,
                wt.method=wt.method, brier.vec=brier.vec)

  x.test <- impute.test(x=x,y=y,x.test=x.test,missing=missing)
  pred.test.DSA <- predict(ty, x.test)

  if (ty$options$outcome.class == 'numeric') {
     tmp <- wt.test * ((y.test - pred.test.DSA) ^ 2)
     cv.risk <- apply(tmp, 2, sum) / sum(wt.test)
  } else if (ty$options$outcome.class == 'factor') {
     fun <- function(f) mean(as.numeric(f) != as.numeric(y.test))
     cv.risk <- unlist(lapply(pred.test.DSA, fun))
  } else if (ty$options$outcome.class == 'survival' &&
             ty$options$loss.fx == "IPCW") {
     tmp <- wt.test * ((y.test[,1] - pred.test.DSA) ^ 2)
     cv.risk <- apply(tmp, 2, sum) / sum(wt.test)
  } else if (ty$options$loss.fx == "Brier") {
     ## For brier, calculating the risk is more complicated because
     ## we need to re-determine the coefficients and the wt depending
     ## on the cutpoint and then use that to calculate the risk.
     ## this function (in new functions) adds up the risk over all
     ## the cutpoint values
     cv.risk <- calculate.risk(model=ty, x, y, wt, x.test, y.test, wt.test,
                               opts=list(loss.fx="Brier", brier.vec=brier.vec))
  }

  c(cv.risk, rep(NA, cut.off.growth - length(cv.risk)))
}

worker.init <- function(lib.loc, a, b, c, d) {
  ## XXX Why was this commented out?
  library(partDSA, lib.loc=lib.loc)
  library(survival)
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

  ## Survival
  wt.method <- control$wt.method
  brier.vec <- control$brier.vec
  ## Make the default value of brier.vec a single cutpoint equal to
  ## the median of y
  if ((is.null(brier.vec)) && loss.function == "Brier") {
    brier.vec=median(y[,1])
  }

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

  if (control$leafy == 1) {
    vfold <- 0

    if (is.factor(y)) {
      y.original <- y
      y.test.original <- y.test
      y <- ConvertFactorsToNumeric(y.original)
      y.test <- ConvertFactorsToNumeric(y.test.original)
    }

    if (missing(sleigh) || ! require('nws',quietly=TRUE)) {
      worker.init(lib.loc=NULL, x, -1, wt, y)
      tree.results <- lapply(1:control$leafy.num.trees, worker.leafy, minbuck,
                             cut.off.growth, MPD, missing, loss.function,
                             x, y, wt, x.test, y.test, wt.test, control,
                             wt.method, brier.vec)
    } else {
      r <- eachWorker(sleigh, worker.init, lib.loc=NULL, x, -1, wt, y)
      lapply(r, function(e) if (inherits(e,'error')) stop(e))
      tree.results <- eachElem(sleigh,worker.leafy,1:control$leafy.num.trees,
                               list(minbuck, cut.off.growth, MPD, missing,
                                    loss.function, x, y, wt, x.test, y.test,
                                    wt.test, control, wt.method, brier.vec))
      lapply(tree.results,function(e) if(inherits(e,'error')) stop(e))
    }

    predicted.values.by.tree <- lapply(tree.results,'[[',1)
    tree.prediction.rules<-lapply(tree.results,'[[',2)
    predicted.test.set.values.by.tree<-lapply(tree.results,'[[',3)
    first.partition.with.var.by.tree<-lapply(tree.results,'[[',4)
    variable.penetrance.by.tree<-lapply(tree.results,'[[',5)

    first.partition.with.var.on.average <- Reduce("+", first.partition.with.var.by.tree) /
                                           length(first.partition.with.var.by.tree)
    var.importance.list<-as.list(first.partition.with.var.on.average)

    variable.penetrance.on.average <- Reduce("+", variable.penetrance.by.tree) /
                                      length(variable.penetrance.by.tree)
    var.penetrance.list <- as.list(variable.penetrance.on.average)

    if (is.factor(y)) { #this is the categorical case
      categorical.results <- categorical.predictions(predicted.values.by.tree,
                                                     predicted.test.set.values.by.tree,
                                                     y, y.test, x, x.test,
                                                     y.original, y.test.original)
    } else if (inherits(y, "Surv")) {
      # This will be for survival
      stop('not implemented yet')
    } else {
      # must be the numeric case
      numerical.results <- numerical.predictions(predicted.values.by.tree,
                                                 predicted.test.set.values.by.tree,
                                                 y, y.test, x, x.test, wt, wt.test)
    }

    if (is.null(names(x))) {
      names(var.importance.list)<- paste("X", 1:dim(x)[2], sep = "")
      names(var.penetrance.list)<-paste("X", 1:dim(x)[2], sep = "")
    } else {
      names(var.importance.list)<-names(x)
      names(var.penetrance.list)<-names(x)
    }

    if (is.factor(y)) {
      results <- list(categorical.results[[1]][[2]],
                      categorical.results[[7]][[2]],
                      categorical.results[[8]][[2]],
                      categorical.results[[4]][[2]],
                      categorical.results[[5]][[2]],
                      categorical.results[[6]][[2]],
                      var.importance.list,
                      var.penetrance.list,
                      tree.prediction.rules)

      names(results) <- list("Training.Set.Error", "Predicted.Training.Set.Values",
                             "Predicted.Test.Set.Values", "Test.Set.Error",
                             "Training.Set.Confusion.Matrix",
                             "Test.Set.Confusion.Matrix", "VIMP",
                             "Variable.Penetrance", "Prediction.Rules")
    } else {
      results <- list(numerical.results[[1]][[2]],
                      numerical.results[[2]][[2]],
                      numerical.results[[3]][[2]],
                      numerical.results[[4]][[2]],
                      var.importance.list,
                      var.penetrance.list,
                      tree.prediction.rules)

      names(results) <- list("Training.Set.Error", "Predicted.Training.Set.Values",
                             "Predicted.Test.Set.Values", "Test.Set.Error", "VIMP",
                             "Variable.Penetrance", "Prediction.Rules")
    }
    class(results)<-('LeafyDSA')
  } else {
    # Only do cross validation if vfold > 1
    if (vfold > 1) {
      ## Set up cross validation. For the case of a categorical outcome variable,
      ## make sure all folds have the same proportions of levels.
      ## For survival, to create CV groups, use the same code as for a factor
      ## outcome but instead of using y as the groups, use the censoring
      ## variable as the groups so that we have the same % of censoring in
      ## each fold.
      if (is.factor(y)) {
        temp <- do.call("cbind", tapply(1:nrow(x), y, function(z) {
          tmp <- sample(rep(1:vfold, length=length(z)), length(z), replace=F)
          rbind(z, tmp)
        }))
        grp.delt <- temp[,order(temp[1,])][2,]
      } else if(inherits(y, "Surv")) {
        groups <- y[,2]
        temp <- do.call("cbind", tapply(1:nrow(x), groups, function(z) {
          tmp <- sample(rep(1:vfold, length=length(z)), length(z), replace=F)
          rbind(z, tmp)
        }))
        grp.delt <- temp[,order(temp[1,])][2,]
      } else if (is.numeric(y)) {
        grp.delt <- sample(rep(1:vfold, length=nrow(x)), nrow(x), replace=F)
      }

      if (missing(sleigh) || ! require('nws', quietly=TRUE)) {
        worker.init(lib.loc=NULL, x, grp.delt, wt, y)
        test.risk.DSA <- lapply(1:vfold, worker, minbuck, cut.off.growth,
                                MPD, missing, loss.function, control,
                                wt.method, brier.vec)
      } else {
        r <- eachWorker(sleigh, worker.init, lib.loc=NULL, x, grp.delt, wt, y)
        lapply(r, function(e) if (inherits(e, 'error')) stop(e))
        test.risk.DSA <- eachElem(sleigh, worker, 1:vfold,
                                  list(minbuck, cut.off.growth, MPD,
                                       missing, loss.function, control,
                                       wt.method, brier.vec))
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

    ## calculate weights for the entire training and test set
    if (loss.function == "IPCW") {
      wt <- assign.surv.wts(x=data.frame(x), y=Surv(y[,1], y[,2]),
          opts=list(loss.fx=loss.function, wt.method=wt.method))
      wt.test <- assign.surv.wts(x=data.frame(x.test),
          y=Surv(y.test[,1], y.test[,2]),
          opts=list(loss.fx=loss.function, wt.method=wt.method))
    }

    ## weights for brier can involve a matrix
    if (loss.function == "Brier") {
      wt <- array(NA, c(nrow(x), length(brier.vec)))
      for (r in 1:length(brier.vec)) {
        wt[,r] <- assign.surv.wts(y=Surv(y[,1],y[,2]),
            opts=list(loss.fx=loss.function,wt.method=wt.method),
            T.star=brier.vec[r])
      }
      wt.test <- array(NA, c(nrow(x.test), length(brier.vec)))
      for (r in 1:length(brier.vec)) {
        wt.test[,r] <- assign.surv.wts(y=Surv(y.test[,1], y.test[,2]),
            opts=list(loss.fx=loss.function,wt.method=wt.method),
            T.star=brier.vec[r])
      }
    }

    ## now go back with test set and full training set and get predictions
    test2.ty <- rss.dsa(x=x, y=y, wt=wt, minbuck=minbuck,
                        cut.off.growth=cut.off.growth, MPD=MPD,missing=missing,
                        loss.function=loss.function, control=control,
                        wt.method=wt.method, brier.vec=brier.vec)

    x.test <- impute.test(x=x,y=y,x.test=x.test,missing=missing)
    pred.test.set.DSA <- predict(test2.ty, x.test)

    if (test2.ty$options$outcome.class == 'numeric') {
      tmp <- wt.test * (as.vector(y.test) - pred.test.set.DSA) ^ 2
      test.set.risk.DSA <- apply(tmp, 2, sum) / sum(wt.test)
    } else if(test2.ty$options$outcome.class == 'factor') {
      fun <- function(f) mean(as.numeric(f) != as.numeric(y.test))
      test.set.risk.DSA <- unlist(lapply(pred.test.set.DSA, fun))
    } else if (test2.ty$options$outcome.class == 'survival' &&
               test2.ty$options$loss.fx == "IPCW") {
      tmp <- wt.test * (as.vector(y.test[,1]) - pred.test.set.DSA) ^ 2
      test.set.risk.DSA <- apply(tmp, 2, sum) / sum(wt.test)
    } else if (test2.ty$options$loss.fx == "Brier") {
      # separate function to calculate risk for brier because it needs
      # to be summed over all the cutpoints in brier.vec
      test.set.risk.DSA <- calculate.risk(model=test2.ty, x, y, wt,
              x.test, y.test, wt.test,
              opts=list(loss.fx="Brier", brier.vec=brier.vec))
    }

    results <- c(test2.ty,  # inherit all fields from the dsa class
                 list(mean.cv.risk.DSA=mean.cv.risk.DSA,
                      sd.cv.risk=sd.cv.risk,
                      pred.test.set.DSA=pred.test.set.DSA,
                      test.set.risk.DSA=test.set.risk.DSA))
    results['moves'] <- NULL  # XXX option to retain 'moves'?
    class(results) <- c('partDSA', class(test2.ty))
  }

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

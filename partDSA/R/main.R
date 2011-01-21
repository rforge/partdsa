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
                loss.function=loss.function,control)

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


worker.leafy <- function(tree.num, minbuck, cut.off.growth, MPD, missing,
                   loss.function,x.in,y.in,wt.in,x.test.in,y.test.in,wt.test.in,control) {
  set.seed(tree.num)   # delete this later   
                   	
  # Set up bootstrap sample
  
  
  obs.in.bag <- sample(1:(dim(x.in)[1]),dim(x.in)[1],T)
    
  x<-data.frame(x.in[obs.in.bag,])
  y<-y.in[obs.in.bag]
  wt <- wt.in[obs.in.bag]

  list.of.training.set.elements<-unique(obs.in.bag)
  list.of.oob.elements <- setdiff(seq(1:dim(x.in)[1]),list.of.training.set.elements)
   

  
  # OOB Setup
  x.oob <- data.frame(x.in[list.of.oob.elements,])
  y.oob <- y.in[list.of.oob.elements]
  wt.oob <- wt.in[list.of.oob.elements]
  
  
  ## this calls a function in algAlone2.R
  ty <- rss.dsa(x=x, y=y, wt=wt, minbuck=minbuck,
                cut.off.growth=cut.off.growth, MPD=MPD,missing=missing,
                loss.function=loss.function,control = control)
  
  rowmin <- function(matrix,row){
  	if(length(which(matrix[row,]!=0))==0){
  		dim(matrix)[2]
  	}else{
	  	min(which(matrix[row,]!=0))-1
	}
  }

  rowappearance<-function(matrix,row){
      if(length(which(matrix[row,]!=0))==0){
         0
       }else{
        #This line can be used to get where it appears --potential use later if we don't want to sum all possible appear. 
        #which(matrix[row,]!=0)
        sum(matrix[row,])
     }
  }


  first.partition.with.var<- mapply(rowmin, ty[10],1:dim(ty[10]$var.importance)[1],SIMPLIFY=TRUE,USE.NAMES=FALSE)
  total.partitions.possible<- (dim(ty[10]$var.importance)[2])*(dim(ty[10]$var.importance)[2]+1)/2 - 1
  variable.penetrance <-mapply(rowappearance,ty[10],1:dim(ty[10]$var.importance)[1],SIMPLIFY=TRUE,USE.NAMES=FALSE)/total.partitions.possible
  
  #If there are two many partitions, ty will not return null for the unused partitions, but will abruptly stop
  #max growth gets the maximum possible partition for the tree              
  max.growth <- length(ty[[5]])

	
  if(!identical(x.in,x.test.in)){
  	x.test <- impute.test(x=x.in,y=y.in,x.test=x.test.in,y.test=y.test.in,missing=missing) 
  }
  x.oob<-impute.test(x=x.in[list.of.training.set.elements,], y=y.in[list.of.training.set.elements], 
  					x.test = x.oob, y.test = y.oob, missing = missing)
  
  
  pred.oob.DSA <- predict(ty, x.oob)  
  if(!identical(x.in, x.test.in)){
  	pred.test.set.DSA <- predict(ty, x.test.in)
  	}

 
  if(is.factor(y)){
    #converts the factors into their actual number to avoid the "factor" data type.
    #Otherwise, the numbers start with 1 and go to n where n is the last factor.
    pred.oob.DSA<-pred.oob.DSA[[max.growth]]
    pred.oob.DSA<- as.numeric(levels(pred.oob.DSA))[as.integer(pred.oob.DSA)]
    if(!identical(x.in,x.test.in)){
    	pred.test.set.DSA<- pred.test.set.DSA[[max.growth]]
    	pred.test.set.DSA<- as.numeric(levels(pred.test.set.DSA))[as.integer(pred.test.set.DSA)]
    	}
  }
  else{
  	  pred.oob.DSA<-pred.oob.DSA[,max.growth]
  	  if(!identical(x.in,x.test.in)){
  	  	pred.test.set.DSA<-pred.test.set.DSA[,max.growth]

  	  	}
  	}

 	
 	###Computing the OOBError Rate
 	tree.oob.pred.values <- array(NA,c(dim(x.in)[1],1))
 	tree.oob.pred.values[list.of.oob.elements,] <- pred.oob.DSA
 	
 	tree.test.set.pred.values <- NA #Initialize this value if there is no training set
 	if(!identical(x.in,x.test.in))
 	{
 		tree.test.set.pred.values<- array(NA,c(dim(x.test.in)[1],1))
  	  	tree.test.set.pred.values[1:dim(x.test.in)[1],]<-pred.test.set.DSA
 		}
 	

   list(tree.oob.pred.values,ty,tree.test.set.pred.values,first.partition.with.var,variable.penetrance)
}


worker.init <- function(lib.loc, a, b, c, d) {
  ###All libraries are loaded by the call
  ###I load the library automatically at the start of my CODE FOR DEBUGGING
  ### CHANGE THIS LATER TO ORIGINAL VERSION!!!
  #library(partDSA, lib.loc=lib.loc)
  #print('I am in here!')
  
  source("~/Dropbox/Yale/Research/PartDSA/LeafyPartDSA21/MyScripts/LoadSource.R")
  #source("/home1/gr272/myfiles/LeafyPartDSA21/MyScripts/LoadSource.R")    
  
  assign('G.x', a, globalenv())
  assign('G.grp.delt', b, globalenv())
  assign('G.wt', c, globalenv())
  assign('G.y', d, globalenv())
  invisible(NULL)
}

partDSA <- function(x, y, wt=rep(1, nrow(x)), x.test=x, y.test=y, wt.test,control=DSA.control(), sleigh) {
	
  ## Set up cross validation such that grp.delt contains
  ## the number of the fold for each observation

  missing <- control$missing
  vfold <- control$vfold
  minbuck <- control$minbuck
  cut.off.growth <- control$cut.off.growth
  MPD <- control$MPD
  loss.function <- control$loss.function
  lib.loc <- NULL 
 
  ##The missingness variable says how many variables are missing
  ##in X which is a data frame
  missingness <- apply(x, 2, function(z) sum(as.numeric(!is.na(z)))) 
  
  ##Checks if an entire column of X is missing
  ##This check is performed for both the training set and the test set
  ##This corresponds to an entire covariate having no values. 
  ##Also tests if some values for a covariate are missing and if
  ##Throws a warning if missing != to impute.to.split
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

  
  if(control$leafy == 1){
  		vfold <- 0
  	
  		if(is.factor(y)){
  			y.original <- y
  			y.test.original<- y.test
  			y<- ConvertFactorsToNumeric(y.original)
  			y.test<- ConvertFactorsToNumeric(y.test.original)
  			
  			}
  	
  	  	if(missing(sleigh) || ! require('nws',quietly=TRUE)){
  			worker.init(lib.loc, x, -1, wt, y)
        	tree.results<- lapply(1:control$leafy.num.trees, worker.leafy,minbuck,cut.off.growth,MPD,missing, 					loss.function,x,y,wt,x.test,y.test,wt.test,control)
		}else{
			r <- eachWorker(sleigh, worker.init, lib.loc, x, -1, wt, y)
  			lapply(r, function(e) if (inherits(e,'error')) stop(e))
  			tree.results<-eachElem(sleigh,worker.leafy,1:control$leafy.num.trees,list			(minbuck,cut.off.growth,MPD,missing,loss.function,x,y,wt,x.test,y.test,wt.test,control))
  			lapply(tree.results,function(e) if(inherits(e,'error')) stop(e))
  		}
  		
  		predicted.values.by.tree <- lapply(tree.results,'[[',1)
  		tree.prediction.rules<-lapply(tree.results,'[[',2)
  		predicted.test.set.values.by.tree<-lapply(tree.results,'[[',3)
  		first.partition.with.var.by.tree<-lapply(tree.results,'[[',4)
                variable.penetrance.by.tree<-lapply(tree.results,'[[',5)
  		
  
  		first.partition.with.var.on.average<-Reduce("+",first.partition.with.var.by.tree)/length											(first.partition.with.var.by.tree)
  		var.importance.list<-as.list(first.partition.with.var.on.average)

                variable.penetrance.on.average<-Reduce("+",variable.penetrance.by.tree)/length(variable.penetrance.by.tree)
                var.penetrance.list<-as.list(variable.penetrance.on.average)
                

                
             	
  		if (is.factor(y)) { #this is the categorical case
			categorical.results<- categorical.predictions(predicted.values.by.tree,
												          predicted.test.set.values.by.tree,y,y.test,x,x.test,
												          y.original, y.test.original)
  		} else if (inherits(y,"survival")) { #This will be for survival 
   					
   	    } else { #must be the numeric case  	
			numerical.results<-numerical.predictions(predicted.values.by.tree,
												          predicted.test.set.values.by.tree,y,y.test,x,x.test,
												          wt,wt.test)
		}


		
		if(is.null(names(x))) {
			names(var.importance.list)<- paste("X", 1:dim(x)[2], sep = "")
			names(var.penetrance.list)<-paste("X", 1:dim(x)[2], sep = "")
			
		}else{
        	names(var.importance.list)<-names(x)
        	names(var.penetrance.list)<-names(x)
        }
  		
  		if(is.factor(y)){
  					
                  results<-list(categorical.results[[1]][[2]],
  						  categorical.results[[7]][[2]],
  						  categorical.results[[8]][[2]], 
  						  categorical.results[[4]][[2]],
  						  categorical.results[[5]][[2]],
  						  categorical.results[[6]][[2]],
  						  var.importance.list,
                          var.penetrance.list,
						  tree.prediction.rules)
  						  
  		   names(results)<- list("Training.Set.Error","Predicted.Training.Set.Values","Predicted.Test.Set.Values", "Test.Set.Error", "Training.Set.Confusion.Matrix", "Test.Set.Confusion.Matrix", "VIMP","Variable.Penetrance", "Prediction.Rules")
  						  		
  		}else{
  			
  			results<-list(numerical.results[[1]][[2]],
  						  numerical.results[[2]][[2]],
  						  numerical.results[[3]][[2]],
  						  numerical.results[[4]][[2]],
  						  var.importance.list,
                          var.penetrance.list,
  						  tree.prediction.rules)
  						  
  			names(results)<- list("Training.Set.Error", "Predicted.Training.Set.Values", "Predicted.Test.Set.Values","Test.Set.Error", "VIMP","Variable.Penetrance", "Prediction.Rules")
  						  	
  			}
  		class(results)<-('LeafyDSA')
  		
  }else{
  		
  	# Only do cross validation if vfold > 1
  	if (vfold > 1) {
    	## Set up cross validation. For the case of a categorical outcome variable,
    	## make sure all folds have the same proportions of levels.
	    if (!is.factor(y)) {
    		#gets a sample of size nrow(x) where the numbers range from 1 to vfold
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
                      loss.function=loss.function, control)

  	x.test <- impute.test(x=x,y=y,x.test=x.test,y.test=y.test,missing=missing)
  	pred.test.set.DSA <- predict(test2.ty, x.test)

	if (test2.ty$options$outcome.class == 'numeric') {
     tmp <- wt.test * (as.vector(y.test) - pred.test.set.DSA) ^ 2
     test.set.risk.DSA <- apply(tmp, 2, sum) / sum(wt.test) # the 2 means you sum over the columns
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

 print.LeafyDSA<-function(results){
	#print(results[1:7])
	cat(sprintf("\n \nTraining Set Error: %s \n \n", results[[1]]))
	
	cat(sprintf("Predicted Training Set Values: \n"))
	print(results[[2]])
	
	cat(sprintf("\n \nTest Set Error: %s \n \n", results[[4]]))
	
	cat(sprintf("\n \nPredicted Test Set Values: \n"))
	print(results[[3]])
	
	if(is.factor(results[[2]])){
		cat(sprintf("\n \nTraining Set Confusion Matrix: \n"))
		print(results[[5]])
		
		cat(sprintf("\n \nTest Set Confusion Matrix: \n"))
		print(results[[6]])
	
	
		cat(sprintf("\n \nVariable Importance:"))
		cat(sprintf("\nVariable Name : Average First Partition\n"))
		for(i in 1: length(results[[7]])){
			cat(sprintf("%s : %s\n",names(results[[7]])[i],results[[7]][[i]]))
		}
		
		cat(sprintf("\n \nVariable Penetrance:"))
		cat(sprintf("\nVariable Name : Average Penetrance\n"))
		for(i in 1: length(results[[8]])){
			cat(sprintf("%s : %s\n",names(results[[8]])[i],results[[8]][[i]]))
		}

		
	}
	else{
		cat(sprintf("\n \nVariable Importance:"))
		cat(sprintf("\nVariable Name : Average First Partition\n"))
		for(i in 1: length(results[[5]])){
			cat(sprintf("%s : %s\n",names(results[[5]])[i],results[[5]][[i]]))
		}
		cat(sprintf("\n \nVariable Penetrance:"))
		cat(sprintf("\nVariable Name : Average Penetrance\n"))
		for(i in 1: length(results[[6]])){
			cat(sprintf("%s : %s\n",names(results[[6]])[i],results[[6]][[i]]))
		}
	}		
}

print.LeafyPredictions<-function(results){
	
	cat(sprintf("\n \nPrediction Error Rate: %s \n \n", results[[1]][[2]]))
	cat(sprintf("Predicted Values: \n"))
	print(results[[2]][[2]])

	if(length(results)==3){
		cat(sprintf("\n \nConfusion Matrix For Predicted Values: \n"))
		print(results[[3]][[2]])
	}
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

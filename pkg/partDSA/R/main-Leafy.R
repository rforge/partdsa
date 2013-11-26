worker.leafy <- function(tree.num, minbuck, cut.off.growth, MPD, missing,
                   loss.function,x.in,y.in,wt.in,x.test.in,y.test.in,wt.test.in,control,
                   wt.method, brier.vec,cox.vec,IBS.wt) {
  ## This was used for testing, presumably
  # set.seed(tree.num)   # delete this later   
                   	
  # Set up bootstrap sample

  #Now subsample unless leafy.subsample=0

  if(control$leafy.subsample>0){
    number.to.sample <- round(control$leafy.subsample*dim(x.in)[1])
    obs.in.bag <- sample(1:(dim(x.in)[1]),number.to.sample,replace=FALSE)
  }else{
    obs.in.bag <- sample(1:(dim(x.in)[1]),dim(x.in)[1],replace=TRUE)
  }

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
                loss.function=loss.function,control = control,
                wt.method=wt.method, brier.vec=brier.vec)

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
  	x.test <- impute.test(x=x.in,y=y.in,x.test=x.test.in,missing=missing) 
  }
  x.oob<-impute.test(x=x.in[list.of.training.set.elements,], y=y.in[list.of.training.set.elements], 
  					x.test = x.oob, missing = missing)
  
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

print.LeafyDSA<-function(x, ...){
        results <- x
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

print.LeafyPredictions<-function(x, ...){
	results <- x
	cat(sprintf("\n \nPrediction Error Rate: %s \n \n", results[[1]][[2]]))
	cat(sprintf("Predicted Values: \n"))
	print(results[[2]][[2]])

	if(length(results)==3){
		cat(sprintf("\n \nConfusion Matrix For Predicted Values: \n"))
		print(results[[3]][[2]])
	}
}

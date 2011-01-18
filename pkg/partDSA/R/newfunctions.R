 add.bas.fx <- function(BFs, new.add, m) {
  ## add a bas fx to the current basis function configuration

  first.BF <- BFs[[m]]
  var.num <- new.add[[5]]
  k.num <- new.add[[6]]
  second.BF <- lapply(first.BF, function(x) x[k.num])
  first.BF[[var.num]][[k.num]][2] <- new.add[[1]]
  second.BF[[var.num]][[1]][1] <- new.add[[1]]
  BFs[[m]] <- first.BF
  BFs[[length(BFs) + 1]] <- second.BF
 
  return(BFs)
}

assess.risk.on.add <- function(poss.add, BF.list, dat, y, wt, opts,x.temp,is.num,missing) {
  gc()
  
  ## from 'add.bas.fx' find best addition for each bas.fx
  ## now see which of M additions minimizes the RSS
  add.risk <- NULL
  new.bf <- vector("list", length=length(poss.add))

  for (M in 1:length(poss.add)) {
    ## check if we can do the split
    if (!poss.add[[M]][[4]]) {
      new.bf[[M]] <- add.bas.fx(BFs=BF.list, new.add=poss.add[[M]], m=M)
      ###update x.temp based on this proposed addition move
      dat2 <- update.missing(x.temp=x.temp,x=dat,y=y,bas=cbind(poss.add[[M]]$new.lt,poss.add[[M]]$new.gt),is.num=is.num, todo="a",missing=missing)
      bas.fx <- data.frame(assign.obs.to.bf(dat2=dat2, n=nrow(dat),
                                            p=ncol(dat), BFs=new.bf[[M]]))
      add.risk[M] <- risk.fx(bas.fx=bas.fx, y=y, wt=wt, opts=opts)
      
    }
    else {
      add.risk[M] <- NA
    }
  }

  ind.a <- which.min(add.risk)[1]

  ### added
  if(sum(!is.na(add.risk))>0){
  ###update x.temp to reflect the chosen best addition move
  dat2 <- update.missing(x.temp=x.temp,x=dat,y=y,bas=cbind(poss.add[[ind.a]]
  $new.lt,poss.add[[ind.a]]$new.gt),is.num=is.num, todo="a",missing=missing)
}else{
 dat2=x.temp
}

  list(new.bf=new.bf[[ind.a]],
       add.risk=if(is.na(ind.a)) add.risk[1] else add.risk[ind.a],x.temp=dat2)
}

newLIST <- function(p) {
  return(list(rep(list(list(c(-Inf, Inf))), p)))
}

assign.obs.to.bf <- function(dat2, n, p, BFs) {
  ## p = num of variables
  ## n = num of obs
  ## dat2 = matrix with variables and corresponding values (nxp)

   
  fun <- function(BF) {
    or <- integer(n)
    for(K in 1:length(BF[[1]])) {
      add <- integer(n)
      for(P in 1:p) {
       # if(is.numeric(dat2[,P])){
       
          x <- as.numeric(dat2[,P])
                
          add <- add + (BF[[P]][[K]][1] < x & x <= BF[[P]][[K]][2])
        
      #  }else if(is.factor(dat2[,P])){
    
       
       #   x <- as.numeric(dat2[,P])-1
      
        #  add <- add + (BF[[P]][[K]][1] <= x & x <  BF[[P]][[K]][2])
       # }
      }
      or <- or + (add == p) 
    }
    as.numeric(or)  # XXX for compatibility
  }
  bf.locat <- do.call('cbind', lapply(BFs, fun))

  assign("bf.locat",bf.locat,envir=globalenv())

  return(list(bf.locat))
}


risk.fx <- function(bas.fx, y, wt, opts){
  if(opts$outcome.class=="factor"){
    get.risk<-categorical.risk(bas.fx=bas.fx,y=y,wt=wt,opts=opts)
  } else if (opts$outcome.class=="survival"){
    get.risk <- survival.overall.risk (bas.fx=bas.fx, y=y, wt=wt, opts=opts)
  } else get.risk<-rss.risk(bas.fx,y,wt)
  return(get.risk)
}


rss.risk <- function(bas.fx, y, wt) {
  n <- length(y)
  get.loss <- NULL
  lm.xy <- lm(y~., data=bas.fx, weight=wt)
  coef <- ifelse(is.na(lm.xy$coefficients), 0, lm.xy$coefficients)
  new <- as.matrix(cbind(1, bas.fx))
  pred.val <- new %*% coef
  get.loss  <- sum(wt * (y - pred.val) ^ 2)/sum(wt)
  return(get.loss)
}

num.x <- function(fv, lv, var.x) {
  return(sum(var.x > fv & var.x < lv))
}

categorical.risk <- function(bas.fx, y, wt, opts) {
  sum.impurities = 0
  for(i in 1:dim(bas.fx)[2]) {
    filter = bas.fx[,i]
    outcome.bucket = y[filter == 1]
    sum.impurities = sum.impurities + (length(outcome.bucket) / length(y)) *
        gini.impurity(outcome.bucket, opts)
  }

  return(sum.impurities)
}

gini.impurity <- function(y, opts) {
  p = rle(sort(as.integer(y)))$lengths / length(y)
  if (opts$loss.fx == "gini")
    1.0 - sum(p * p)
  else
    sum(-p * log(p))
}

get.bfs <-function(BFs, dat) {
  
  data.frame(cbind(rep(1, nrow(dat)),
                   assign.obs.to.bf(dat2=dat, n=nrow(dat),
                                    p=ncol(dat), BFs=BFs)[[1]]))
}

get.coefs <- function(BFs, dat, y, wt) {
  outs <- data.frame(assign.obs.to.bf(dat2=dat, n=nrow(dat),
                                      p=ncol(dat), BFs=BFs))
 
  names(outs)=paste("X",1:ncol(outs),sep="")
  lm.xy <- lm(y~., data=outs, weight=wt)
  coef <- ifelse(is.na(lm.xy$coefficients), 0, lm.xy$coefficients)
  r2 <- summary(lm.xy)$r.squared
  return(list(coef=coef, r2=r2))
}

get.votes <- function(BFs, dat, y, wt) {
  outs <- data.frame(assign.obs.to.bf(dat2=dat, n=nrow(dat),
                                      p=ncol(dat), BFs=BFs))
  part.pred <- numeric(ncol(outs))
  for (j in 1:ncol(outs)) {
    ty <- table(y[outs[,j] == 1])
    d <- dimnames(ty)[[1]][which.max(ty)]
    part.pred[j] <- match(d, levels(y))
  }

  list(vote=part.pred)
}


##This function takes the best.ind which corresponds to the maximum goodness
## value. If there is only one such value, this same input value will be
## returned. If there are multiple maximum goodness values, we choose the one 
##that is most central. Therefore this function is called every time we find 
##best.ind.

central.split <- function(best.ind,n){
 
  ## This line finds the absolute value of the difference between each best.ind   ##and the central value (n/2) and takes that best.ind value which has the
  ## minimum distance to the center.
  
  return(best.ind[which.min(abs(best.ind-n/2))[1]])
}


## x.temp is an imputed version of x with NA values filled in based on the average of that variable within that observation's node. X represents the original X matrix so that we can determine the original placement of NA values. Then we update the x.temp values corresponding to the NA values based on the new basis functions, bas. is.num is a binary vector of length equal to the number variables with a 1 representing a continuous variable and 0 representing a categorical variable. todo is equal to a when we are creating two new basis functions and it equals d when we are creating (combining) one new basis function
update.missing <- function (x.temp,x, y, bas,is.num, todo, missing){
  
  if(missing=="no"){
    
    return(x)
  }else if(missing=="default"){
  
  ### this is the case when we have two new basis function so dim(bas)[2]=2
  if (todo=="a"){
  
    # when we have a categorical outcome, the imputed value should be based on 
    # those observations in the node with the same outcome value
    if (is.factor(y)){
    
      # work on each variable individually
      for (i in 1:dim(x.temp)[2]){
 
        #get position of all na values for variable i
        na.value <- is.na(x[,i])

        #for each outcome level of y
        for (j in 1:length(levels(y))){
           
          #make sure there is at least one observation in
          # this basis function and level of y
          if(length((x[bas[,1] & y==levels(y)[j],i]))>0){
 
               
                #all observations in this node are missing on this 
                #variable so get average from this variale overall
          	if (length(which(!is.na(x[bas[,1]==1,i])))==0){
                	replace1 <- ifelse(is.num[i]==1,mean(x[,i],na.rm=T),
                	as.numeric(names(which.max(table(x[,i])))))
               
               		 warning(paste("All values of variable",names(x.temp)[i],"are NA in node. Using overall average of this variable."))

          	#all observations in this node for this level are missing
                # on this variables so get average from node
                }else if (length(which(!is.na(x[bas[,1]==1&
                y==levels(y)[j],i])))==0){
			
                        replace1 <- (ifelse(is.num[i]==1,mean(x[bas[,1]==1,i],na.rm=T),as.numeric(names(which.max(table(x[bas[,1]==1,i]))))))
                	warning(paste("All values of variable",names(x.temp)[i],"for level",levels(y)[j], "are NA in this node. Using overall average in the node"))

                #otherwise:no warnings messages, just get the imputed value from the mean or majority vote of those observations in proper level and proper node
          	}else{
                	replace1 <-  ifelse(is.num[i]==1,
                	mean(x[bas[,1]==1& y==levels(y)    [j],i],na.rm=T),
                	as.numeric(names(which.max(table(x[bas[,1]==1&y==levels(y)[j],i])))))
         	}
         
                
                #update x.temp matrix based on above

          	x.temp[na.value & bas[,1]==1 & y==levels(y)[j],i] <- replace1
          }


          #do the exact same thing as above but this time with basis function 2
          if(length(x[bas[,2]==1 & y==levels(y)[j],i])>0){
          
		if (length(which(!is.na(x[bas[,2]==1,i])))==0){
                	replace2 <- ifelse(is.num[i]==1,mean(x[,i],na.rm=T),
                	as.numeric(names(which.max(table(x[,i])))))
                	warning(paste("All values of variable",names(x.temp)[i],"are NA in node. Using overall average of this variable. "))
          
		}else if (length(which(!is.na(x[bas[,2]==1&y==levels(y)[j],i])))==0){
                
			replace2 <- ifelse(is.num[i]==1,mean(x[bas[,2]==1,i],na.rm=T),
                	as.numeric(names(which.max(table(x[bas[,2]==1,i])))))
               		warning(paste("All values of variable",names(x.temp)[i],"for level",levels(y)[j], "are NA in this node. Using overall average in the node."))
          	}else{
                	replace2 <-  ifelse(is.num[i]==1,
                	mean(x[bas[,2]==1& y==levels(y)[j],i],na.rm=T),
                	as.numeric(names(which.max(table(x[bas[,2]==1&y==levels(y)[j],i])))))
         	}
         
   
        
          	#do the same for the second basis function
          	x.temp[na.value & bas[,2]==1 & y==levels(y)[j],i] <- replace2
       
        }
      }
      }
  
    # this is the case for continuous outcomes, just take an mean or mode of that variable in the node
    }else{
      

      #for each variable
      for (i in 1:dim(x.temp)[2]){
        
      	#find location of  na values
        na.value <- is.na(x[,i])

        #if all observations in the node are NA for this variale,
        # take overall average
        if (length(which(!is.na(x[bas[,1]==1,i])))==0){
                replace1 <- ifelse(is.num[i]==1,mean(x[,i],na.rm=T),
                as.numeric(names(which.max(table(x[,i])))))
                warning(paste("All values of variable",names(x.temp)[i],
               "are NA in node. Using overall average of this variable."))
        #otherwise take average within this node
        }else{
                replace1 <- ifelse (is.num[i]==1,
                mean(x[bas[,1]==1,i],na.rm=T),
                as.numeric(names(which.max(table(x[bas[,1]==1,i])))))
        }
     
        #do the same as above, except don't worry about outcome class of y
        x.temp[na.value & bas[,1]==1,i] <- replace1

        #if all observaitons in the node are NA for this variable,
        # take overall average
	if (length(which(!is.na(x[bas[,2]==1,i])))==0){
                replace2 <- ifelse(is.num[i]==1,mean(x[,i],na.rm=T),
                as.numeric(names(which.max(table(x[,i])))))
                 warning(paste("All values of variable",names(x.temp)[i],
                "are NA in node. Using overall average of this variable."))
        #otherwise take average within this node
        }else{
                replace2 <- ifelse (is.num[i]==1,
                mean(x[bas[,2]==1,i],na.rm=T),
                as.numeric(names(which.max(table(x[bas[,2]==1,i])))))
        }
        
      
        x.temp[na.value & bas[,2]==1,i] <- replace2
    }

    }
  }  


  #Now we only have one basis function, so bas is a vector.
  if (todo=="d"){
    
    # in the case of a categorical outcome, update based on the observations 
    # in that node with the same outcome class
    if (is.factor(y)){
    
      #for each variable
      for (i in 1:dim(x.temp)[2]){
        
        #get which observations are na on this variable
        na.value <- is.na(x[,i])
   
      
        
        for (j in 1:length(levels(y))){
 
          if(length((x[bas==1&y==levels(y)[j],i]))>0){

          	if (length(which(!is.na(x[bas==1,i])))==0){
                	replace <- ifelse(is.num[i]==1,mean(x[,i],na.rm=T),
                	as.numeric(names(which.max(table(x[,i])))))
                       warning(paste("All values of variable",names(x.temp)[i],"are NA in node. Using overall average of this variable."))
          	}else if (length(which(!is.na(x[bas==1&y==levels(y)[j],i])))==0){
			replace <- ifelse(is.num[i]==1,mean(x[bas==1,i],na.rm=T),as.numeric(names(which.max(table(x[bas==1,i])))))
                warning(paste("All values of variable",names(x.temp)[i],"for level",levels(y)[j], "are NA in each node.
                        Using overall average in the node."))
          	}else{
               		replace <-  ifelse(is.num[i]==1,
                	mean(x[bas==1& y==levels(y)[j],i],na.rm=T),
                	as.numeric(names(which.max(table(x[bas==1&y==levels(y)[j],i])))))       

         	}
          
          
          	#depending on whether variable is continuous or categorical,
          	# update with the mean or mode
         
        
          	x.temp[na.value & bas==1 & y==levels(y)[j],i] <- replace
       
     
           }
	}
      
      }


    }else{
     #in the case of a continuous outcome
      for (i in 1:dim(x.temp)[2]){
        na.value <- is.na(x[,i])
      
        
        if (length(which(!is.na(x[bas==1,i])))==0){
                replace <- ifelse(is.num[i]==1,mean(x[,i],na.rm=T),
                as.numeric(names(which.max(table(x[,i])))))
                warning(paste("All values of variable",names(x.temp)[i],"are NA in node. Using overall average of this variable."))
        }else{
                replace <- ifelse (is.num[i]==1,
                mean(x[bas==1,i],na.rm=T),
                as.numeric(names(which.max(table(x[bas==1,i])))))
        }
        x.temp[na.value& bas==1,i] <- replace
      }
    }

  }
  
  


  return(x.temp)
  }
}



impute.test <- function(x,y,x.test,y.test,missing){

 if (missing=="no"){
    return(x.test)
 }
 else if (missing=="default"){
 ## impute the test matrix based on means and mode from training set
    for (k in 1:dim(x.test)[2]){
 
    #for categorical outcome, update based on the outcome class
    # as well as the observation's node
       if(is.factor(y)){
           for(i in levels(y)){
              x.test[(is.na(x.test[,k])&y.test==i),k] <- ifelse(is.factor(x[,k]),
              names(which.max(table(x[y==i,k]))),mean(x[y==i,k],na.rm=T))
           }
    
       }else {   
          x.test[(is.na(x.test[,k])),k] <- ifelse(is.factor(x[,k]),
          names(which.max(table(x[,k]))),mean(x[,k],na.rm=T))
       }
    }

 return(x.test)
 }
}









############################################################################################################################################
############################################################################################################################################
##### Survival Functions ###


## This is the top level of the survival function which is called. The first if clause is for Brier and it repeats the lower level survival function the number of times 
##corresponding to the length of the brier.vec. It then sums the risk over these times. The second if clause is for IPCW and just runs the lower risk function once.
survival.overall.risk <- function(bas.fx,y,wt,opts){
        
         brier.vec=opts$brier.vec
         if(opts$loss.fx=="Brier"){ 
              ##Defines the function to be called for each cutpoint in Brier.vec 
              get.each.risk <- function(index){
              
                  ##creates a binary y vector given a specific values of T.star
                   T.star=brier.vec[index]
                   y.Brier=as.numeric(y[,1]>T.star)
                  
                  ##Finds the corresponding weights for this y vector
                   wt.Brier=wt[,index]
                  ##Gets the risk for this particular y, wt, and bas.fx combination
 		               get.risk=survival.risk(bas.fx,y.Brier,wt.Brier,opts)
                   return(get.risk)
              }
         
             
              
              ##risks is a list with each element corresponding to the risk for each cutpoint in brier.vec   
              risks=lapply(1:length(brier.vec),get.each.risk)
              
              ## get the overall risk by summing risks for each individual brier cutpoint
              get.risk=sum(unlist(risks))
         } else if(opts$loss.fx=="IPCW" ){
             ## For IPCW, just called the survival risk function once.
             get.risk=survival.risk(bas.fx,y[,1],wt,opts)
        }
      

	return(get.risk)






}


## Note that y is just the survival time (without the censoring component).  For IPCW, this will be a continuous variable and for brier it will be binary
## derived from the continuous variable and the cutpoint. 
survival.risk<-function(bas.fx,y,wt,opts){
      
   ## probably don't need this "if" statement
   if (opts$loss.fx=="IPCW"|opts$loss.fx=="Brier"){
    
  	  ## calculate the average within each basis function vector by first multiplying the weights by the y-values
      numerats<-y*wt
     	denomins<-wt
        
       ## Do some formatting
       if(!is.matrix(bas.fx)){
    		   if(is.vector(bas.fx)){
      			  bas.fx<-as.matrix(bas.fx,ncol=1)
    		   }else bas.fx<-as.matrix(bas.fx,ncol=ncol(bas.fx))
 		  }
      if (!is.vector(numerats)){
               numerats=as.vector(numerats)
      }
      if (!is.vector(denomins)){
              denomins=as.vector(denomins)
      }
    
      ## Then sum the numerats within each basis function and divide by the total weight in that basis function. betas will be predicted values in each basis function.
   
      betas<- apply(bas.fx*numerats,2,sum)/(apply(bas.fx*denomins,2,sum))
      
      ## Get predicted value for each observation by multiplying bas.fx by betas.  
      pred.val <- bas.fx %*% betas
  
        ##calculate loss by comparing y to predicted value
      get.loss <- sum(wt * (y - pred.val) ^ 2)/sum(wt)
        
        
      return(get.loss)
     
   }
 }



 ##### get.at.surv.times #### from AM
 "get.at.surv.times"<-
function(surv.cens, times)
{
#
# surv.cens is an object created by survfit
# times is a vector of times at which you want 
# an estimate of the survival function
#
	nt <- length(times)
	outs <- rep(0, nt)
	survv <- surv.cens$surv
	ns <- length(survv)
	timev <- surv.cens$time
	for(i in 1:nt) {
		if(times[i] < timev[1]) {
			outs[i] <- 1
		}
		else if(times[i] >= timev[ns]) {
			outs[i] <- survv[ns]
		}
		else {
			outs[i] <- survv[timev == max(timev[timev <= times[i]])
				][1]
		}
	}
	no <- length(outs[outs == 0])
	outs[outs == 0] <- rep(survv[ns - 1], no)
	return(outs)
}


#### get.init.am ### from AM
"get.init.am"<-
function(surv.cens, coeff.cox, w, delta, ttilde,deltaMod=delta)
{
        w <- ifelse(is.na(w),0,w)
        if(!is.matrix(w)){
          w <- matrix(w,nrow=length(w),ncol=1)
        }
      	nn <- length(ttilde)
        coeff.cox <- matrix(coeff.cox,nrow=length(coeff.cox),ncol=1)
        coeff.cox[is.na(coeff.cox)] <- 0
      	linpred <- w %*%coeff.cox
 
      	sum.surv.cond <- surv.cens^(exp(linpred))
        sum.surv.cond <- ifelse(sum.surv.cond<.2,.2,sum.surv.cond)
        if(is.na(min(sum.surv.cond))){
          if(delta[is.na(sum.surv.cond)]==0){
          sum.surv.cond[is.na(sum.surv.cond)] <- 1
          } 
        }
        ##### For IPCW, deltaMod is just delta####
	B <- (deltaMod)/sum.surv.cond
	return(B)
}


################## Assign Survival Weights ###################################################################

## Note x is only passed in if we are doing IPCW using the cox method to calculate the weights.
assign.surv.wts <- function (x,y,opts,T.star=T.star){
  ##note: default for IPCW in opts will be cox
 
 
  cens=y[,2]
  censC=1-cens
  Time=y[,1]


  #case 1 with loss function as IPCW 
  if (opts$loss.fx=="IPCW") {
          
     ## The method for calculating the weights for IPCW can be cox or KM, first option is cox
     if(opts$wt.method=="Cox"){
      
     ## if condition means that at least some patients are censored
     if(sum(censC)>0){
    
  	    mm <- model.matrix(~.,x) 
  	     
        keep.for.cox <- apply(data.frame(apply(x,1,is.na)),2,sum)
        Time.cox<- Time[keep.for.cox==0]
        censC.cox <- censC[keep.for.cox==0]
        cens.cox <- cens[keep.for.cox==0]
        cov.tr <- as.matrix(mm[,-1])
        surv.cox <- coxph(Surv(Time.cox, censC.cox) ~ cov.tr)

        # Get coefficients from cox model
        coeff.cox <- surv.cox$coefficients
        # Get baseline survival
        surv.cens <- survfit(surv.cox, newdata = data.frame(cov.tr = 0),type = "kaplan-meier")
        #  Vector of baseline survival
        basesurv <- get.at.surv.times(surv.cens, Time.cox)
        # UG(D) for each subject
        ic0 <- get.init.am(basesurv, coeff.cox, cov.tr, cens.cox, Time.cox)
        
        ## what to do with missing data in \bar{G} now give them a one or 0
        cox.wt1 <- keep.for.cox
        cox.wt1 <- ic0
        cox.wt1[keep.for.cox==1] <- cens[keep.for.cox==1]
      
        
    }
    else cox.wt1<-cens
   
    return(as.vector(cox.wt1))
   
  }else if(opts$wt.method=="KM"){
  #case 2 where loss function is IPCW and wt.method is KM


  
    #create kaplan meier model
    KMfit=survfit(Surv(Time,censC)~1)

    #calculate weights by finding the "survival" probability corresponding to the given Brier time
    G=NULL
    for (i in 1:length(Time)){
      G[i]<-KMfit$surv[which(KMfit$time==Time[i])]
      if(G[i]<.2){G[i]=.2}
    }
  
  #the actual weight is 1/G
  KM.wt1<-1/G

  #set those people censored to have weight 0
  KM.wt1[which(cens==0)] <- 0

  return (KM.wt1)  


  }

}else if(opts$loss.fx=="Brier"){

   #now with loss function as brier, we use kaplan meier to calculate weights

  # set T.star to be the next smallest Time value
  T.star.closest=Time[(Time-T.star)>=0][which.min((Time-T.star)[(Time-T.star)>=0])]
  
  #For people with event time after T.star, set their brier time equal to T* in order to find the weight according to Graf equation
  Time_Brier=ifelse(Time>T.star,T.star.closest,Time)
  
  #create kaplan meier model
  KMfit=survfit(Surv(Time,censC)~1)
  
  

  #calculate weights by finding the "survival" probability corresponding to the given Brier time
  G=NULL
 
  for (i in 1:length(Time)){
  G[i]<-KMfit$surv[which(KMfit$time==Time_Brier[i])]
  
  if(G[i]<.2){G[i]=.2}
  }
  
  #the actual weight is 1/G
  KM.wt1<-1/G

  #set those people censored before T.star to have weight 0
  KM.wt1[which(Time<=T.star& cens==0)] <- 0
  

 
  return (KM.wt1)  


}

}

calculate.risk <- function(model, x, y, wt, x.test, y.test, wt.test, opts){
   test.set.risk.DSA=array(0,length(model$coefficients))
     #brier.vec is a vector containing the times we are looking at for brier
     brier.vec=opts$brier.vec
     for (i in 1:length(brier.vec)){
                
                 
                T.star=brier.vec[i]
                #create binary versions of y based on this particular T.star value
                y.Brier=as.numeric(y[,1]>T.star)
                y.test.Brier=as.numeric(y.test[,1]>T.star)
               
 	
                #find weights using this particular T.star value 
                 wt.Brier=wt[,i]
                 wt.test.Brier=wt.test[,i]
                #using these weights and y-values, modify the field ty$coefficients to represent the new predicted values for each node 
                fun <- function(k) get.coefs(k, dat=x, y=y.Brier, wt=wt.Brier)$coef
                get.coef.from.training <- lapply(model$IkPn, fun)
                model$coefficients <- get.coef.from.training
                
                #use this updated ty, to predict values based on the test set
                pred.test.DSA <- predict.dsa(model,x.test)
                
                #calculate the loss and make cv.risk the sum over all values in brier.vec
                tmp <- as.vector(wt.test.Brier )* (as.vector(y.test.Brier) - pred.test.DSA) ^ 2
    
                test.set.risk.DSA <- test.set.risk.DSA + apply(tmp, 2, sum) / sum(wt.test.Brier)
                
     }
     
     return (test.set.risk.DSA)
     
}

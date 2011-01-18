
GetData.F1<- function(ls.n=200,ts.n=1000,sd.1=1){
 ls<-mlbench.friedman1(n=ls.n,sd=sd.1)
 ts<-mlbench.friedman1(n=ts.n,sd=sd.1)
 return(list(ls.set=ls,ts.set=ts))
}



GetData.F2<-function(ls.n=200,ts.n=1000,sd.2=.35){

 ls<-mlbench.friedman2(n=ls.n,sd=sd.2)
 ts<-mlbench.friedman2(n=ts.n,sd=sd.2)

 return(list(ls.set=ls,ts.set=ts))

}

GetData.F3<-function(ls.n=200,ts.n=1000,sd.3=.35){

 ls<-mlbench.friedman3(n=ls.n,sd=sd.3)
 ts<-mlbench.friedman3(n=ts.n,sd=sd.3)

 return(list(ls.set=ls,ts.set=ts))

}


GetData.F4<-function(ls.n=200,ts.n=1000,sd.4=.0625,U){



if (U==0){
  x1<-rnorm(ls.n,0,1)
  x2<-rnorm(ls.n,0,1)
  x3<-rnorm(ls.n,0,1)
  er<-rnorm(ls.n,0,sd.4)
  x4<-rnorm(ls.n,0,1)
  x5<-rnorm(ls.n,0,1)
  x6<-rnorm(ls.n,0,1)
  x7<-runif(ls.n,0,1)
  x8<-rgamma(ls.n,1,2)
  x9<-rexp(ls.n,2)
  x10<-rchisq(ls.n,.5)
  ls<-list(x=cbind(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10),y=x1^2+x2^2+x3^2+er)
  x1<-rnorm(ts.n,0,1)
  x2<-rnorm(ts.n,0,1)
  x3<-rnorm(ts.n,0,1)
  er<-rnorm(ts.n,0,sd.4)
  x4<-rnorm(ts.n,0,1)
  x5<-rnorm(ts.n,0,1)
  x6<-rnorm(ts.n,0,1)
  x7<-runif(ts.n,0,1)
  x8<-rgamma(ts.n,1,2)
  x9<-rexp(ts.n,2)
  x10<-rchisq(ts.n,.5)
}
else{
  x1<-runif(ls.n,0,1)
  x2<-runif(ls.n,0,1)
  x3<-runif(ls.n,0,1)
  er<-rnorm(ls.n,0,sd.4)
  x4<-runif(ls.n,0,1)
  x5<-runif(ls.n,0,1)
  x6<-runif(ls.n,0,1)
  x7<-rnorm(ls.n,0,1)
  x8<-rgamma(ls.n,1,2)
  x9<-rexp(ls.n,2)
  x10<-rchisq(ls.n,.5)
  ls<-list(x=cbind(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10),y=x1^2+x2^2+x3^2+er)
  x1<-runif(ts.n,0,1)
  x2<-runif(ts.n,0,1) 
  x3<-runif(ts.n,0,1)
  er<-runif(ts.n,0,sd.4)
  x4<-runif(ls.n,0,1)
  x5<-runif(ls.n,0,1)
  x6<-runif(ls.n,0,1)
  x7<-rnorm(ls.n,0,1)
  x8<-rgamma(ls.n,1,2)
  x9<-rexp(ls.n,2)
  x10<-rchisq(ls.n,.5)
}
  ts<-list(x=cbind(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10),y=x1^2+x2^2+x3^2+er)

  

  return(list(ls.set=ls,ts.set=ts))

}

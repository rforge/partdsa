library(partDSA)
library(mlbench)
source('FriedmanData.R')

s <- sleigh(workerCount=2,verbose=TRUE)
stat <- status(s, TRUE, 60)
cat(sprintf('Running a sleigh with %d workers\n', stat$numWorkers))

seed <- 6442
set.seed(seed)
# loglevel(debug)

n <- 250
ts.n <- 1000
var <- .25
U <- 0

data <- 'F1'
get.dat <- switch(data,
    F1=GetData.F1(ls.n=n, ts.n=ts.n, sd.1=var),
    F2=GetData.F2(ls.n=n, ts.n=ts.n, sd.2=var),
    F3=GetData.F3(ls.n=n, ts.n=ts.n, sd.3=var),
    F4=GetData.F4(ls.n=n, ts.n=ts.n, sd.4=var, U))

x <- get.dat$ls.set$x
y <- get.dat$ls.set$y
wt <- rep(1, n)
x.test <- get.dat$ts.set$x
y.test <- get.dat$ts.set$y
wt.test <- rep(1, ts.n)

control <- DSA.control(minbuck=20, cut.off.growth=7)

time <- system.time(
    results <- partDSA(x=x, y=y, wt=wt,
                       x.test=x.test, y.test=y.test, wt.test=wt.test,
                       control=control, sleigh=s))
print(results)
cat('New elapsed time: ')
print(time[[3]])

##### Example 1 ####

library(VGAM)
library(survival)

tr.n <- 250
ts.n <- 500
x <- data.frame(sample(1:100, tr.n, replace=TRUE))
names(x) <- "X1"
x.test <- data.frame(sample(1:100, ts.n, replace=TRUE))
names(x.test) <- "X1"

scale <- ifelse(x <= 15 | x > 85, .5, 5)
time <- (rgpd(tr.n, 0, scale, 0))
cens <- rep(1, tr.n)
cens[sample(1:tr.n, round(tr.n/3), FALSE)] <- 0
y <- log(time)


scale.test <- ifelse(x.test <= 15 | x.test > 85, .5, 5)
time.test <- (rgpd(ts.n, 0, scale.test, 0))
cens.test <- rep(1, ts.n)
cens.test[sample(1:ts.n, round(ts.n/3), FALSE)] <- 0
y.test <- log(time.test)

detach(package:VGAM)


y <- Surv(y,cens)
y.test <- Surv(y.test, cens.test)

library(partDSA)
model.IPCW <- partDSA(x=x, y=y, x.test=x.test, y.test=y.test,
    control=DSA.control(vfold=5, MPD=.01, minbuck=20,
                        loss.function="IPCW",wt.method="KM"))
print(model.IPCW)
dumpDSA(model.IPCW, 'surv.xml')
showDSA(model.IPCW)

model.Brier <- partDSA(x=x, y=y, x.test=x.test, y.test=y.test,
    control=DSA.control(vfold=5, MPD=.01, minbuck=20,
                        loss.function="Brier"))
print(model.Brier)
showDSA(model.Brier)


#### Example 2 ####

library(VGAM)
library(survival)
set.seed(1)
p <- 5
x <- matrix(NA, tr.n, p)
for (j in 1:p) {
  x[,j] <- sample(1:100, tr.n, replace=TRUE)
}
x <- data.frame(x)
names(x) <- c(paste("X",1:p,sep=""))

x.test <- matrix(NA,ts.n,p)
for (j in 1:p){
  x.test[,j] <- sample(1:100, ts.n, replace=TRUE)
}
x.test <- data.frame(x.test)
names(x.test) <- c(paste("X", 1:p, sep=""))

scale <- ifelse(x[,1] > 50 | x[,2] > 75, 5, .5)
time <- (rgpd(tr.n, 0, scale, 0))
cens <- rep(1, tr.n)
cens[sample(1:tr.n, round(tr.n/3), FALSE)] <- 0


scale.test <- ifelse(x.test[,1] > 50 | x.test[,2] > 75, 5, .5)
time.test <- (rgpd(ts.n, 0, scale.test, 0))
cens.test <- rep(1, ts.n)
cens.test[sample(1:ts.n, round(ts.n/3), FALSE)] <- 0


detach(package:VGAM)

y <- Surv(time,cens)
y.test <- Surv(time.test, cens.test)

library(partDSA)
set.seed(1)
model.IPCW <- partDSA(x=x, y=y, x.test=x.test, y.test=y.test,
    control=DSA.control(vfold=5, MPD=.01, minbuck=20,
                        loss.function="IPCW", wt.method="KM"))
print(model.IPCW)

set.seed(1)
model.Brier <- partDSA(x=x, y=y, x.test=x.test, y.test=y.test,
    control=DSA.control(vfold=5, MPD=.01, minbuck=20,
                        loss.function="Brier"))
print(model.Brier)

#### Example 3: Data Analysis ###
library(ipred)
data(GBSG2)
dataset <- GBSG2
outcome <- Surv(dataset$time, dataset$cens)
vars <- dataset[,c(1:8)]

x <- vars
y.new <- outcome[,1]
cens <- outcome[,2]
cens[which(outcome[,1] > 2000)] <- 0
y.new <- ifelse(outcome[,1] > 2000, 2000, outcome[,1])

y <- log(y.new)

brier.vec <- median(y)

set.seed(1)
model.IPCW <- partDSA(x=x, y=Surv(y,cens),
    control=DSA.control(vfold=5, cut.off.growth=10, MPD=.01, minbuck=20,
                        loss.function="IPCW"))
print(model.IPCW)

set.seed(1)
model.Brier <- partDSA(x=x, y=Surv(y,cens),
    control=DSA.control(vfold=5, cut.off.growth=10, MPD=.01, minbuck=20,
                        loss.function="Brier", brier.vec=brier.vec))
print(model.Brier)

library(partDSA)

kdat <- read.table(file="cmpdat2.txt")
xdat <- kdat[,c(2:9,16:30,32:42)]
ydat <- Surv(kdat$surv, kdat$cens)
md.surv <- partDSA(x=xdat, y=ydat,
                   control=DSA.control(vfold=5, MPD=.2, missing='impute.at.split'))
dumpDSA(md.surv, 'surv2.xml')
showDSA(md.surv)

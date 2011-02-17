library(partDSA)

ident <- read.table("dataset.txt", sep="\t", header=TRUE)

set.seed(6442)

control <- DSA.control(vfold=1, minbuck=10, cut.off.growth=10,
                       leafy=1, leafy.num.trees=5,
                       leafy.random.num.variables.per.split=3)

x <- data.frame(ident[,c(1:8)])[1:(500),]
y <- as.factor(ident$cens)[1:500]
wt <- rep(1, 500)

x.test <- data.frame(ident[,c(1:8)])[501:686,]
y.test <- as.factor(ident$cens)[501:686]
wt.test <- rep(1, 186)

results <- partDSA(x, y, wt, x.test, y.test, wt.test, control=control)
print(results)

predictions3 <- predict(results, x.test, y.test, wt.test)
print(predictions3)

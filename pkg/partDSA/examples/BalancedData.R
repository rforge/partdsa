library(partDSA)

x <- matrix(rnorm(500), 100, 5)
y <- as.factor(as.numeric(x[,1] + x[,2] + rnorm(100,0,0.3) > 0))

control <- DSA.control(vfold=1, minbuck=10, cut.off.growth=10, loss.function="gini",
	               leafy=1, leafy.num.trees=3,
                       leafy.random.num.variables.per.split=3)
results <- partDSA(x, y, control=control)
print(results)

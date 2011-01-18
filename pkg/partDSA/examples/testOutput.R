library(partDSA)

# 1) notice how the model coefficients have an intercept term and then
#    that last partition has a value 0 - seems like the intercept
#    should be added to each partition and then removed

x <- data.frame(matrix(rnorm(200), 50, 4) )
x[,2] <- as.numeric(x[,2] > 0)
x[sample(1:50, 10), 2] <- 2
y <- x[,1]^2 / 4 + x[,2]^2 + rnorm(50, 0, .25)
x[,3] <- as.factor(x[,3] > 0)
x[,2] <- as.factor(x[,2])
x[,2] <- as.factor(ifelse(x[,2]==0, "a", ifelse(x[,2]==1, "b", "c")))
x[sample(1:50, 10), 1] <- NA
x[sample(1:50, 10), 2] <- NA
x[sample(1:50, 10), 3] <- NA
x[sample(1:50, 10), 4] <- NA
model <- partDSA(data.frame(x), y, control=DSA.control(missing="default"))
model

# 2) notice how thecoefficients are 1,2,3 instead of a,b,c

x <- matrix(rnorm(300), 100, 3)
y <- as.factor(sample(c("a", "b", "c"), 100, TRUE) )
x.test <- matrix(rnorm(150), 50, 3)
y.test <- as.factor(sample(c("a", "b", "c"), 50, TRUE))
model <- partDSA(x, y)
model

# 3) can we change the name of the variables in the variable importance
#    to reflect the names of the variables input by the user?

x <- data.frame(matrix(rnorm(150), 50, 3))
y <- x[,1]^2 + x[,2]^2 + rnorm(50)
names(x) <- c("A", "B", "C")
model <- partDSA(x, y)
model

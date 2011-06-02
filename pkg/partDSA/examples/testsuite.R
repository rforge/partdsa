# Test suite for the partDSA package.  

library(partDSA)

x <- matrix(rnorm(500),50,10)
y <- rnorm(50)
partDSA(x, y)

y <- as.factor(sample(0:1,50,replace=T))
partDSA(x, y)

x <- matrix(rnorm(500),50,10)
y <- x[,1]^2+x[,2]^2+x[,3]^2
partDSA(data.frame(x), y)

x <- matrix(rnorm(500),50,10)
y <- x[,1]^2+x[,2]^2+x[,3]^2
partDSA(data.frame(x), y, control=DSA.control(missing="no"))

x <- matrix(rnorm(500),50,10)
y <- x[,1]^2+x[,2]^2+x[,3]^2
y <- as.factor(as.numeric(y>2))
x.test <- matrix(rnorm(500),50,10)
y.test <- x[,1]^2+x[,2]^2+x[,3]^2
y.test <- as.factor(as.numeric(y.test>2))
partDSA(data.frame(x), y, wt=rep(1,nrow(x)),
        data.frame(x.test), y.test, wt.test=rep(1,nrow(x.test)))

x <- matrix(rnorm(500),50,10)
y <- x[,1]^2+x[,2]^2+x[,3]^2+rnorm(50,0,.25)
x[sample(1:50,10),1] <- NA
x[sample(1:50,10),3] <- NA
partDSA(data.frame(x), y, control=DSA.control(missing="impute.at.split"))

x <- data.frame(matrix(rnorm(500),50,10))
x[,2] <- as.numeric(x[,2]>0)
y <- x[,1]^2+x[,2]^2+x[,3]^2+rnorm(50,0,.25)
x[,7] <- as.factor(x[,7]>0)
x[,2] <- as.factor(x[,2])
x[,2] <- as.factor(ifelse(x[,2]==0,"a","b") )
x[sample(1:50,10),1] <- NA
x[sample(1:50,10),3] <- NA
x[sample(1:50,10),5] <- NA
x[sample(1:50,10),7] <- NA
partDSA(data.frame(x), y, control=DSA.control(missing="impute.at.split"))

x <- data.frame(matrix(rnorm(500),50,10) )
x[,2] <- as.numeric(x[,2]>0)
y <- x[,1]^2/4+x[,2]^2+rnorm(50,0,.25)
x[,7] <- as.factor(x[,7]>0)
x[,2] <- as.factor(x[,2])
x[,2] <- as.factor(ifelse(x[,2]==0,"a","b") )
x[sample(1:50,10),1] <- NA
x[sample(1:50,10),3] <- NA
x[sample(1:50,10),5] <- NA
x[sample(1:50,10),7] <- NA
partDSA(data.frame(x), y, control=DSA.control(missing="impute.at.split"))

x <- data.frame(matrix(rnorm(500),50,10) )
x[,2] <- as.numeric(x[,2]>0)
x[sample(1:50,15),] <- 2
y <- x[,1]^2/4+x[,2]^2+rnorm(50,0,.25)
x[,7] <- as.factor(x[,7]>0)
x[,2] <- as.factor(x[,2])
x[,2] <- as.factor(ifelse(x[,2]==0,"a","b") )
x[sample(1:50,10),1] <- NA
x[sample(1:50,10),3] <- NA
x[sample(1:50,10),5] <- NA
x[sample(1:50,10),7] <- NA
partDSA(data.frame(x), y, control=DSA.control(missing="impute.at.split"))

x <- data.frame(matrix(rnorm(500),50,10) )
x[,2] <- as.numeric(x[,2]>0)
x[sample(1:50,10),2] <- 2
y <- x[,1]^2/4+x[,2]^2+rnorm(50,0,.25)
x[,7] <- as.factor(x[,7]>0)
x[,2] <- as.factor(x[,2])
x[,2] <- as.factor(ifelse(x[,2]==0,"a",ifelse(x[,2]==1,"b","c")))
x[sample(1:50,10),1] <- NA
x[sample(1:50,10),3] <- NA
x[sample(1:50,10),5] <- NA
x[sample(1:50,10),7] <- NA
partDSA(data.frame(x), y, control=DSA.control(missing="impute.at.split"))

x <- data.frame(matrix(rnorm(500),50,10) )
x[,2] <- as.numeric(x[,2]>0)
x[sample(1:50,10),2] <- 2
y <- x[,1]^2/2+x[,2]^2+rnorm(50,0,.25)
y <- as.numeric(y>1)
y[sample(1:50,10)] <- 2
y <- as.factor(y)
x[,7] <- as.factor(x[,7]>0)
x[,2] <- as.factor(x[,2])
x[,2] <- as.factor(ifelse(x[,2]==0,"a",ifelse(x[,2]==1,"b","c")))
x[sample(1:50,10),1] <- NA
x[sample(1:50,10),3] <- NA
x[sample(1:50,10),5] <- NA
x[sample(1:50,10),7] <- NA
partDSA(data.frame(x), y, control=DSA.control(missing="impute.at.split"))

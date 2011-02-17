library(partDSA)
library(MASS)
library(foreach)

set.seed(6442)

n <- nrow(Boston)
tr.n <- floor(n / 2)
ts.n <- n - tr.n

train.index <- sample(1:n, tr.n, replace=FALSE)
test.index <- c(1:n)[-train.index]
vars <- Boston[,-14]
outcome <- Boston[,14]

x <- vars[train.index,]
y <- outcome[train.index]
wt <- rep(1, tr.n)

x.test <- vars[test.index,]
y.test <- outcome[test.index]
wt.test <- rep(1, ts.n)

results <- partDSA(x, y, wt, x.test, y.test, wt.test, control=DSA.control(cut.off.growth=5))
print(results$tablist)

idx <- foreach(BFs=results$IkPn) %do% {
  test <- partDSA:::assign.obs.to.bf(dat2=x, n=nrow(x), p=ncol(x), BFs=BFs)
  idx <- foreach(r=iter(test, by='row'), .combine='c') %do% {
    i <- which(r != 0)
    if (length(i) != 1) {
      stop('unexpected output from assign.obs.to.bf')
    }
    i
  }
  factor(idx)
}

fun <- function(BFs) {
  test <- partDSA:::assign.obs.to.bf(dat2=x, n=nrow(x), p=ncol(x), BFs=BFs)
  i <- unlist(lapply(seq(length=nrow(test)), function(irow) which(test[irow,] != 0)))
  factor(i)
}
idx <- lapply(results$IkPn, fun)

print(idx)

p <- predict(results, newdata=x)
# print(p)

fun1 <- function(vals) list(mean=mean(vals), sd=sd(vals), values=vals, length=length(vals))
fun2 <- function(vals) list(values=vals, length=length(vals))
fun3 <- function(preds) list(predicted=preds[1])

for (cut.off in seq(along=results$IkPn)) {
  cat(sprintf('cut.off.growth = %d\n', cut.off))

  if (results$options$outcome.class == "factor") {
    mean.sd.y <- tapply(y, idx[[cut.off]], fun2, simplify=FALSE)
    predicted <- tapply(p[,cut.off], idx[[cut.off]], fun3, simplify=FALSE)
    summary.part <- mapply('c', mean.sd.y, predicted, SIMPLIFY=FALSE, USE.NAMES=FALSE)
    print(summary.part)
  } else {
    mean.sd.y <- tapply(y, idx[[cut.off]], fun1, simplify=FALSE)
    predicted <- tapply(p[,cut.off], idx[[cut.off]], fun3, simplify=FALSE)
    summary.part <- mapply('c', mean.sd.y, predicted, SIMPLIFY=FALSE, USE.NAMES=FALSE)
    print(summary.part)
  }
  cat('\n')
}

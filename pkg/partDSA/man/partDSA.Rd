\name{partDSA}
\alias{partDSA}
\alias{DSA.control}
\title{partDSA}
\description{
\emph{partDSA} is a novel tool for generating a piecewise constant
estimation sieve of candidate estimators based on an intensive and
comprehensive search over the entire covariate space. The strength of
this algorithm is that it builds \emph{'and'} and \emph{'or'}
statements. This allows combinations and substitutions of regions for
the purpose of discovering intricate correlations patterns and
interactions in addition to main effects. Depending on the application,
this approach will supersede methods such as CART by being not only
more aggressive but also more flexible. As such, \emph{partDSA} provides
the user an additional tool for their statistical toolbox.
}
\usage{
	
partDSA(x, y, wt=rep(1, nrow(x)), x.test=x, y.test=y, wt.test,
        control=DSA.control(), sleigh)
DSA.control(vfold=10, minbuck=6, cut.off.growth=10, MPD=0.1,
            missing="impute.at.split", loss.function="default",
            wt.method="KM", brier.vec=NULL,
            leafy=0, leafy.random.num.variables.per.split=4,
            leafy.num.trees=50, leafy.subsample=0, save.input=FALSE, 
            cox.vec=NULL, IBS.wt=NULL)
}
\arguments{
\item{x}{The matrix or data frame of predictor variables for the training set,
 used to build the model.
 Each row corresponds to an observation, and each column corresponds to
 a variable.}
\item{y}{The outcome (response) vector, either continuous or categorical,
 representing the true response values for observations in \code{x}.
 The length of this vector should equal the number of rows in \code{x}.}
\item{wt}{Optional vector  of training weights with length equal to the
 number of observations in \code{x}.
 Default is a vector of ones with length equal to the number of training
 set observations.}
\item{x.test}{The matrix or data frame of predictor variables used to
 build the model.
 The number of columns (variables) of \code{x.test} should equal the number
 of columns as \code{x}.
 The default is \code{x}.}
\item{y.test}{The outcome (response) vector, either continuous or categorical,
 representing the true response values for observations in \code{x.test}.
 The length of this vector should equal the number of rows in \code{x.test}.
 The default value is \code{y}.}
\item{wt.test}{Optional vector of test weights with length equal to the
 number of test set observations.
 Default value is \code{wt} if \code{x.test} wasn't specified, otherwise
 it is a vector of ones with length  equal to the number of test
 set observations.}
\item{control}{A list object used to specify additional control parameters.
 This is normally created by calling the \code{DSA.control} function.
 Default value is the result of calling \code{DSA.control} with no arguments.}
\item{sleigh}{Optional \code{sleigh} object to allow the cross-validation
 to be performed in parallel using the \code{nws} package.
 If not specified, the cross-validation will be executed sequentially.}
\item{vfold}{The number of folds of cross-validation for the model
 building process.
 The default value is 10.}
\item{minbuck}{The minimum number of observations in any terminal partition.
 The default value is 6.}
\item{cut.off.growth}{The maximum number of terminal partitions to be
 considered when building the model.
 The default value is 10.}
\item{MPD}{Minimum Percent Difference.
 The model fit must improve by this percentage in order to be considered.
 This saves time in the model building process.
 The default value is 0.1.}
\item{missing}{Character string specifying how missing data should be
 handled.  The default value is "no." See the details section from more information.}
\item{loss.function}{The function to be minimized when building the model.
 For categorical outcomes, "entropy" (default) or "gini" can be specified.
 For continuous outcomes, the L2 loss function is used.}
\item{wt.method}{Not documented yet.}
\item{brier.vec}{Not documented yet.}
\item{cox.vec}{Not documented yet.}
\item{IBS.wt}{Not documented yet.}
\item{leafy}{Not documented yet.}
\item{leafy.random.num.variables.per.split}{Not documented yet.}
\item{leafy.num.trees}{Not documented yet.}
\item{leafy.subsample}{Numeric value between 0 and 1.  The value 0 is
  used for bootstrap sampling (sampling witht replacement).  If the
  value is greater than 0, it corresponds to the proportion of samples
  used to build the model, such as 0.632.  The default is 0.}
\item{save.input}{Indicates if \code{x} and \code{y} should be saved
 in the object returned by partDSA.  If \code{FALSE}, \code{x} and \code{y}
 are set to \code{NULL}.  The default value is \code{FALSE}.}
}
\details{
\code{missing} set to "no" indicates that there is no missing data and
will create an error if missing data is found in the dataset. Setting missing="impute.at.split" will use a data
imputation method similar to that in CRUISE (Kim and Loh, 2001). At each
split, the non-missing observations for a given variable will be used
to find the best split, and the missing observations will be imputed
based on the mean or mode (depending on whether the variable is
categorical or continuous) of the non-missing observations in that node.
Once the node assignment of these missing observations is determined
using the imputed values, the imputed values are returned to their
missing status. For missing values in the test set, the grand mean or
mode from the corresponding variables in the training set are used.
Including variables which are entirely missing will result in an error.
}
\examples{
library(MASS)
set.seed(6442)

n <- nrow(Boston)
tr.n <- floor(n / 2)
train.index <- sample(1:n, tr.n, replace=FALSE)
test.index <- (1:n)[-train.index]

x <- Boston[train.index, -14]
y <- Boston[train.index, 14]
x.test <- Boston[test.index, -14]
y.test <- Boston[test.index, 14]

control <- DSA.control(vfold=1)  # no cross-validation
partDSA(x, y, x.test=x.test, y.test=y.test, control=control)
}

\keyword{utilities}

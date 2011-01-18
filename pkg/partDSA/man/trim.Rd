\name{trim}
\alias{trim}
\alias{trim.partDSA}
\alias{trim.dsa}
\title{trim}
\description{
\code{trim} is used to trim, or prune, the model object
returned by the \code{partDSA} function.
}
\usage{
trim(object, \dots)
\method{trim}{partDSA}(object, cut.off.growth, \dots)
\method{trim}{dsa}(object, cut.off.growth, \dots)
}
\arguments{
\item{object}{\code{partDSA} or \code{dsa} object to be trimmed.}
\item{cut.off.growth}{number of the level to trim to.}
\item{\dots}{unused.}
}

\keyword{utilities}

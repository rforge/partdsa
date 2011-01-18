\name{showDSA}
\alias{showDSA}
\alias{showDSA.dsa}
\alias{showDSA.character}
\title{showDSA}
\description{
\code{showDSA} is a convenience function that starts the Java
visualization program (included in the partDSA package) to display
information about the model created by \code{partDSA} function.
There are two methods: one taking a partDSA/dsa object, the
other taking the name of a visualization file created via the
\code{dumpDSA} function.
}
\usage{
showDSA(x, javacmd=getOption("javacmd"), quietly=FALSE, \dots)
\method{showDSA}{dsa}(x, javacmd=getOption("javacmd"), quietly=FALSE, \dots)
\method{showDSA}{character}(x, javacmd=getOption("javacmd"), quietly=FALSE, wait=FALSE, \dots)
}
\arguments{
\item{x}{\code{dsa} object or the name of the visualization file to be shown.}
\item{javacmd}{A character string giving the path of the Java interpreter.
  Defaults to \code{getOption("javacmd")}, which is not set by default,
  in which case, "java" is used.}
\item{quietly}{A logical value indicating if messages should be suppressed.
  Defaults to \code{FALSE}.}
\item{wait}{A logical value indicating if the R session should wait until the
  the Java program exits before continuing.
  Defaults to \code{FALSE}.}
\item{\dots}{Not currently used.}
}

\note{
Java 1.5 or greater is required to run these functions, although Java 1.6
is recommended.  The Java interpreter should be in the command search
path, unless either the \code{javacmd} option is set, or the
\code{javacmd} argument is specified.
}

\keyword{utilities}

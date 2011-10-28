DSA.control <- function(vfold=10, minbuck=6, cut.off.growth=10, MPD=0.1,
                        missing="impute.at.split", loss.function="default",
                        wt.method="KM", brier.vec=NULL,
                        leafy=0, leafy.random.num.variables.per.split=4,
                        leafy.num.trees=50, save.input=FALSE, cox.vec=NULL, IBS.wt=NULL) {
  if (leafy == 1 && (!is.wholenumber(leafy.num.trees)))
    stop('The number of trees must be a whole number')

  if (leafy == 1 && (!is.wholenumber(leafy.random.num.variables.per.split)))
    stop('The number of random variables per split must be a whole number')

  if (leafy == 1 && (leafy.num.trees <= 0))
    stop('The number of trees must be greater than 0')

  if (leafy == 1 && (leafy.random.num.variables.per.split <= 0))
    stop('The number of random variables per split must be greater than 0')

  if (!is.numeric(vfold) || length(vfold) > 1 || vfold < 1)
    stop('vfold must be a numeric value >= 1')

  if (!is.numeric(minbuck) || length(minbuck) > 1 || minbuck < 1)
    stop('minbuck must be a numeric value >= 1')

  if (!is.numeric(cut.off.growth) || length(cut.off.growth) > 1 ||
      cut.off.growth < 1)
    stop('cut.off.growth must be a numeric value >= 1')

  if (!is.numeric(MPD) || length(MPD) > 1 || MPD < 0)
    stop('MPD must be a non-negative numeric value')

  if (!is.character(missing) || length(missing) > 1)
    stop('missing must be a character value')

  if (!is.character(loss.function) || length(loss.function) > 1)
    stop('loss.function must be a character value')

  if (!is.character(wt.method) || length(wt.method) > 1)
    stop('wt.method must be a character value')

  if (!(is.null(brier.vec) || is.numeric(brier.vec)))
    stop('brier.vec must be numeric')
	
 if(!(is.null(cox.vec) || is.numeric(cox.vec)))
      stop('cox.vec must be numeric')

  if(!(is.null(IBS.wt) || is.numeric(IBS.wt)))
      stop('IBS.wt must be numeric')

 
  if (!is.logical(save.input))
    stop('save.input must be logical')

  list(vfold=vfold, minbuck=minbuck, cut.off.growth=cut.off.growth,
       MPD=MPD, missing=missing, loss.function=loss.function,
       wt.method=wt.method, brier.vec=brier.vec, cox.vec=cox.vec, IBS.wt=IBS.wt, leafy=leafy, 
       leafy.random.num.variables.per.split=leafy.random.num.variables.per.split,
       leafy.num.trees=leafy.num.trees, save.input=save.input)
}

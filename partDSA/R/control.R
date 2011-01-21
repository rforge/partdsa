DSA.control <- function(vfold=10, minbuck=6, cut.off.growth=10, MPD=0.1, missing="impute.at.split", loss.function="default", leafy = 0, leafy.random.num.variables.per.split = 4, leafy.num.trees = 50) {
  
  if(leafy == 1 && (!is.wholenumber(leafy.num.trees)))
  	stop('The number of trees must be a whole number')
	
  if(leafy == 1 && (!is.wholenumber(leafy.random.num.variables.per.split)))
  	stop('The number of random variables per split must be a whole number')
  	
  if(leafy == 1 && (leafy.num.trees<= 0))
  	stop('The number of trees must be greater than 0')
  
  if(leafy == 1 && (leafy.random.num.variables.per.split<= 0))
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

  list(vfold=vfold, minbuck=minbuck, cut.off.growth=cut.off.growth,
       MPD=MPD, missing=missing, loss.function=loss.function, leafy = leafy, leafy.random.num.variables.per.split = leafy.random.num.variables.per.split, leafy.num.trees = leafy.num.trees)
}

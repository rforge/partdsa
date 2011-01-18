DSA.control <- function(vfold=10, minbuck=6, cut.off.growth=10, MPD=0.1,
                        missing="no", loss.function="default") {
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
       MPD=MPD, missing=missing, loss.function=loss.function)
}

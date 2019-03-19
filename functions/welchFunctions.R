# TODO: Welch test (kind of ANOVA) based on https://statistics.berkeley.edu/computing/r-t-tests
# 
# Author: el-sharkawy
###############################################################################


kFoldWelch <- function(df, metricsColumn, labelsColumn, nSamples = 1000) {
	goods <- df[metricsColumn][df[labelsColumn]==0, ]
	bads  <- df[metricsColumn][df[labelsColumn]==1, ]
	
	ts = replicate(nSamples, t.test(goods, bads)$statistic)
	return(ts)
}

welch <- function(df, metricsColumn, labelsColumn) {
	goods <- df[metricsColumn][df[labelsColumn]==0, ]
	bads  <- df[metricsColumn][df[labelsColumn]==1, ]
	
	result <- t.test(goods, bads)
	return(result)
}

plotKFoldWelch <- function(ts, degreeFreedom) {
	min <- range(ts)[1]
	max <- range(ts)[2]
	pts = seq(min, max, length=100)
	
	plot(pts, dt(pts, df=degreeFreedom), col='red', type='l')
	lines(density(ts))
}
# TODO: Add comment
# 
# Author: el-sharkawy
###############################################################################
source("functions/functions.R")
source("functions/datafunctions.R")
source("functions/plotFunctions.R")
source("functions/welchFunctions.R")

# df <- readFromCSV("Sample")
# df$nErrors[df$nErrors > 0] <- 1
# df <- removeIdentifier(df, TRUE)
# ts <- kFoldWelch(df, 2, 1, nSamples=10)
# t <- welch(df,2,1)
# ## print(range(ts)$statistic)
# ## print(t)
# ## 
# 
# ## x <- c(1,2,3,4,5,6,7,8)
# ## y <- c(8,7,6,5,4,3,2,1)
# #print(t.test(x,y))
# 
# #pts = seq(-4.5,4.5,length=100)
# #plot(pts,dt(pts,df=18),col='red',type='l')
# #lines(density(ts))
# 
# plotKFoldWelch(ts, t$parameter)
# print(t)


myTtest <- try(t.test(...))
if (inherits(myTtest, "try-error"))
{
  cat(myTtest)
  myTtest <- NA
}
# TODO: Add comment
# 
# Author: el-sharkawy
###############################################################################
source("functions/functions.R")
source("functions/datafunctions.R")
source("functions/plotFunctions.R")
source("functions/welchFunctions.R")
# col1 = c(0, 0, 1, 0, 1)
# col2 = c(1, 1, 1, 0, 1)
# col3 = c(1, 2, 3, 4, 5)
# fictiveDF <- as.data.frame(cbind(col1, col2, col3))


df <- readFromCSV("Sample", folder="data")
df <- convertToNumbers2(df, TRUE)
df <- removeIdentifier(df, TRUE)
columNames <- names(df)
column = 2
# name = columNames[column]
# plot <- createKernelDensityPlot(df, column, 1, name, scale=TRUE)
# print(plot)
# goods <- df[column][df[1]==0, ]
# m <- max(goods)

# x <- rnorm(12)
# f <- ecdf(x)
# print(f)
# plot(f)
# l <- as.list(environment(f))
# min = min(l$x)
# max = max(l$x)

# print(df)
# 
# result <- kruskalWallis(df, 2, 1)
# print(result)
# grepl("-no0", "ecdf-log-no0")
# plot(c(0), col='blue')


r1 <- cohensD.as.df(df, column, 1, addSummary=TRUE)
# column = 3
# r2 <- cohensD.as.df(df, column, 1)
# r3 <- rbind(r1, r2)
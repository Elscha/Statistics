# TODO: Add comment
# 
# Author: el-sharkawy
###############################################################################
source("functions/functions.R")
source("functions/datafunctions.R")
source("functions/plotFunctions.R")
source("functions/welchFunctions.R")

df <- readFromCSV("Sample", folder="data")
df <- convertToNumbers2(df, TRUE)
df <- removeIdentifier(df, TRUE)
df$nErrors[df$nErrors > 0] <- 1
columNames <- names(df)
column = 100
name = columNames[column]
plot <- createCumlativeDistributionPlot(df, column, 1, name)
print(plot)
# goods <- df[column][df[1]==0, ]
# m <- max(goods)

# x <- rnorm(12)
# f <- ecdf(x)
# print(f)
# plot(f)
# l <- as.list(environment(f))
# min = min(l$x)
# max = max(l$x)
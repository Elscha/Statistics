# TODO: Add comment
# 
# Author: el-sharkawy
###############################################################################
source("functions/functions.R")
source("functions/datafunctions.R")
source("functions/plotFunctions.R")
source("functions/welchFunctions.R")

# df <- readFromCSV("Sample", folder="data")
# df <- convertToNumbers2(df, TRUE)
# df <- removeIdentifier(df, TRUE)
# df$nErrors[df$nErrors > 0] <- 1
# columNames <- names(df)
# column = 100
# name = columNames[column]
# plot <- createCumlativeDistributionPlot(df, column, 1, name)
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

# col1 = c(0, 0, 1, 0, 1)
# col2 = c(1, 1, 1, 0, 1)
# col3 = c(1, 2, 3, 4, 5)
# df <- as.data.frame(cbind(col1, col2, col3))
# print(df)
# 
# df2 <- removeRowsByValue(df, 1, 0)
# print(df2)
# df3 <- removeRowsByValue(df, 2, 1)
# print(df3)
# grepl("-no0", "ecdf-log-no0")
plot(c(0), col='blue')
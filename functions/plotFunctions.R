library(ggplot2)

createVioplotForMetric <- function(df, metricsColumn, labelsColumn, addToExistingPlot=TRUE) {
  library(vioplot)
  
  goods <- df[metricsColumn][df[labelsColumn]==0, ]
  bads  <- df[metricsColumn][df[labelsColumn]==1, ]
  
  vioplot(x1, x2, names=c("Unproblematic", "Errornous"), col=c("#66a3ff","#ff6666"), add=addToExistingPlot)
  name <- colnames(df)[metricsColumn]
  title(paste("Violin Plots of ", name))
  
  return(plot)
}
createVioplotForMetric2 <- function(df, metricsColumn, labelsColumn, trim=TRUE) {
  
  df[[labelsColumn]] <- as.factor(df[[labelsColumn]])
  columNames <- names(df)
  plot <- ggplot(df, aes_string(x = columNames[labelsColumn], y = columNames[metricsColumn], color=columNames[labelsColumn])) + geom_violin(trim = trim)
  # Add Summary values (http://www.sthda.com/english/wiki/ggplot2-violin-plot-quick-start-guide-r-software-and-data-visualization):
  plot <- plot + geom_boxplot(width=0.1)
  plot <- plot + stat_summary(fun.y=mean, geom="point", size=2)#, color="red")
  
  # Change Legend (http://www.sthda.com/english/wiki/ggplot2-legend-easy-steps-to-change-the-position-and-the-appearance-of-a-graph-legend-in-r-software):
  plot <- plot + theme(legend.background = element_rect(fill="lightblue", size=0.5, linetype="solid", colour ="darkblue"))
  
  return(plot)
}

createCumlativeDistributionPlot <- function(df, metricsColumn, labelsColumn, name="ECDF Plot", verticals=TRUE, points=FALSE, scale=FALSE, xMin=-1) {
  # Based on: https://stackoverflow.com/a/20601807
  goods <- df[metricsColumn][df[labelsColumn]==0, ]
  bads  <- df[metricsColumn][df[labelsColumn]>0, ]
  
  if (scale) {
    goods <- log1p(goods)
    bads <- log1p(bads)
  }
  
  maxValue <- max(goods, bads)
  addPlot  <- TRUE
  if (length(goods)) {
    distFuncGoods <- ecdf(goods)    # P is a function giving the empirical CDF of goods
    p <- plot(distFuncGoods, verticals=verticals, do.points=points, col='blue', main=name, xlim=c(xMin, maxValue))
  } else {
    addPlot <- FALSE
  }
  if (length(bads)) {
    distFuncBads  <- ecdf(bads)     # P is a function giving the empirical CDF of bads
    p <- plot(distFuncBads, verticals=verticals, do.points=points, add=addPlot, col='red')
  }
  
  return(p)
}

createKernelDensityPlot <- function(df, metricsColumn, labelsColumn, name="Kernel Density Plot", scale=FALSE) {
  # Based on: https://stackoverflow.com/a/20601807
  
  goods <- df[metricsColumn][df[labelsColumn]==0, ]
  bads  <- df[metricsColumn][df[labelsColumn]>0, ]
  
  if (scale) {
    goods <- log1p(goods)
    bads <- log1p(bads)
  }
  
  densitityGoods <- NULL
  densitityBads <- NULL
  if (length(goods)) {
    densitityGoods <- density(goods)
  }
  if (length(bads)) {
    densitityBads <- density(bads)
  }
  
  if(!is.null(densitityGoods) && !is.null(densitityBads)) {
    minX <- min(densitityGoods$x, densitityBads$x)
    maxX <- max(densitityGoods$x, densitityBads$x)
    minY <- min(densitityGoods$y, densitityBads$y)
    maxY <- max(densitityGoods$y, densitityBads$y)
  } else if (!is.null(densitityGoods)) {
    minX <- min(densitityGoods$x)
    maxX <- max(densitityGoods$x)
    minY <- min(densitityGoods$y)
    maxY <- max(densitityGoods$y)
  } else if (!is.null(densitityBads)) {
    minX <- min(densitityBads$x)
    maxX <- max(densitityBads$x)
    minY <- min(densitityBads$y)
    maxY <- max(densitityBads$y)
  } else {
    minX <- -1
    maxX <- 1
    minY <- 0
    maxY <- 1
  }
  
  addPlot  <- TRUE
  if (length(goods)) {
    p <- plot(densitityGoods, col='dodgerblue4', xlim=c(minX, maxX), ylim=c(minY, maxY), main=name)
  } else {
    addPlot <- FALSE
  }
  if (length(bads)) {
    densitityBads <- density(bads)     # returns the density data
    if(addPlot) {
      # based on: https://stackoverflow.com/a/6939220
      lines(densitityBads, col='red')
    } else {
      p <- plot(densitityBads, add=addPlot, col='firebrick3', xlim=c(minX, maxX), ylim=c(minY, maxY), main=name)
    }
  }
  
  xTextPos <- minX
  goodText <- paste(length(goods), "Healthy Functions")
  badText  <- paste(length(bads), "Erroneous Functions")
  text(x = xTextPos, y = maxY, goodText, col = "dodgerblue4", cex = 0.9, adj = c(0, NA))
  text(x = xTextPos, y = maxY*.95, badText, col = "firebrick3", cex = 0.9, adj = c(0, NA))
  
  return(p)
}

savePlot <- function(plot, folder, file="Plot.png") {
  ggsave(paste(folder, "/", file, sep=""), plot = plot)
}

# columnNames <- names(mtcars)
# for (column in 2:ncol(mtcars)) {
#     p <- createVioplotForMetric2(mtcars, 1, column)
#     #print(columnNames[column])
#     name <- paste("VarianceAnalysis-", columnNames[column], ".png", sep="")
#     savePlot(p, "data", file=name)
# }

# head(mtcars)
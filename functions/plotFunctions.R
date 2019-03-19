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
  library(ggplot2)
  
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
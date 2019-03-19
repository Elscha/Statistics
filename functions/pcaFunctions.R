plotPCA <- function(pcaData, fileName=NULL) {
  if (!is.null(fileName)) {
    png(paste("out/", fileName, ".png", sep=""))
    plot(pcaData, type = "l")
    dev.off()
  } else {
    plot(pcaData, type = "l")
  }
}

pcaAnalysis <- function(df, fileName) {
  pcaResult <- prcomp(df[-1], center = TRUE, scale. = TRUE)
  plotPCA(pcaResult, fileName)
  
  linCombinations <-summary(pcaResult)$rotation
  importance      <- summary(pcaResult)$importance
  
  # Eigenvector > 1 means that PC is useful and explains something: https://www.youtube.com/watch?v=3wmaPTL-fhM
  eigenvectors <- (pcaResult$sdev)^2
  
  model <- rbind(linCombinations, importance, eigenvectors)
  
  return(model)
}
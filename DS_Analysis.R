#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
source("functions/functions.R")
source("functions/datafunctions.R")
source("functions/plotFunctions.R")
source("functions/welchFunctions.R")

# Libary for SQL on data frames: http://dept.stat.lsa.umich.edu/~jerrick/courses/stat701/notes/sql.html
library(sqldf)
#library(ddply)

linearRegression <- function(df) {
  # cf. https://stackoverflow.com/a/11991742
  return(fit <- lm(nErrors ~ ., data = df))
}

linearRegressionComplex <- function(df) {
  f <- createAllCombinationsFormula(names(df))
  fit <- lm(formula = f, data = df)
  return(fit)
}

singleFoldLinearyRegression <- function(df) {
  # See: https://www.r-bloggers.com/how-to-perform-a-logistic-regression-in-r/
  df$nErrors[df$nErrors > 0] <- 1
  
  nValues <- nrow(df)
  splitRow <- as.integer(nValues * 0,8)
  train <- df[1:splitRow,]
  test <- df[(splitRow +1):nValues,]
  
  model <- glm(nErrors ~ ., data = train, family=binomial(link='logit'))
  
  fitted.results   <- predict(model,newdata=test ,type='response')
  fitted.results   <- ifelse(fitted.results > 0.5,1,0)
  misClasificError <- mean(fitted.results != test$nErrors)
  accuracy         <- (1-misClasificError)
  print(paste('Accuracy ',accuracy))
  
  return(accuracy)
}

pickRandomlySameAmount <- function(df) {
  goods <- sqldf("SELECT * FROM df where nErrors = 0 ORDER BY RANDOM() LIMIT (SELECT COUNT (*) as nValues FROM df where nErrors > 0)")
  bads  <- sqldf("SELECT * FROM df where nErrors > 0")
  # Concatenate both frames: https://stackoverflow.com/a/8365050
  df <- rbind(goods, bads)
  #Shuffle merged frame: https://stackoverflow.com/a/11503439
  df <- df[sample(nrow(df)),]
  #nValues <- sqldf("SELECT COUNT (*) as nValues FROM df where nErrors > 0")$nValues[1];
  
  return(df)
}

kFoldLinearyRegression <- function(df, k = 10, testSize = 0.2) {
  df <- pickRandomlySameAmount(df)
  # See: https://www.r-bloggers.com/how-to-perform-a-logistic-regression-in-r/
  df$nErrors[df$nErrors > 0] <- 1
  nValues                    <- nrow(df)
  
  # cf. https://stackoverflow.com/a/28196342
  errList <- df$nErrors == 1
  nErrors <- sum(errList)
  print(paste(nErrors, " of ", nValues, " are errors", sep=""))
  
  stepSize <- as.integer(nValues / k)
  intSize  <- as.integer(nValues * testSize)
  
  result <- list()
  accMean <- 0
  
  for (i in 1:k) {
    firstCut  <- as.integer(stepSize * (i - 1) + 1)
    secondCut <- min(as.integer(firstCut + intSize), nValues)
    
    train         <- df[-c(firstCut:secondCut), ]
    test          <- df[c(firstCut:secondCut), ]
   
    model <- glm(nErrors ~ ., data = train, family=binomial(link='logit'))
    
    fitted.results   <- predict(model,newdata=test ,type='response')
    fitted.results   <- ifelse(fitted.results > 0.5, 1, 0)
    misClasificError <- mean(fitted.results != test$nErrors)
    
    accuracy <- (1-misClasificError)
    result   <- append(result, accuracy)
    accMean  <- accMean + accuracy
    print(paste('Accuracy', accuracy))
  }
  
  accMean <- (accMean / k)
  print(paste('Total Accuracy', accMean))
  
  return(result)
}

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

mergeFileDFs <- function(listOfDFs) {
  # See: https://stackoverflow.com/a/47950810 & https://blog.zhaw.ch/datascience/r-reduce-applys-lesser-known-brother/
  df <- Reduce(function(x,y) merge(x,y, by="File"), listOfDFs)
  
  return(df)
}

mergeFunctionDFs <- function(listOfDFs) {
  # See: https://stackoverflow.com/a/47950810 & https://blog.zhaw.ch/datascience/r-reduce-applys-lesser-known-brother/
  # Also use function to avoid duplicated column
  df <- Reduce(function(x,y) merge(x,y, by=c("File", "Function", "Line")), listOfDFs)
  
  return(df)
}

analysis <- function(df, isFunctionBased, type, dataName) {
  df <- convertToNumbers2(df, isFunctionBased)
  analysisName <- paste("CorrelationResults (", type, ") for ", dataName, sep="")
  
  p <- NULL
  model <- NULL
  if (type == "lm") {
    df <- removeIdentifier(df, isFunctionBased)
    df <- normalizeData(df)
    model <- linearRegression(df)
    print(model)
    print(summary(model))
    
    # Pullout p-Value: https://stackoverflow.com/a/5587781f
    f <- summary(model)$fstatistic
    p <- pf(f[1],f[2],f[3],lower.tail=F)
    model <- summary(model)$coefficients
  } else if (type == "lm2") {
    df <- removeIdentifier(df, isFunctionBased)
    df <- normalizeData(df)
    model <- linearRegressionComplex(df)
    #print(model)
    #print(summary(model))
    
    # Pullout p-Value: https://stackoverflow.com/a/5587781f
    f <- summary(model)$fstatistic
    p <- pf(f[1],f[2],f[3],lower.tail=F)
    model <- summary(model)$coefficients
  } else if (type == "pca-linear") {
    df <- removeIdentifier(df, isFunctionBased)
    df <- normalizeData(df)
    model <- pcaAnalysis(df, paste("PCA-", dataName, sep=""))
  } else if (type == "pca-log") {
    df <- removeIdentifier(df, isFunctionBased)
    df <- normalizeData_loc(df)
    model <- pcaAnalysis(df, paste("PCA-", dataName, sep=""))
    
    importanceIndex <- (nrow(model) - 1)
    result <- filterPCAResult(model, 4, 0.8, importanceIndex, 0.95)
    usefulMetrics <- as.data.frame(result)
    saveResults("Usefulmetrics", usefulMetrics)
  } else if (type == "glm") {
    df <- removeIdentifier(df, isFunctionBased)
    df <- normalizeData(df)
    model <- singleFoldLinearyRegression(df)
  } else if (type == "glm-k") {
    df <- removeIdentifier(df, isFunctionBased)
    df <- normalizeData(df)
    model <- kFoldLinearyRegression(df)
  } else if (type == "mergePCs") {
    names <- readFromCSV("UsefulMetrics")
    df <- normalizeColumnNames(df)
    model <- applyPCs(df, names)
    
    analysisName <- "RelevantMetricsData"
  } else if (type == "mergeOnly") {
    analysisName <- "AllMetricsData"
    model <- normalizeColumnNames(df)
  } else if (type == "VisDif") {
    df$nErrors[df$nErrors > 0] <- 1
    df <- removeIdentifier(df, isFunctionBased)
    columnNames <- names(df)
    for (column in 2:ncol(df)) {
      p <- createVioplotForMetric2(df, column, 1)
      name <- paste("VarianceAnalysis-", columnNames[column], ".png", sep="")
      savePlot(p, "out", file=name)
    }
    model <- NULL
  } else if (type == "anova") {
    df$nErrors[df$nErrors > 0] <- 1
    df <- removeIdentifier(df, isFunctionBased)
    columnNames <- names(df)
    
    analysisName <- "ANOVA"
    model <- data.frame()
    for (column in 2:ncol(df)) {
      print(paste("Process:", columnNames[column]))
      tmp   <- varianceTest(df, column, 1)
      model <- rbind(model, tmp)
    }
    print("Finished")
  }
  
  if (!is.null(model)) {
    saveResults(analysisName, model, p)
  }
}

readAndAnalyse <- function(listOfFileNames, isFunctionBased, type) {
  name <- listOfFileNames[1]
  # df <- readFromCSV(listOfFileNames[1], folder="data/atomic_full")
  df <- readFromCSV(listOfFileNames[1], folder="data")
  
  if (length(listOfFileNames) > 1) {
    for (f in listOfFileNames[-1]) {
      print(paste("Process:", f))
      newItem <- readFromCSV(f, folder="data/atomic_full")
      
      # see: https://stackoverflow.com/a/17579145
      df <- merge(df, newItem)[, union(names(df), names(newItem))]
    }
  }
  
  analysis(df, isFunctionBased, type, name)
}

if (length(args) == 0) {
  #stop("Please specify filename.", call.=FALSE)
  readAndAnalyse(c("Sample"), TRUE, "VisDif")
} else {
  # See https://stackoverflow.com/a/26692756
  vargs <- strsplit(args, ",")
  readAndAnalyse(vargs[1], as.logical(vargs[2]), vargs[3])
}
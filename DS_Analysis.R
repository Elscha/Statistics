#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
source("functions/functions.R")
source("functions/dataFunctions.R")
source("functions/binClassificationFunctions.R")
source("functions/pcaFunctions.R")
source("functions/plotFunctions.R")
source("functions/welchFunctions.R")

linearRegres <- function(df, isFunctionBased) {
  df <- removeIdentifier(df, isFunctionBased)
  df <- normalizeData(df)
  model <- linearRegression(df)
  print(model)
  print(summary(model))
  
  return(model)
}

linearRegresComplex <- function(df, isFunctionBased) {
  df <- removeIdentifier(df, isFunctionBased)
  df <- normalizeData(df)
  model <- linearRegressionComplex(df)
  
  return(model)
}

pcaAnalyses <- function(df, isFunctionBased, type) {
  df <- removeIdentifier(df, isFunctionBased)
  if (type == "pca-linear") {
    df <- normalizeData(df)
  } else if (type == "pca-log") {
    df <- normalizeData_loc(df)
  }
  
  model <- pcaAnalysis(df, paste("PCA-", dataName, sep=""))
  
  importanceIndex <- (nrow(model) - 1)
  result <- filterPCAResult(model, 4, 0.8, importanceIndex, 0.95)
  usefulMetrics <- as.data.frame(result)
  saveResults("Usefulmetrics", usefulMetrics)
  
  return(model)
}

glmAnalysis <- function(df, isFunctionBased, type) {
  df <- removeIdentifier(df, isFunctionBased)
  df <- normalizeData(df)
  
  if (type == "glm") {
    model <- singleFoldLinearyRegression(df)
  } else if (type == "glm-k") {
    model <- kFoldLinearyRegression(df)
  }
  
  return(model)
}

welchAnalysis <- function(df, isfunctionBased) {
  df$nErrors[df$nErrors > 0] <- 1
  df <- removeIdentifier(df, isFunctionBased)
  columnNames <- names(df)
  
  model <- data.frame()
  for (column in 2:ncol(df)) {
    print(paste("Process:", columnNames[column]))
    t <- welch(df, column, 1)
    data = c(columnNames[column], t$p.value, t$statistic, t$estimate, t$method, t$parameter)
    model <- rbind(model, data)
  }
  colnames(model) <- c("Metric", "p-Value", "t-Value",  "Mean of Healthies", "Mean of Errornous.", "Test Method", "Degrees of Freedom")
  print("Finished")
  
  return(model)
}

analysis <- function(df, isFunctionBased, type, dataName) {
  df <- convertToNumbers2(df, isFunctionBased)
  analysisName <- paste("CorrelationResults (", type, ") for ", dataName, sep="")
  
  p <- NULL
  model <- NULL
  if (type == "lm") {
    model <- linearRegres(df, isFunctionBased)
    
    # Pullout p-Value: https://stackoverflow.com/a/5587781f
    f <- summary(model)$fstatistic
    p <- pf(f[1],f[2],f[3],lower.tail=F)
    model <- summary(model)$coefficients
  } else if (type == "lm2") {
    model <- linearRegresComplex(df, isFunctionBased)
    
    # Pullout p-Value: https://stackoverflow.com/a/5587781f
    f <- summary(model)$fstatistic
    p <- pf(f[1],f[2],f[3],lower.tail=F)
    model <- summary(model)$coefficients
  } else if (type == "pca-linear" || type == "pca-log") {
    model <- pcaAnalyses(df, isFunctionBased, type)
  } else if (type == "glm" || type == "glm-k") {
    model <- glmAnalysis(df, isFunctionBased, type)
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
  } else if (type == "welch") {
    model <- welchAnalysis(df, isFunctionBased)
  } else {
    print("Please specify one of the following arguments:")
    print("- lm: Linear regression, all metrics are independent")
    print("- lm2: Linear regression, consider also metric combinations")
    print("- pca-linear/loc: PC analysis with linear or logarithmic normaliation")
    print("- glm[-k]: Logistic Regression (Binary classification), optional k-Fold based")
    print("- mergePCs: Select metrics, which are used in pricinpal components")
    print("- mergeOnly: MErge only metrics (from multiple input data) into one common sheet")
    print("- VisDif: Compute violin diagrams for each metric (healthy vs. errornous functions)")
    print("- anova: Compute statistical summaries and ANOVA test for each metric (healthy vs. errornous functions)")
    print("- welch: Single Welch test on each metric (healthy vs. errornous functions)")
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
  readAndAnalyse(vargs[[1]], as.logical(vargs[2]), vargs[3])
}
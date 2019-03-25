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


# Removes the last nRows (onlyLast=FALSE) or returns
# only the last nRows (onlyLast=TRUE).
lastNRows <- function(df, nRows, onlyLast=TRUE) {
  lastRow <- nrow(df)
  start <- lastRow - nRows + 1
  if(onlyLast) {
    cut <- df[start:lastRow,]
  } else {
    cut <- df[-start:-lastRow,]
  }
  return(cut)
}

# Keeps all column until value (or higher) was found in a column in the specified row
keepUntil <- function(df, rowIndex, value) {
  relevantRow <- as.data.frame(df[rowIndex:rowIndex, ])
  #relevantColumns <- apply(relevantRow, 1, function(x) which(x >= value))
  firstColumn <- which(relevantRow[1] > value)[1] #relevantColumns[1]
  
  # Fallback if criteria could not be found
  if (is.na(firstColumn)) {
    firstColumn <- ncol(df)
  }
  
  # A vector misses column names, fallback keep at least two rows
  if(firstColumn == 1) {
    firstColumn = 2
  }
  
  
  cut <- df[, 1:firstColumn]
  
  return(cut)
}

filterPCAResult <- function(pcaResult, nMetaRows, relDiff, importanceRow, impotanceValue) {
  pcaResult <- keepUntil(pcaResult, importanceRow, impotanceValue)
  
  # Extract principle components and determine max of each PC
  pcs <- lastNRows(pcaResult, nMetaRows, FALSE)
  absPCs <- abs(pcs)
  max <- apply(absPCs, 2, function(x) max(x))
  
  # Relative to the max based on: https://stackoverflow.com/a/25551495
  relatives <- t(t(absPCs) / max)
  
  keep_full <- apply(relatives, 2, function(x) x >= relDiff)
  # Based on: https://stackoverflow.com/a/28233806
  keep <- as.data.frame(apply(keep_full, 1, function(r) any(r %in% TRUE)))
  # Keep relevant rows and extract their names: https://stackoverflow.com/a/7534548
  metricNames <- subset(keep, keep[1]==TRUE)
  metricNames <- row.names(metricNames)
  # print(relatives)
  # print(keep)
  return(metricNames)
}

removeColumnsByName <- function(df, columnNames, removeSelected=TRUE) {
  if (removeSelected) {
    # See: https://stackoverflow.com/a/5234201
    tmp <- df[, -which(names(df) %in% columnNames)]
    if (length(tmp) == 0) {
      # Type conversation error, try to fix this
      tmp <- df[, -which(names(df) %in% unlist(columnNames))]
    }
    df <- tmp
  } else {
    # See: https://stackoverflow.com/a/5234201
    tmp <- df[, which(names(df) %in% columnNames)]
    if (length(tmp) == 0) {
      # Type conversation error, try to fix this
      tmp <- df[, which(names(df) %in% unlist(columnNames))]
    }
    df <- tmp
  }
  
  return(df)
}

removeRowsByValue <- function(df, column, valueToRemove) {
  # Based on: https://stackoverflow.com/a/8006918
  newDF <- df[(df[column] != valueToRemove), ]
  
  return(newDF)
}

normalizeColumnNames <- function(df) {
  names <- normalizeNames(colnames(df))
  
  colnames(df) <- names
  return(df)
}

normalizeNames <- function(names) {
  names <- gsub("\\.\\.\\.", " & ", names)
  names <- gsub("\\.\\.", "@@", names)
  names <- gsub("\\.", " ", names)
  names <- gsub("@@", "\\. ", names)
  # Clean-up of type names
  names <- gsub(" bool 1 ", "(bool=1)", names)
  names <- gsub(" tristate 1 ", "(tristate=1)", names)
  names <- gsub(" string 1 ", "(string=1)", names)
  names <- gsub(" integer 1  int 1 ", "(integer=1, int=1)", names)
  names <- gsub(" hex 1 ", "(hex=1)", names)
  # Clean-up of basis vectors
  names <- gsub(" 1 0 0 ", "(1-0-0)", names)
  names <- gsub(" 0 1 0 ", "(0-1-0)", names)
  names <- gsub(" 0 0 1 ", "(0-0-1)", names)
  
  return(names)
}

applyPCs <- function(df, usefullMetrics, isfunctionBased=TRUE) {
  df <- normalizeColumnNames(df)
  if(isfunctionBased) {
    baseNames = c("File", "Function", "Line", "nErrors")
  } else {
    baseNames = c("File", "nErrors")
  }
  baseNames <- as.data.frame(baseNames)
  names(baseNames) <- names(usefullMetrics)
  names <- rbind(baseNames, usefullMetrics)
  df <- removeColumnsByName(df, names, FALSE)
  
  return(df)
}

# ANOVA
varianceTest <- function(df, metricsColumn, labelsColumn) {
  columNames <- names(df)
  goods <- df[metricsColumn][df[labelsColumn]==0, ]
  bads  <- df[metricsColumn][df[labelsColumn]==1, ]
  
  # Statistical Summary
  Unproblematic <- summary(goods)
  Errornous     <- summary(bads)
  model <- rbind(Unproblematic, Errornous)
  
  # ANOVA
  formulaStr = paste(columNames[labelsColumn], "~", columNames[metricsColumn])
  anovaModel = aov(as.formula(formulaStr), data=df)
  anovaModel <- summary(anovaModel)
  anovaModel <- as.data.frame(anovaModel[[1]])
  
  # Add column with name of metric
  metricName <- normalizeNames(columNames)[metricsColumn]
  # Name of this object will be displayed in output
  Metric = c(metricName, metricName)
  nameDF <- data.frame(Metric)
  model <- cbind(nameDF, model, anovaModel)
  
  return(model)
}

cohensD <- function(df, metricsColumn, labelsColumn, pool=FALSE, hedgesD=FALSE) {
  # Based on: https://www.rdocumentation.org/packages/effsize/versions/0.7.4/topics/cohen.d
  library("effsize")
  
  columNames <- names(df)
  goods <- df[metricsColumn][df[labelsColumn]==0, ]
  bads  <- df[metricsColumn][df[labelsColumn]>0, ]

  # Second parameter is treated as control group
  result <- cohen.d(bads, goods, pooled=pool, hedges.correction=hedgesD)
  
  return(result)
}

cohensD.as.df <- function(df, metricsColumn, labelsColumn, pool=FALSE, hedgesD=FALSE, addSummary=FALSE) {
  metricName <- names(df[metricsColumn])
  r <- cohensD(df, metricsColumn, labelsColumn, pool=pool, hedgesD=hedgesD)
  
  classification.as.int <- r$magnitude
  classification.as.str <- levels(classification.as.int)[unclass(classification.as.int)]
  
  result        <- c(metricName, r$estimate, classification.as.str, r$method, r$conf.int)
  names(result) <- c("Metric", "Delta", "Classification", "Test Method", "Conf int. (lower)", "Conf int. (upper)")
  result.as.df  <- as.data.frame(t(result))
  
  if (addSummary) {
    goods <- df[metricsColumn][df[labelsColumn]==0, ]
    bads  <- df[metricsColumn][df[labelsColumn]>0, ]
    
    summary.goods  <- summary(goods)
    summary.bads   <- summary(bads)
    variance.goods <- var(goods)
    variance.bads  <- var(bads)
    median.diff    <- extractSummary(summary.bads, 3) - extractSummary(summary.goods, 3)
    mean.diff      <- extractSummary(summary.bads, 4) - extractSummary(summary.goods, 4)
    summary.both   <- c(extractSummary(summary.goods, 1), extractSummary(summary.goods, 2), extractSummary(summary.goods, 3),
                        extractSummary(summary.goods, 4), extractSummary(summary.goods, 5), extractSummary(summary.goods, 6), variance.goods,
                        extractSummary(summary.bads, 1), extractSummary(summary.bads, 2), extractSummary(summary.bads, 3),
                        extractSummary(summary.bads, 4), extractSummary(summary.bads, 5), extractSummary(summary.bads, 6), variance.bads,
                        mean.diff, median.diff)
    
    names(summary.both) <- c("Min. (Healthy)", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.", "Variance (Healthy)",
                             "Min. (Errorneous)", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.", "Variance (Errorneous)",
                             "Mean Diff (Errorneous - Healthy)", "Median Diff (Errorneous - Healthy)")
    
    result.as.df2 <- as.data.frame(t(summary.both))
    result.as.df  <- cbind(result.as.df, result.as.df2)
  }
  
  return(result.as.df)
}

statisticalSummary <- function(df) {
  return(summary(df))
}

extractSummary <- function(summaryVector, index) {
  return (as.numeric(trimws(sub("^.*:", "", summaryVector[index]))))
}
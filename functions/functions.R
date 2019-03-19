readFromCSV <- function(fileName, folder="data", separator=";") {
  path    <- paste(folder, "/", fileName, ".csv", sep = "")
  locData <- read.csv(path, header = TRUE, sep=separator)
  
  return(locData)
}

convertToNumbers <- function(df, startIndex) {
  nColumns <- ncol(df)
  
  #cf. https://stackoverflow.com/a/2290107
  df[, startIndex:nColumns] <- sapply(df[, startIndex:nColumns], as.numeric)
  
  return(df)
}

convertToNumbers2 <- function(df, isFunctionBased) {
  if (isFunctionBased) {
    df <- convertToNumbers(df, 4)
  } else {
    df <- convertToNumbers(df, 2)
  }
  
  return(df)
}

removeIdentifier <- function(df, isFunctionBased) {
  # cf. https://stackoverflow.com/a/11991742
  
  if (isFunctionBased) {
    df <- df[, -c(1,2,3)]
  } else {
    df <- df[, -1]
  }
  
  return(df)
}

normalizeNErrors <- function(df) {
  # Replace all values which a greater than 0 by 1: https://stackoverflow.com/a/13871691
  df$nErrors[df$nErrors>0] <- 1
  
  return(df)
}

normalizeData <- function(df) {
  df_data <- df[-1]
  
  # see: https://datascienceplus.com/fitting-neural-network-in-r/
  maxs <- apply(df_data, 2, max) # 2 = Max over columns
  mins <- apply(df_data, 2, min) # 2 = Min over columns
  scaled <- as.data.frame(scale(df_data, center = mins, scale = maxs - mins))
  df = data.frame(df[1], scaled)
  
  return(df)
}

normalizeData_loc <- function(df) {
  # See: https://www.r-bloggers.com/computing-and-visualizing-pca-in-r/
  df_data <- df[-1]
  # log1p = lo(1 + x) to handle zeros: https://stat.ethz.ch/R-manual/R-devel/library/base/html/Log.html
  df_data <- log1p(df_data)
  df = data.frame(df[1], df_data)
  
  return(df)
}

createAllCombinationsFormula <- function(names, y="nErrors") {
  allSingle <- paste(paste(names[!names %in% y], collapse = " + "))
  
  combinations = c()
  outer_end <- (length(names)-1)
  for (i in 1:outer_end) {
    if (names[i] != y) {
      start <- (i+1)
      end   <- length(names)
      
      for (j in start:end) {
        if (names[j] != y) {
          combinations <-append(combinations, paste(names[i], names[j], sep = " * "))
        }
      }
    }
  }
  allPairs <- paste(combinations[!combinations %in% "nErrors"], collapse = " + ")
  
  formula = paste(y, " ~ ", allSingle, " + ", allPairs, sep = "")
  print(formula)
  return(as.formula(formula))
}

saveResults <- function(fileName, model, p = NULL) {
  
  if (!is.null(p)) {
    name <- paste("out/Results for - ",  fileName, " - ", p, ".csv", sep = "")
  } else {
    name <- paste("out/Results for - ",  fileName, ".csv", sep = "")
  }
  write.table(model, name, sep=";", dec=".")
}

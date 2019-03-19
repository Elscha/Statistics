source("functions/functions.R")
source("functions/datafunctions.R")

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
  goods <- df[df[labelsColumn]==0, ]
  bads  <- df[df[labelsColumn]==1, ]
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
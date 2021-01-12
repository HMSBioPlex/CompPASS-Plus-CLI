##
#
# Add introductory information here.
#
##

comppassPlus <- function(inputData, formula, cvUnit){
  
  #Get names of cross-validation sets (i.e. plate or IP identifiers)
  cvSetNames = unique(inputData[[cvUnit]])
  numCVSets = length(cvSetNames)
  
  print(paste("Number of cross-validation sets: ", numCVSets), quote = FALSE)
  
  # Train a separate model for each cross-validation subset
  outputData = data.frame()
  
  for(i in 1:numCVSets){
    
    name = cvSetNames[i]
    print(paste("Scoring plate ", name), quote=FALSE)
    
    trainData = data[which(data[[cvUnit]] != name),]
    testData = data[which(data[[cvUnit]] == name),]
    
    scores = comppassPlusScoring(trainData, testData, formula)
    
    outputData = rbind(outputData, scores)
  }
  
  outputData
}

comppassPlusScoring <- function(trainData, testData, formula){
  
  #Make sure the e1071 package is loaded so the Naive Bayes classifier can be used
  loaded = require("e1071")
  if(!loaded) return (FALSE)
  
  n1 = nrow(trainData)
  n2 = nrow(testData)
  print(paste("# Train: ", n1, " # Test: ", n2), quote=FALSE)
  
  # Train classifier
  print("Training classifier.", quote = FALSE)
  nbClass = naiveBayes(formula, trainData, laplace = 0.1)
  
  #print(nbClass)
  
  # Calculate predictions
  print("Making predictions.", quote = FALSE)
  predictions = predict(nbClass, testData, threshold = 1e-7, type="raw")
  
  # Combine results
  rownames(predictions) = NULL
  rownames(testData) = NULL

  scoredData = merge(testData, predictions, by="row.names")
  scoredData
  
}

percentileFactor <- function(values){
  as.factor(round(rank(values)/length(values)*500, 0))
}

addPercentileFactors <- function(data){

  data["ave_apsm_pct"] = percentileFactor(data$ave_apsm)
  data["entropy_pct"] = percentileFactor(data$entropy)
  data["nwdscore_pct"] = percentileFactor(data$nwdscore)
  data["zscore_pct"] = percentileFactor(data$zscore)

  data["plate_zscore_pct"] = percentileFactor(data$plate_zscore)
  data["total_psms_pct"] = percentileFactor(data$total_psms)
  data["ratio_pct"] = percentileFactor(data$ratio)
  data["ratioTotalPSMs_pct"] = percentileFactor(data$ratioTotalPSMs)
  data["UtoTratio_pct"] = percentileFactor(data$UtoTratio)
  data
}
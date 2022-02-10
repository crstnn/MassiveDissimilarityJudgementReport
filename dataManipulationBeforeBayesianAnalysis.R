
source("dataAggregation.R")
source("preliminaryGraphsAndAnalysis.R")
load("cleansedTrialData.Rda") # had to load in data this way rather than calling a file due to namespace conflicts.

uniqueColourDoublePassSumDF <- data.frame(matrix(0, ncol = countOfUniqueRowsTotalSet, nrow = countOfUniqueRowsTotalSet))
uniqueColourDoublePassCountDF <- data.frame(matrix(0, ncol = countOfUniqueRowsTotalSet, nrow = countOfUniqueRowsTotalSet))
colnames(uniqueColourDoublePassSumDF) <- 
  rownames(uniqueColourDoublePassSumDF) <- 
  colnames(uniqueColourDoublePassCountDF) <- 
  rownames(uniqueColourDoublePassCountDF) <- 
  rowsTotalSetHEX$colour
currentRealComparisionCount <- as.data.frame(matrix(0, nrow(colourSetHEX), 1))
previousParticipantID <- NULL
isNewParticipant <- FALSE

# symmetrised sum of all dissimilarity judgements (can be easily adjusted for per individual judgements)
for (i in 1:nrow(cleansedTrialData)){
  currentParticipantOBS <- cleansedTrialData[i,]
  currParticipantID <- currentParticipantOBS[,"participant"]
  if(i == 1){
    previousParticipantID <- currParticipantID
  }
  isNewParticipant <- previousParticipantID != currParticipantID
  if (isNewParticipant){
    
    currentRealComparisionCount <- as.data.frame(matrix(0, nrow(colourSetHEX), 1))
    previousParticipantID <- currParticipantID
  }
  
  currRC <- currentParticipantOBS[,"realcomparison"]
  
  
  currentRealComparisionCount[currRC,] <- currentRealComparisionCount[currRC,] + 1
  
  # counts both passes of each participant
  if(currentRealComparisionCount[currRC,] <= 2){
    HEX1 <- colourSetHEX[currRC,1]
    HEX2 <- colourSetHEX[currRC,2]
    
    
    uniqueColourDoublePassSumDF[HEX2, HEX1] <- 
      uniqueColourDoublePassSumDF[HEX2, HEX1] + currentParticipantOBS[, "dissimilarity"]
    uniqueColourDoublePassSumDF[HEX1, HEX2] <- 
      uniqueColourDoublePassSumDF[HEX1, HEX2] + currentParticipantOBS[, "dissimilarity"]
    uniqueColourDoublePassCountDF[HEX2, HEX1] <- uniqueColourDoublePassCountDF[HEX2, HEX1] + 1
    uniqueColourDoublePassCountDF[HEX1, HEX2] <- uniqueColourDoublePassCountDF[HEX1, HEX2] + 1
    
  }
}

allParticipantsDissimilarityAverage <- (uniqueColourDoublePassSumDF / uniqueColourDoublePassCountDF) / 7 # ratio of the largest possible value a participant can select

rownames(allParticipantsDissimilarityAverage) <- names(allParticipantsDissimilarityAverage) <- NULL


save(allParticipantsDissimilarityAverage, file="allParticipantsDissimilarityAverage.Rda")

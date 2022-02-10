library(tidyverse)
library(ramify)
library(dplyr)
library(ggplot2)
library(reshape2)
library(viridis)
library(MASS)
library(hrbrthemes)
library(scales)
library(broom)
library(pglm)

colourSetHEX <- data.frame(firstColour = firstColourSetHEX, secondColour = secondColourSetHEX)

rowsTotalSetHEX <- rbind(data.frame(colour = firstColourSetHEX), data.frame(colour = secondColourSetHEX)) %>% group_by_all %>% count

# preliminary check that the data has been read in correctly
countOfUniqueRowsFirstSet <- length(unique(firstColourSetHEX))
countOfUniqueRowsSecondSet <- length(unique(secondColourSetHEX))
countOfUniqueRowsTotalSet <- length(rowsTotalSetHEX$colour)


# set lower triangle of matrix to NA - this gives heatmap its unique upper triangle
upperTriangularMatrix <- tril(matrix(1, ncol = countOfUniqueRowsTotalSet, nrow = countOfUniqueRowsTotalSet), diag = FALSE)
upperTriangularMatrix[upperTriangularMatrix == 1] <- NA
uniqueColourCountDF <- data.frame(upperTriangularMatrix)
# setting all give colours as both row and column names
colnames(uniqueColourCountDF) <- rownames(uniqueColourCountDF) <- rowsTotalSetHEX$colour


currentRealComparisionCount <- as.data.frame(matrix(0, nrow(colourSetHEX), 1))
previousParticipantID <- NULL
isNewParticipant <- FALSE


# loop responsible for population of colour comparison matrix
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
  
  # counts only the first pass of each participant
  if(currentRealComparisionCount[currRC,] == 1){
    
    HEX1 <- colourSetHEX[currRC,1]
    
    HEX2 <- colourSetHEX[currRC,2]
    
    if(is.na(uniqueColourCountDF[HEX1, HEX2])){
      uniqueColourCountDF[HEX2, HEX1] <- uniqueColourCountDF[HEX2, HEX1] + 1
    }else{
      uniqueColourCountDF[HEX1, HEX2] <- uniqueColourCountDF[HEX1, HEX2] + 1
    }
  }
}


matrixToLongFormat <- function (uniqueColourCountDF){
  # long format function necessary for ggplot heatmap
  # @uniqueColourCountDF: matrix of colour comparison pairs
  
  longUniqueColourCountDF <- data.frame()
  
  # loop to convert to long format
  for(r in 1:nrow(uniqueColourCountDF)){
    for(c in 1:ncol(uniqueColourCountDF)){
      if (!is.na(uniqueColourCountDF[r, c])){
        longUniqueColourCountDF <- 
          rbind(longUniqueColourCountDF, c(colnames(uniqueColourCountDF)[r], 
                                           colnames(uniqueColourCountDF)[c], 
                                           uniqueColourCountDF[r, c]))
      }
    }
  }
  colnames(longUniqueColourCountDF) <- c("colour1", "colour2", "count")
  longUniqueColourCountDF$count <- as.numeric(longUniqueColourCountDF$count)
  return(longUniqueColourCountDF)
  
}

longUniqueColourCountDF <- matrixToLongFormat(uniqueColourCountDF)

# first pass count heatmap
ggplot(longUniqueColourCountDF, aes(colour1, colour2, fill=count)) + 
  geom_tile() + scale_fill_viridis(discrete=FALSE) +
  theme_ipsum() + 
  theme(legend.key.size = unit(1.2,"line"),
        axis.text.x = element_text(colour=longUniqueColourCountDF$colour2, angle=90),
        axis.text.y = element_text(colour=longUniqueColourCountDF$colour2),
        axis.title.x = element_blank(), axis.title.y = element_blank())


uniqueColourDoublePassCountDF <- data.frame(upperTriangularMatrix)
colnames(uniqueColourDoublePassCountDF) <- rownames(uniqueColourDoublePassCountDF) <- rowsTotalSetHEX$colour
currentRealComparisionCount <- as.data.frame(matrix(0, nrow(colourSetHEX), 1))
previousParticipantID <- NULL
isNewParticipant <- FALSE

# loop responsible for population of colour comparison matrix
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
    
    if(is.na(uniqueColourDoublePassCountDF[HEX1, HEX2])){
      uniqueColourDoublePassCountDF[HEX2, HEX1] <- uniqueColourDoublePassCountDF[HEX2, HEX1] + 1
    }else{
      uniqueColourDoublePassCountDF[HEX1, HEX2] <- uniqueColourDoublePassCountDF[HEX1, HEX2] + 1
    }
  }
}


# setting all give colours as both row and column names

longUniqueDoublePassColourCountDF <- matrixToLongFormat(uniqueColourDoublePassCountDF)

# two pass count heatmap
ggplot(longUniqueDoublePassColourCountDF, aes(colour1, colour2, fill=count)) + 
  geom_tile() + scale_fill_viridis(discrete=FALSE) +
  theme_ipsum() + 
  theme(legend.key.size = unit(1.2,"line"),
        axis.text.x = element_text(colour=longUniqueDoublePassColourCountDF$colour2, angle=90),
        axis.text.y = element_text(colour=longUniqueDoublePassColourCountDF$colour2),
        axis.title.x = element_blank(), axis.title.y = element_blank())


currentRealComparisionCount <- as.data.frame(matrix(0, nrow(colourSetHEX), 1))
sameColourDissimilarity <- as.data.frame(matrix(0, length(rowsTotalSetHEX$colour), 2))
colnames(sameColourDissimilarity) <- c("sum", "count")
rownames(sameColourDissimilarity) <- rowsTotalSetHEX$colour
previousParticipantID <- NULL
isNewParticipant <- FALSE

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
  
  HEX1 <- colourSetHEX[[currRC,1]]
  
  HEX2 <- colourSetHEX[[currRC,2]]
  
  # counts of only the first pass of each participant and the two HEX values are the same
  if(currentRealComparisionCount[currRC,] == 1 & HEX1 == HEX2){
    
    sameColourDissimilarity[HEX1, "sum"] <- sameColourDissimilarity[HEX1, "sum"] + currentParticipantOBS[, "dissimilarity"]
    
    sameColourDissimilarity[HEX1, "count"] <- sameColourDissimilarity[HEX1, "count"] + 1
  }
}


sameColourDissimilarity$average <- sameColourDissimilarity$sum / sameColourDissimilarity$count
sameColourDissimilarity[is.na(sameColourDissimilarity)] = 0 # for division by zero case


ggplot(sameColourDissimilarity, aes(x=average)) + geom_histogram(bins=31)

ggplot(sameColourDissimilarity, aes(x=count)) + geom_histogram(bins=30) + scale_x_continuous(limits=c(0, 31))


aggDataDF <- merge(x = cleansedTrialData, y = truthColourTable, by.x = "realcomparison" , by.y = 0, all.x = TRUE) 
aggDataDF$rDiff <- abs(aggDataDF$r1 - aggDataDF$r2)
aggDataDF$gDiff <- abs(aggDataDF$g1 - aggDataDF$g2)
aggDataDF$bDiff <- abs(aggDataDF$b1 - aggDataDF$b2)
aggDataDF$RGBdiffSum <- aggDataDF$rDiff + aggDataDF$gDiff + aggDataDF$bDiff
aggDataDF$dissimilarity <- as.factor(aggDataDF$dissimilarity)

ggplot(aggDataDF, aes(x=RGBdiffSum, y=dissimilarity)) + 
  geom_violin()

# hist(aggDataDF$RGBdiffSum)

# taking the mean of each participant (if double pass was reached)
aggDataDFStabilised <- cleansedTrialData %>% group_by(participant, realcomparison) %>% summarise(dissimilarity_mean=(mean(dissimilarity)), response_time_mean = mean(response_time))

aggDataDFStabilised$dissimilarity_mean <- as.factor(aggDataDFStabilised$dissimilarity_mean)

# left join (truth table row number on agg data realcomparison)
aggDataDFStabilised <- merge(x = aggDataDFStabilised, y = truthColourTable, by.x = "realcomparison" , by.y = 0, all.x = TRUE) 
aggDataDFStabilised$rDiff <- abs(aggDataDFStabilised$r1 - aggDataDFStabilised$r2)
aggDataDFStabilised$gDiff <- abs(aggDataDFStabilised$g1 - aggDataDFStabilised$g2)
aggDataDFStabilised$bDiff <- abs(aggDataDFStabilised$b1 - aggDataDFStabilised$b2)


getConfInterval <- function(m, level=0.99){
  # returns confidence intervals of all regression coefficients
  # @m: model frame of interest
  m <- summary(m)
  conf <- as.data.frame(matrix(NA, 2, 0), row.names = c("lower", "upper"))
  zcrit <- qnorm(level)
  
  for (c in 1:nrow(m$coefficients)){
    conf["lower",rownames(m$coefficients)[c]] <- m$coefficients[c, 1] - zcrit * m$coefficients[c, 2]
    conf["upper",rownames(m$coefficients)[c]] <- m$coefficients[c, 1] + zcrit * m$coefficients[c, 2]
  }
  return(conf)
}


logitModel <- polr(formula = dissimilarity ~ rDiff + gDiff + bDiff + response_time, data = aggDataDF, method = "logistic")


probitModel <- polr(formula = dissimilarity ~ rDiff + gDiff + bDiff + response_time, data = aggDataDF, method = "probit")

options(scipen=999)
summary(logitModel)
summary(probitModel)

getConfInterval(logitModel)
getConfInterval(probitModel)




logitFEModel <- pglm(dissimilarity ~ rDiff + gDiff + bDiff + response_time, data = aggDataDF,
                     index = c('participant'), family = ordinal('logit'), model = 'random', na.action = na.omit, chunksize = 5000)

summary(logitFEModel)

probitFEModel <- pglm(dissimilarity ~ rDiff + gDiff + bDiff + response_time, data = aggDataDF, index = 'participant', family = ordinal('probit'), model = 'random', method = 'bfgs', start = NULL)


 
# ggplot(lnewdat, aes(x = response_time_mean, y = Probability, colour = Level)) +
#   geom_line() + facet_grid(rDiff ~ gDiff ~ bDiff, labeller="label_both")
# 
#
# image(1:length(longUniqueColourCountDF$colour2), 1, 
#       as.matrix(1:length(longUniqueColourCountDF$colour2)), col=longUniqueColourCountDF$colour2,
#       xlab="", ylab = "", xaxt = "n", yaxt = "n", bty = "n")
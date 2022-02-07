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

# matching regex of any amount of characters then followed by .csv
files <- list.files(path="./data", pattern="*.csv", full.names=TRUE)
filesizes <- file.size(files)

truthColourTableColumnNames <- c("r1", "g1", "b1", "r2", "g2", "b2")
truthColourTable <- read.csv("./colourcodes/colourcodes.csv", header=F)
names(truthColourTable) <- truthColourTableColumnNames

files <- files[-(which(filesizes < 4))]

pilotdata <- sapply(files, read.csv, simplify=FALSE) %>% bind_rows(.id = "fileId")

# changing realcomparison from zero-indexed to one-indexed

participantsIDFrame <- data.frame(unique(pilotdata$participant))

pilotdata <- pilotdata %>% arrange("participant")

# variables of interest from collected data
trial_vars<- c( "participant", "practice_comparison", "pracsimilarity", "realcomparison", "similarity", "response_time", "trials_2.thisN") 
catch_vars<- c("participant", "catch_response_time", "catchnumberprac", "catchpracsimilarity", "catchnumber", "catchsimilarity", "catchRT", "catchtrialorder")
trialdata <- (pilotdata %>% filter(!is.na(realcomparison)))[trial_vars]
catchdata <- (pilotdata %>% filter(!is.na(catchnumber)))[catch_vars]
trialdata = rename(trialdata, dissimilarity = similarity)

trialdata$realcomparison <- trialdata$realcomparison + 1

rgb2hex <- function(r, g, b) {rgb(r, g, b, maxColorValue = 255)}

# compartmentalising the two dots presented to participants
firstColourSet <- truthColourTable[,truthColourTableColumnNames[1:3]]
secondColourSet <- truthColourTable[,truthColourTableColumnNames[4:6]]

names(firstColourSet) <- names(secondColourSet) <- c("r", "g", "b")

#conversion of 3 RGB columns in both colour tables to HEX for easier manipulation
firstColourSetHEX <- apply(firstColourSet, 1, function (x) rgb2hex(x[1], x[2], x[3]))
secondColourSetHEX <- apply(secondColourSet, 1, function (x) rgb2hex(x[1], x[2], x[3]))


ggplot(trialdata, aes(x=response_time)) + geom_histogram() + scale_x_continuous(limits=c(0, 5))
ggplot(trialdata, aes(x=dissimilarity)) + geom_histogram(bins = 8) + scale_x_continuous(limits=c(-1, 8))

# -----------------------CATCH-TRIAL ANALYSIS-----------------------------

# checking whether catchnumber matches catchsimiliarity for any given row
catchdata <- catchdata %>% mutate(
  is_valid_catch_response = if_else(catchnumber == catchsimilarity, 1, 0), 
  is_invalid_catch_response = if_else(catchnumber != catchsimilarity, 1, 0))

# per participant (*based on unique ID) count of correct and incorrect responses
catchdataParticipant <- catchdata %>% 
  group_by(participant) %>% 
  summarise(
    correct_responses = sum(is_valid_catch_response), 
    incorrect_responses = sum(is_invalid_catch_response))

# total count of participants' catch trials completed on their given experiment
catchdataParticipant$total <- catchdataParticipant$correct_responses + catchdataParticipant$incorrect_responses

# calculation of accuracy metric
catchdataParticipant$accuracy <- (catchdataParticipant$correct_responses/catchdataParticipant$total) * 100

# Visually inspect to find where one would prefer the cutoff point to be
# rule of thumb: 70% accuracy but choices should be made on a case-by-case basis
hist(catchdataParticipant$accuracy)


# CHANGE SELECTED CUT-OFF HERE. Results in a DF of 'valid' participants
selectedCutOff <- 70
catchdataParticipant <- catchdataParticipant %>% filter(accuracy > selectedCutOff)

# --------------------CHECK FOR COMPLETION OF EXPERIMENT---------------------

trialdataParticipant <- trialdata %>% 
  group_by(participant, realcomparison) %>% 
  count(realcomparison, name = 'realcomparison_frequency')

trialdataParticipant <- 
  trialdataParticipant %>% filter(realcomparison_frequency == 2) # 2 due to double pass invariant


# set.seed(101)
# my.mle<-fitdistr(filter(trialdata, participant == participantsIDFrame[400,])$response_time, densfun="gamma")
# dgamma(0.9, coef(my.mle)[[1]], coef(my.mle)[[2]])

# source("preliminaryGraphsAndAnalysis.R")

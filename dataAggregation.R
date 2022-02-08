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
catchdataValidParticipants <- catchdata %>% mutate(
  is_valid_catch_response = if_else(catchnumber == catchsimilarity, 1, 0), 
  is_invalid_catch_response = if_else(catchnumber != catchsimilarity, 1, 0))

# per participant (*based on unique ID) count of correct and incorrect responses
catchdataValidParticipants <- catchdataValidParticipants %>% 
  group_by(participant) %>% 
  summarise(
    correct_responses = sum(is_valid_catch_response), 
    incorrect_responses = sum(is_invalid_catch_response))

# total count of participants' catch trials completed on their given experiment
catchdataValidParticipants$total <- catchdataValidParticipants$correct_responses + catchdataValidParticipants$incorrect_responses

# calculation of accuracy metric
catchdataValidParticipants$accuracy <- (catchdataValidParticipants$correct_responses/catchdataValidParticipants$total) * 100

# Visually inspect to find where one would prefer the cutoff point to be
# rule of thumb: 70% accuracy but choices should be made on a case-by-case basis
hist(catchdataValidParticipants$accuracy)


# CHANGE SELECTED CUT-OFF HERE. Results in a DF of 'valid' participants (based on correct catch response)
selectedCutOff <- 70
catchdataValidParticipants <- catchdataValidParticipants %>% filter(accuracy > selectedCutOff)

# --------------------CHECK FOR COMPLETION OF EXPERIMENT---------------------
# Analysis assumes the experiment methodology invariant that halfway through
# the experiment a participant won't be introduced to any new colour comparisons
# i.e. the experiment is setup such that a participant can't leave the 
# experiment early and have seen colour comparisons twice with new colour 
# comparisons still yet to be displayed to the participant in any given experiment

trialdataValidParticipants <- trialdata %>% 
  group_by(participant, realcomparison) %>% 
  count(realcomparison, name = 'realcomparison_frequency')

# Results in a DF of 'valid' participants (based on completing the experiment)
trialdataValidParticipants <- trialdataValidParticipants %>% 
  group_by(participant) %>%
  filter(realcomparison_frequency == 2) %>% # 2 due to double pass invariant
  distinct(participant)

# HOW TO UTILISE THE DFs PREFIXED WITH 'ValidParticipants'
# Find the *intersection* of the DFs on based on the 'participant' column to get 
# a list of valid participants from both analysis steps. Then using your *master*
# DF containing all the experiment data (here I use trialdata) you filter 
# based on participants that are in the aformentioned *intersected* list.
# Like so:

cleansedTrialData <- trialdata %>% 
    filter(participant %in% 
           intersect(catchdataValidParticipants$participant, 
                      trialdataValidParticipants$participant))


# -------------------------------------------------------------------------

# set.seed(101)
# my.mle<-fitdistr(filter(trialdata, participant == participantsIDFrame[400,])$response_time, densfun="gamma")
# dgamma(0.9, coef(my.mle)[[1]], coef(my.mle)[[2]])

# source("preliminaryGraphsAndAnalysis.R")

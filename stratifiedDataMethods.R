source("dataAggregation.R")

# -----------------STRATIFIED DATA SPLITTING-------------------------
# Stratified sampling based on equal split

cleansedTrialData <- cleansedTrialData %>%
  group_by(realcomparison) %>%
  mutate(isTrainingSet = (row_number() <= 1/2 * n())) %>% # for a half-half split
  ungroup()

firstHalf <- filter(cleansedTrialData , isTrainingSet)
secondHalf <- filter(cleansedTrialData , !isTrainingSet)



# -----------------STRATIFIED (RANDOM) SAMPLING----------------------
# Stratified sampling based on equal split

splitObj <- rsample::initial_split(cleansedTrialData, 
                                   prop = 1/2,  # for a half-half split
                                   strata = realcomparison)

firstHalf <- rsample::training(splitObj)

secondHalf <- rsample::test(splitObj)
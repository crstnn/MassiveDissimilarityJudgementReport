
library(R.matlab)
library(coda)
library(bridgesampling)

source("order_stimuli.R")

data_set <- "cohen_lines" # "color_normal"

#--------------------------------------------------------------------------
# load and prepare data
#--------------------------------------------------------------------------

if (data_set == "color_normal") {
  
  r <- readMat("data/helm_ind.mat")
  tmpy <- r$dind[,,1:10]
  tmpy <- tmpy/max(tmpy)
  nSubjects <- 10
  nStimuli <- as.numeric(r$n)
  stimulusNames <- c('rp', 'ro', 'y', 'gy1', 'gy2',
                     'g', 'b', 'pb', 'p2', 'p1')
  stimulus_names_long <- c("red-purple", "red-orange", "yellow",
                           "green-yellow-1", "green-yellow-2",
                           "green", "blue", "purple-blue",
                           "purple-2", "purple-1")
  
} else if (data_set == "cohen_lines") {
  
  r <- readMat("data/CohenLines.mat")
  tmpy <- r$dind
  tmpy <- tmpy/max(tmpy)
  tmpy[,,5] <- 1 - tmpy[,,5] # Subject used scale wrong way
  nSubjects <- dim(tmpy)[3]
  nStimuli <- as.numeric(r$n)
  stimulusNames <- as.character(1:9)
  stimulus_names_long <- stimulusNames
  
}

# constants
nPairs <- nStimuli * (nStimuli - 1) / 2
# Unpack similarity matrices into vectors
pair <- matrix(nrow = nPairs, ncol = 2)
y <- matrix(nrow = nSubjects, ncol = nPairs)
count <- 0
for( i in 1:(nStimuli - 1)) {
  for (j in (i+1):nStimuli) {
    count <- count + 1
    pair[count, 1] <- i
    pair[count, 2] <- j
    y[, count] <- tmpy[i, j,]
  }
}

new_order <- order_stimuli(y = y, pair = pair)
if (data_set == "cohen_lines") {
  index <- c(1, which(new_order == 2))
  new_order[index] <- new_order[rev(index)]
}
pair_new <- recode_pair(new_order = new_order, pair = pair)
stimulus_names_new <- stimulusNames[new_order]
stimulus_names_long_new <- stimulus_names_long[new_order]

#-------------------------------------------------------------------------------
# save stimulus ordering and labels
#-------------------------------------------------------------------------------

dnames <- data.frame(stimulusNumber = seq_along(stimulus_names_long_new),
                     stimulusName = stimulus_names_long_new)
write.csv(dnames, file = paste0("csv/", data_set, "/legend.csv"),
          quote = FALSE, row.names = FALSE)

#-------------------------------------------------------------------------------
# save posterior samples
#-------------------------------------------------------------------------------

if (data_set == "lee_navarro") {
  maxDimensions <- 5
} else if (data_set == "cohen_lines") {
  maxDimensions <- 3
} else {
  maxDimensions <- 4
}

for (nDimensions in seq_len(maxDimensions)) {
  load(paste0("demcmc_fits/", data_set, "/demcmcfit_dim",
              nDimensions, ".Rdata"))
  samplestmp <- as.matrix(ml_samples)
  cnt <- colnames(samplestmp)
  index <- grepl(pattern = "x", cnt) | cnt == "sdS" |
    cnt == "unnormalizedLogPosterior"
  samples <- samplestmp[,index]
  cn <- colnames(samples)
  xindex <- grepl("x", cn)
  cn[xindex] <- gsub(pattern = ",", replacement = "_", x = cn[xindex])
  colnames(samples) <- cn
  write.csv(samples, file = paste0("csv/", data_set, "/samples_",
                                   nDimensions, "d.csv"),
            quote = FALSE, row.names = FALSE)
}


#-------------------------------------------------------------------------------
# save posterior probabilities
#-------------------------------------------------------------------------------

load(paste0("demcmc_bridge/", data_set, "/bridge.Rdata"))

# posterior model probabilities
if (data_set == "lee_navarro") {
  post_probs <- post_prob(bridge[[1]], bridge[[2]],
                          bridge[[3]], bridge[[4]],
                          bridge[[5]],
                          model_names = paste(seq_len(5),
                                              "d", sep = "-"))
} else if (data_set == "cohen_lines") {
  post_probs <- post_prob(bridge[[1]], bridge[[2]], bridge[[3]],
                          model_names = paste(seq_len(3),
                                              "d", sep = "-"))
} else {
  post_probs <- post_prob(bridge[[1]], bridge[[2]],
                          bridge[[3]], bridge[[4]],
                          model_names = paste(seq_len(4),
                                              "d", sep = "-"))
}

write.csv(post_probs, file = paste0("csv/", data_set,
                                    "/post_prob.csv"),
          quote = FALSE, row.names = FALSE)

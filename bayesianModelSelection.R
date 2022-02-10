# code thanks to Gronau and Lee 2020
library(R.matlab)
library(truncnorm)
library(Rcpp)
library(coda)
library(bridgesampling)
library(mcmcplots)

source("order_stimuli.R")
sourceCpp("demcmc_all_dim.cpp")

load("allParticipantsDissimilarityAverage.Rda") # had to load in data this way rather than calling a file due to namespace conflicts.

data_set <-  "color_normal" 

#--------------------------------------------------------------------------
# helper functions
#--------------------------------------------------------------------------

init_fun <- function(n_chains, data, par_names) {
  
  xpos_ind <- data$xpos_ind
  xfree_ind <- data$xfree_ind
  sdS_ind <- data$sdS_ind
  
  theta_start <- matrix(0, nrow = n_chains,
                        ncol = data$nDimensions * data$nStimuli + 1)
  theta_start[,xpos_ind] <- runif(n_chains * length(xpos_ind))
  theta_start[,xfree_ind] <- runif(n_chains * length(xfree_ind), -1, 1)
  theta_start[,sdS_ind] <- rtruncnorm(n_chains, a = 0,
                                      mean = 0.15, sd = 0.2)
  colnames(theta_start) <- par_names
  return(theta_start)
  
}

demcmc_as_mcmc_list <- function(out, xzero_ind, for_bridge = FALSE) {
  
  theta <- out$theta
  target <- out$target
  n_chains <- nrow(target)
  
  l <- vector("list", n_chains)
  for (i in seq_len(n_chains)) {
    if ( ! for_bridge) {
      l[[i]] <- mcmc(cbind(t(theta[i,,]),
                           unnormalizedLogPosterior = target[i,]))
    } else {
      l[[i]] <- mcmc(cbind(t(theta[i,-xzero_ind,])))
    }
  }
  
  return(mcmc.list(l))
  
}

prepare4bridge <- function(mcmclist, par_names, data) {
  
  cn <- colnames(mcmclist[[1]])
  lb <- rep(-Inf, length(cn))
  ub <- rep(Inf, length(cn))
  names(lb) <- names(ub) <- cn
  lb[["sdS"]] <- 0
  lb[which(cn %in% par_names[data$xpos_ind])] <- 0
  ub[which(cn %in% par_names[data$xpos_ind])] <- 1
  lb[cn %in% par_names[data$xfree_ind]] <- -1
  ub[cn %in% par_names[data$xfree_ind]] <- 1
  
  data_bridge <- data
  data_bridge$xpos_ind <- which(cn %in% par_names[data$xpos_ind]) - 1
  data_bridge$xfree_ind <- which(cn %in% par_names[data$xfree_ind]) - 1
  data_bridge$sdS_ind <- which(cn == par_names[data$sdS_ind]) - 1
  
  out <- list(data = data_bridge, lb = lb, ub = ub)
  return(out)
  
}

#--------------------------------------------------------------------------
# load and prepare data
#--------------------------------------------------------------------------


# r <- readMat("data/helm_ind.mat")
# tmpy <- r$dind[,,1:10]
# tmpy <- tmpy/max(tmpy)
# nSubjects <- 10
# stimulusNames <- c('rp', 'ro', 'y', 'gy1', 'gy2',
#                    'g', 'b', 'pb', 'p2', 'p1')
# nStimuli <- as.numeric(r$n)

allParticipantsDissimilarityAverage <- allParticipantsDissimilarityAverage[1:20, 1:20]

nStimuli <- nrow(allParticipantsDissimilarityAverage)
tmpy <- array(numeric(),c(nStimuli,nStimuli,0)) # 93 colour pairs

tmpy <- as.array(abind::abind(tmpy, allParticipantsDissimilarityAverage, along=3))
nSubjects <- 1


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


pair_new <- recode_pair(new_order = new_order, pair = pair)

#--------------------------------------------------------------------------
# fit model
#--------------------------------------------------------------------------

nDimensions <- 5

par_names <- c(paste0("x[", rep(seq_len(nStimuli),
                                each = nDimensions),
                      ",", seq_len(nDimensions), "]"), "sdS")

# number of different elements in x matrix
n_zero <- nDimensions * (nDimensions + 1) / 2
n_nonzero <- nStimuli * nDimensions - n_zero
n_pos <- nDimensions
n_free <- n_nonzero - n_pos

# zero index
xzero_ind <- numeric(n_zero)
i_zero <- 1
for (i in seq_len(nDimensions)) {
  for (j in i:nDimensions) {
    xzero_ind[i_zero] <- j + (i-1)*nDimensions
    i_zero <- i_zero + 1
  }
}
par_names[xzero_ind]

# positive index
xpos_ind <- seq_len(nDimensions) * (nDimensions + 1)
par_names[xpos_ind]

# free index
xfree_ind <- seq_len(nStimuli * nDimensions)[-c(xzero_ind, xpos_ind)]
par_names[xfree_ind]

# sdS index
sdS_ind <- nStimuli * nDimensions + 1

data <- list(y = y,
             nDimensions = nDimensions,
             nStimuli = nStimuli,
             nSubjects = nSubjects,
             nPairs = nPairs,
             pair = pair_new,
             xpos_ind = xpos_ind,
             xfree_ind = xfree_ind,
             sdS_ind = sdS_ind)

n_chains <- 15
n_iter_m <- 500
n_thin <- 3
n_iter <- 3000 * n_thin
init <- init_fun(n_chains = n_chains,
                 data = data,
                 par_names = par_names)

# specify blocks
blocks <- as.list(seq_along(par_names)[-xzero_ind])


set.seed(123)


# sample with migration first to pull in chains
out1 <- demcmc(data = data,
               n_iter = n_iter_m,
               n_burnin = 0,
               n_chains = n_chains,
               n_thin = 1,
               n_pars = length(par_names),
               par_names = par_names,
               init = init,
               blocks = blocks,
               constants = xzero_ind,
               p_migrate = 0.05,
               report = 50)

# sampling
out <- demcmc(data = data,
              n_iter = n_iter,
              n_burnin = 0,
              n_thin = n_thin,
              p_migrate = 0,
              blocks = blocks,
              constants = xzero_ind,
              theta = out1$theta,
              report = 50)

# create mcmc lists (for plots and for bridge sampling)
ml_samples <- demcmc_as_mcmc_list(out = out,
                                  xzero_ind = xzero_ind,
                                  for_bridge = FALSE)
ml_bridge <- demcmc_as_mcmc_list(out = out,
                                 xzero_ind = xzero_ind,
                                 for_bridge = TRUE)

mcmcplot(ml_samples)

save(out, ml_samples, ml_bridge, par_names, data,
     file = paste0("demcmc_fits/", data_set, "/demcmcfit_dim",
                   nDimensions, ".Rdata"))


#--------------------------------------------------------------------------
# bridge sampling
#--------------------------------------------------------------------------

set.seed(1)


maxDimensions <- 5


bridge <- vector("list", maxDimensions)

for (i in seq_len(maxDimensions)) {
  
  
  nDimensions <- i
  load(paste0("demcmc_fits/", data_set, "/demcmcfit_dim",
              nDimensions, ".Rdata"))
  tmp <- prepare4bridge(mcmclist = ml_bridge,
                        par_names = par_names,
                        data = data)
  bridge[[i]] <- bridge_sampler(samples = ml_bridge,
                                log_posterior = log_posterior,
                                data = tmp$data,
                                lb = tmp$lb,
                                ub = tmp$ub,
                                cores = 1,
                                method = "warp3",
                                repetitions = 5)
  save(bridge, file = paste0("demcmc_bridge/", data_set, "/bridge.Rdata"))
  
}

#--------------------------------------------------------------------------
# posterior probabilities
#--------------------------------------------------------------------------

load(paste0("demcmc_bridge/", data_set, "/bridge.Rdata"))


post_probs <- post_prob(bridge[[1]], bridge[[2]],
                        bridge[[3]], bridge[[4]],
                        bridge[[5]],
                        model_names = paste(seq_len(5),
                                            "d", sep = "-"))

print(post_probs)


library(car)

order_stimuli <- function(y, pair) {
  
  nStimuli <- max(unique(as.vector(pair)))
  stim <- numeric(nStimuli)
  data <- data.frame(y = colMeans(y), pair = pair)
  data2 <- data[order(data$y, decreasing = TRUE),]
  stim[1] <- data2[1, "pair.1"]
  stim[2] <- data2[1, "pair.2"]
  data_tmp <- data2[-1,]
  
  for (i in 3:nStimuli) {
    
    remaining_stim <- (1:nStimuli)[-stim[1:(i-1)]]
    
    ind <- vector("list", i-1)
    subdata <- vector("list", i-1)
    dists_2_previous <- matrix(nrow = nStimuli - (i-1), ncol = i-1)
    row.names(dists_2_previous) <- remaining_stim
    colnames(dists_2_previous) <- stim[1:(i-1)]
    
    for (j in 1:(i-1)) {
      
      ind[[j]] <- which(data_tmp$pair.1 == stim[j] | data_tmp$pair.2 == stim[j])
      subdata[[j]] <- data_tmp[ind[[j]],]
      
      for (k in remaining_stim) {
        
        indstim <- which(subdata[[j]]$pair.1  == k | subdata[[j]]$pair.2 == k)
        dists_2_previous[as.character(k),j] <-  subdata[[j]][indstim,"y"]
        
      }
      
    }
    
    mins <- apply(dists_2_previous, 1, min)
    stim[i] <- as.numeric(names(which.max(mins)))
    
    for (l in 1:(i-1)) {
      rmind <- which( (data_tmp$pair.1 == stim[i] & data_tmp$pair.2 == stim[l]) |
                        (data_tmp$pair.1 == stim[l] & data_tmp$pair.2 == stim[i]))
      data_tmp <- data_tmp[-rmind,]
    }
    
  }
  
  return(stim)
  
}

recode_pair <- function(new_order, pair) {
  
  new_pair <- pair
  recodes <- paste(new_order, "=", seq_along(new_order), collapse = " ; ")
  new_pair[,1] <- recode(var = pair[,1], recodes = recodes)
  new_pair[,2] <- recode(var = pair[,2], recodes = recodes)
  return(new_pair)
  
}

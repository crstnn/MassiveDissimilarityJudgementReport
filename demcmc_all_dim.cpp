#include <RcppArmadillo.h>
//[[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;

// [[Rcpp::export]]
double log_posterior(NumericVector theta, List data) {
  
  // extract data
  int nDimensions = data["nDimensions"];
  int nStimuli = data["nStimuli"];
  int nSubjects = data["nSubjects"];
  int nPairs = data["nPairs"];
  NumericMatrix pair = data["pair"];
  NumericVector y = data["y"];
  IntegerVector xpos_ind = data["xpos_ind"];
  IntegerVector xfree_ind = data["xfree_ind"];
  int sdS_ind = data["sdS_ind"];
  
  // extract parameters and evaluate priors
  int n_zero = nDimensions * (nDimensions + 1) / 2;
  int n_nonzero = nStimuli * nDimensions - n_zero;
  int n_pos = nDimensions;
  int n_free = n_nonzero - n_pos;
  
  NumericVector xpos(n_pos);
  int x_pos_out = 0; 
  
  double target = 0.0;
  
  for (int i = 0; i < n_pos; i++) {
    xpos[i] = theta[xpos_ind[i]];
    if (xpos[i] < 0.0 || xpos[i] > 1.0) {
      x_pos_out += 1;
    }
    target += R::dunif(xpos[i], 0.0, 1.0, 1);
  }
  
  NumericVector xfree(n_free);
  int x_free_out = 0; 
  
  for (int i = 0; i < n_free; i++) {
    xfree[i] = theta[xfree_ind[i]];
    if (xfree[i] < -1.0 || xfree[i] > 1.0) {
      x_free_out += 1;
    }
    target +=  R::dunif(xfree[i], -1.0, 1.0, 1);
  }
  
  double sdS = theta[sdS_ind];
  target += R::dnorm(sdS, 0.15, 0.2, 1) -
    R::pnorm(0.0, 0.15, 0.2, 0, 1);
  
  // if parameters are not ok return value so that
  // parameters will not be accepted 
  if (x_pos_out > 0 || x_free_out > 0 || sdS <= 0.0) {
    return -1e10;
  }
  
  // build matrix with latent stimulus locations
  NumericMatrix x(nStimuli, nDimensions);
  int i_pos = 0;
  int i_free = 0;
  
  for (int i = 0; i < nStimuli; i++) {
    for (int j = 0; j < nDimensions; j++) {
      if (i <= j) {
        x(i,j) = 0;
      } else if (i == j + 1) {
        x(i,j) = xpos[i_pos];
        i_pos += 1;
      } else {
        x(i,j) = xfree[i_free];
        i_free += 1;
      }
    }
  }
  
  // compute distances
  NumericMatrix d(nStimuli, nStimuli);
  for (int i = 0; i < nStimuli; i++) {
    for (int j = 0; j < nStimuli; j++) {
      double tmp = 0.0;
      for (int k = 0; k < nDimensions; k++) {
        tmp += pow(x(i,k) - x(j,k), 2);
      }
      d(i, j) = sqrt(tmp);
    }
  }
  
  // evaluate likelihood
  for (int i = 0; i < nSubjects; i++) {
    for (int j = 0; j < nPairs; j++) {
      target += R::dnorm(y(i, j),
                         d(pair(j, 0) - 1, pair(j, 1) - 1),
                         sdS, 1) -
                R::pnorm(0.0, d(pair(j, 0) - 1, pair(j, 1) - 1), sdS, 0, 1);
    }
  }
  
  return target;
  
}

// [[Rcpp::export]]
List crossover(int k,
               IntegerVector par,
               NumericMatrix theta_curr,
               NumericVector target_curr,
               List data,
               double b) {
  
  double gamma = R::runif(0.5, 1);

  // select two chains from remaining chains
  IntegerVector idx = seq_len(theta_curr.nrow()) - 1;
  IntegerVector remaining_chains = idx[idx != k];
  NumericVector prob(remaining_chains.length());
  for (int i = 0; i < prob.size(); i++) {
    prob[i] = 1.0 / as<double>(wrap(prob.size()));
  }
  IntegerVector chains = sample(remaining_chains, 2, false, prob);
  
  // generate proposal
  NumericVector theta_prop = theta_curr(k,_);
  
  for (int i = 0; i < par.length(); i++) {
    theta_prop[par[i]] = theta_curr(k,par[i]) +
      gamma * (theta_curr(chains[0],par[i]) - theta_curr(chains[1],par[i])) +
      R::runif(-b, b); 
  }
  
  // evaluate unnormalized log posterior
  double target_prop = log_posterior(theta_prop, data);
  
  // compute acceptance probability and replace pars and target if accepted
  if (R::runif(0, 1) < exp(target_prop - target_curr[k])) {
    for (int i = 0; i < par.length(); i++) {
      theta_curr(k,par[i]) = theta_prop[par[i]];
    }
    target_curr[k] = target_prop;
  }
  
  // prepare output
  List out;
  out["theta_curr_k"] = theta_curr(k,_);
  out["target_curr_k"] = target_curr[k];
  
  return out;
}

// [[Rcpp::export]]
List migrate(NumericMatrix theta_curr,
             NumericVector target_curr,
             List data,
             IntegerVector constants,
             double b) {
  
  int n_chains = theta_curr.nrow();
  IntegerVector chains = seq_len(n_chains) - 1;
  NumericVector prob(n_chains);
  for (int i = 0; i < n_chains; i++) {
    prob[i] = 1.0 / as<double>(wrap(n_chains));
  }
  
  // sample number of chains to be involved
  // eta is the actual number of chains involved
  // eta_ind is for indexing (hence eta_ind = eta - 1)
  int eta_ind = as<int>(sample(chains, 1, false, prob));
  int eta = eta_ind + 1;
  IntegerVector G = sample(chains, eta, false, prob);
  
  NumericMatrix theta_prop(eta, theta_curr.ncol());
  NumericVector target_prop(eta);
  
  for (int i = 0; i < G.length(); i++) {
    for (int j = 0; j < theta_curr.ncol(); j++) {
      int is_constant = 0;
      for (int k = 0; k < constants.length(); k++) {
        is_constant += j == constants[k];
      }
      if (is_constant) {
        theta_prop(i,j) = theta_curr(G[i],j);
      } else {
        theta_prop(i,j) = theta_curr(G[i],j) + R::runif(-b, b);
      }
    }
    target_prop[i] = log_posterior(theta_prop(i,_), data);
  }
  
  if (R::runif(0, 1) < exp(target_prop[eta_ind] - target_curr[G[0]])) {
    theta_curr(G[0],_) = theta_prop(eta_ind,_);
    target_curr[G[0]] = target_prop[eta_ind];
  }
  
  if (eta > 1) {
    // if more then one chain involved, swap in cyclic fashion
    for (int i = 0; i < eta_ind; i++) {
      if (R::runif(0, 1) < exp(target_prop[i] - target_curr[G[i + 1]])) {
        theta_curr(G[i + 1],_) = theta_prop(i,_);
        target_curr[G[i + 1]] = target_prop[i];
      }
    }
  }
  
  // prepare output
  List out;
  out["theta_curr"] = theta_curr;
  out["target_curr"] = target_curr;
  
  return out;
  
}

// [[Rcpp::export]]
List demcmc_cpp(List data,
                int n_iter,
                int n_chains,
                int n_pars,
                IntegerVector constants,
                List blocks,
                double p_migrate,
                double b,
                arma::cube theta,
                int report) {
  
  int n_blocks = blocks.size();
  
  NumericMatrix target(n_chains, n_iter);
  NumericMatrix theta_start = as<NumericMatrix>(wrap(theta.slice(0)));
  for (int k = 0; k < n_chains; k++) {
    target(k,0) = log_posterior(theta_start(k,_), data);
  }
  
  List tmp;

  for (int i = 1; i < n_iter; i++) {
    
    if (report > 0 && (i + 1) % report == 0) {
      Rf_PrintValue(wrap((i + 1)));
    }

    if (R::runif(0, 1) < p_migrate) {

      tmp = migrate(as<NumericMatrix>(wrap(theta.slice(i-1))),
                    target(_,i-1),
                    data,
                    constants,
                    b);
      
      NumericMatrix theta_curr = tmp["theta_curr"];
      NumericVector target_curr = tmp["target_curr"];
      
      for (int k = 0; k < n_chains; k++) {
        for (int j = 0; j < n_pars; j++) {
          theta(k,j,i) =  theta_curr(k,j);
        }
      }
      target(_,i) = target_curr;

    } else {

      NumericMatrix theta_curr = as<NumericMatrix>(wrap(theta.slice(i-1)));
      NumericVector target_curr = target(_,i-1);
      
      for (int k = 0; k < n_chains; k++) {

        for (int block = 0; block < n_blocks; block++) {

          tmp = crossover(k,
                          as<IntegerVector>(blocks[block]),
                          theta_curr,
                          target_curr,
                          data,
                          b);
          NumericVector theta_curr_k = tmp["theta_curr_k"];
          double target_curr_k = tmp["target_curr_k"];
          theta_curr(k,_) = theta_curr_k;
          target_curr[k] = target_curr_k;

        }

        for (int j = 0; j < n_pars; j++) {
          theta(k,j,i) = theta_curr(k,j);
        }
        target(k,i) = target_curr[k];
      }
    }
  }
  
  List out;
  out["theta"] = theta;
  out["target"] = target;
  
  return out;
}


/*** R

demcmc <- function(data,
                   n_iter = 1e4,
                   n_burnin = 0,
                   n_chains = 20,
                   n_thin = 1,
                   n_pars,
                   par_names = NULL,
                   init = NULL,
                   constants = numeric(0),
                   blocks = list(1, 2),
                   p_migrate = 0,
                   b = 0.01,
                   theta = NULL,
                   report = 10) {
  
  if (is.null(theta)) {
    
    # create array for storing samples
    theta <- array(dim = c(n_chains, n_pars, n_iter))
    
    # insert starting values
    # (will not work for constants and bounded pars!)
    if (is.null(init)) {
      theta[,,1] <- rnorm(n_chains * n_pars)
    } else {
      theta[,,1] <- init
    }
    
  } else {
    
    theta.old <- theta
    n_chains <- dim(theta.old)[1]
    n_pars <- dim(theta.old)[2]
    theta <- array(dim = c(n_chains, n_pars, n_iter))
    theta[,,1] <- theta.old[,,dim(theta.old)[3]]
    par_names <- dimnames(theta.old)[[2]]
    
  }
  
  # since cpp indexing starts with 0 not 1 substract 1
  blocks <- lapply(blocks, function(x) x - 1)
  constants <- constants - 1
  data$xpos_ind <- data$xpos_ind - 1
  data$xfree_ind <- data$xfree_ind - 1
  data$sdS_ind <- data$sdS_ind - 1
  
  s <- demcmc_cpp(data,
                  n_iter,
                  n_chains,
                  n_pars,
                  constants,
                  blocks,
                  p_migrate,
                  b,
                  theta,
                  report)
  
  if (n_burnin > 0) {
    s$theta <- s$theta[,,-seq_len(n_burnin)]
    s$target <- s$target[,-seq_len(n_burnin)]
  }
  
  if (n_thin > 1) {
    s$theta <- s$theta[,,seq_len(dim(s$theta)[3]) %% n_thin == 0]
    s$target <- s$target[,-seq_len(dim(s$theta)[3]) %% n_thin == 0]
  }
  
  dimnames(s$theta)[[2]] <- par_names
  
  out <- list(theta = s$theta, target = s$target)
  return(out)
  
}

*/


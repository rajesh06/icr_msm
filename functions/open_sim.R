open_sim <- function(open_ClNr, last_mat){ #function returns a single data frame with the type of closure and settlement lag
  
  
  #need this to get starting point for markov chain list
  maturity <- 
    open_claims$maturity[which(open_claims$ClNr == open_ClNr)]
  
  mtm_code <- 
    open_claims$mtm_code[which(open_claims$ClNr == open_ClNr)]
  
  my_mc_list <- new("markovchainList", markovchains = 
      mtm_selections[(maturity):length(mtm_selections)], #Note that maturity here is an index (not an age)
    name = paste(maturity, "to ult", sep = "_")) 
    
  #simulate markov chain results
  sim_data <- markovchain::rmarkovchain(n = my_iters, object = my_mc_list, 
    t0 = mtm_code, include.t0 = F)
  
  #organize the simulated data by iteration (rows) and transition (columns)
  outcomes <- matrix(sim_data$values, ncol = my_iters, 
    nrow = length((maturity):length(mtm_selections)), 
    dimnames = list((maturity):length(mtm_selections), 
      1:my_iters)) %>%
    t()
  
  #Get the outcomes we actually care about
  #transition to first closed state
  #whether cz or cnz
  outcomes2 <- dplyr::mutate(as.data.frame(outcomes),
    z_nz = ifelse(stringr::str_detect(last_mat, pattern = 'cz'), 'z', 
      'nz'), #look at the last simulated outcome to get the state.
    settle_lag = apply(X = outcomes, MARGIN = 1, 
      FUN = function(x) min(grep(pattern = "[c]", x)))) #Settle lag is in twelve-month intervals
  
  return(outcomes2)
  
}

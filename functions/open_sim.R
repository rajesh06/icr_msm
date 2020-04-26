claims_simulation <- function(open_ClNr, last_mat){ #function returns a single data frame with the type of closure and settlement lag
  
  #print(open_ClNr)
  #open_ClNr <- 3384
  #open_ClNr <- 447
  #need this to get starting point for markov chain list
  maturity <- 
    sim_claims$maturity[which(sim_claims$ClNr == open_ClNr)]
  
  mtm_code <- 
    sim_claims$mtm_code[which(sim_claims$ClNr == open_ClNr)]
  
  my_mc_list <- new("markovchainList", markovchains = 
      mtm_selections[(maturity):length(mtm_selections)], #Note that maturity here is an index (not an age)
    name = paste(maturity, "to ult", sep = "_")) 
  
  #simulate markov chain results
  sim_data <- markovchain:: rmarkovchain(n = my_iters, object = my_mc_list, 
    t0 = mtm_code, include.t0 = F)
  
  #organize the simulated data by iteration (rows) and transition (columns)
  outcomes <- matrix(sim_data$values, ncol = my_iters, 
    nrow = length((maturity):length(mtm_selections)), 
    dimnames = list((maturity):length(mtm_selections), 
      1:my_iters)) %>%
    t()
  
  final_state <- outcomes[, ncol(outcomes)]
  any_open <- apply(X = outcomes, MARGIN = 1, FUN = function(x) sum(grepl(pattern = "[o]", x)))
  
  #Get the outcomes we actually care about
  #transition to first closed state
  #whether cz or cnz
  outcomes2 <- tibble::as_tibble(outcomes) %>% 
    dplyr::mutate(final_state = final_state, any_open = any_open, 
      ClNr = open_ClNr) %>% 
    dplyr::mutate(
      z_nz = ifelse(stringr::str_detect(final_state, pattern = 'cz'), 'z', 'nz'), #look at the last simulated outcome to get the state.
      settle_lag = dplyr::case_when(
        any_open == 0 ~ 0,
        TRUE ~ 1 + apply(X = outcomes, MARGIN = 1, 
          FUN = function(x) max(grep(pattern = "[o]", x)))  #last open
      ))#Settle lag is in twelve-month intervals
  
  return(outcomes2)
  
}

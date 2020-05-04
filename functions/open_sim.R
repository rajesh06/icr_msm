claims_simulation <- function(open_ClNr, last_mat){ #function returns a single data frame with the type of closure and settlement lag
  
  #print(open_ClNr)
  #open_ClNr <- 1171
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
    t0 = mtm_code, include.t0 = TRUE) %>% 
    dplyr::mutate(current_maturity = maturity) %>% 
    dplyr::mutate(transition = 
        rep(x = 0:length(my_mc_list@markovchains), times = my_iters)) %>% 
    dplyr::mutate(maturity = transition + current_maturity) %>% 
    dplyr::mutate(maturity_label = dplyr::case_when(
      maturity == current_maturity ~ 'current',
      maturity == 13 ~ 'final',
      TRUE ~ as.character(maturity)),
      open = grepl(x = values, pattern = '[o]')
    ) %>% 
    dplyr::mutate(open_trans = open * transition) %>% 
    dplyr::mutate(status = values) %>% 
    dplyr::select(-values)
  
  settle_lag <- sim_data %>% 
    dplyr::group_by(iteration) %>% 
    dplyr::mutate(last_open = max(open_trans) + current_maturity,
      first_closed = dplyr::case_when(
        grepl(x = mtm_code, pattern = '[c]') ~ 0, 
        TRUE ~ 1
      )) %>% 
    dplyr::mutate(first_closed = last_open + first_closed) %>% #wasn't sure if I needed to do this in two steps
    dplyr::mutate(settle_lag = first_closed - current_maturity) %>% 
    dplyr::filter(maturity_label == 'final') %>% 
    dplyr::select(-transition, -maturity, -open_trans) %>% 
    dplyr::mutate(ClNr = open_ClNr, 
      AY = sim_claims$AY[which(sim_claims$ClNr == open_ClNr)],
      cum_paid = sim_claims$cum_paid[which(sim_claims$ClNr == open_ClNr)])
  
  

  return(list(state_sim = sim_data, settle_lag = settle_lag, ClNr = open_ClNr, 
    AY = sim_claims$AY[which(sim_claims$ClNr == open_ClNr)], 
    cum_paid = sim_claims$cum_paid[which(sim_claims$ClNr == open_ClNr)]))
   
  
  
  
  #   
  # 
  # #organize the simulated data by iteration (rows) and transition (columns)
  # outcomes <- matrix(data = sim_data$values, ncol = my_iters + 1, 
  #   nrow = length((maturity):length(mtm_selections)), 
  #   dimnames = list((maturity):length(mtm_selections), 
  #     1:my_iters)) %>%
  #   t()
  # 
  # final_state <- outcomes[, ncol(outcomes)]
  # any_open <- apply(X = outcomes, MARGIN = 1, FUN = function(x) sum(grepl(pattern = "[o]", x)))
  # 
  # #Get the outcomes we actually care about
  # #transition to first closed state
  # #whether cz or cnz
  # outcomes2 <- tibble::as_tibble(outcomes) %>% 
  #   dplyr::mutate(final_state = final_state, any_open = any_open, 
  #     ClNr = open_ClNr) %>% 
  #   dplyr::mutate(
  #     z_nz = ifelse(stringr::str_detect(final_state, pattern = 'cz'), 'z', 'nz'), #look at the last simulated outcome to get the state.
  #     settle_lag = dplyr::case_when(
  #       any_open == 0 ~ 0,
  #       TRUE ~ 1 + apply(X = outcomes, MARGIN = 1, 
  #         FUN = function(x) max(grep(pattern = "[o]", x)))  #last open
  #     ))#Settle lag is in twelve-month intervals
  # 
  return(sim_data)
  
}

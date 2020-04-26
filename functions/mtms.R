create_mtms <- function(start_age){
    #start_age <- 1 #for testing
    
    #maturity interval filter
    mtm_data <- markov_data %>% 
      dplyr::filter(mat_ymyp1 %in% c(start_age, start_age + 1))
    
    #identify subjects with both begining and ending valuations
    eligble_subjects <- mtm_data %>% 
      dplyr::group_by(mc_subject) %>% 
      dplyr::summarise(obs = n()) %>% 
      dplyr::filter(obs == 2) %>% 
      dplyr::pull(mc_subject)
    
    #filter on those subjects
    mtm_data <- mtm_data %>% 
      dplyr::filter(mc_subject %in% eligble_subjects) %>% 
      dplyr::arrange(mc_subject, mat_ymyp1)
    
    #create the transition matrix
    mtm <- msm::statetable.msm(state = mc_status,
      subject = mc_subject, data = mtm_data)
    
    #These are not all the same size; so we need to fix that (by squaring)
    mtm <- square_mats(mat = mtm, states = markov_states)
    
    return(mtm)
    
  }
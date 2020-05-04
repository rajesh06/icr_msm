test_sim <- results_df %>% 
  dplyr::filter(AY == 2005)

# sum(test_sim$cum_paid)/100
# testing
# val_2005 %>% 
#   dplyr::filter(AY == 2005) %>% 
#   dplyr::pull(cum_paid) %>% 
#   sum()

act_ultimates <- test_data %>% 
  dplyr::select(ClNr, AY, dplyr::starts_with('Pay')) %>% 
  dplyr::filter(AY == 2005) %>% 
  tidyr::pivot_longer(dplyr::starts_with('Pay'), names_to = 'increment', 
    values_to = 'paid') %>% 
  dplyr::group_by(ClNr) %>% 
  dplyr::summarise(ultimate = sum(paid))

sim_means <- sapply(X = act_ultimates$ClNr, FUN = function(my_ClNr){ #act_ultimates$ClNr
  test_sim %>% 
    dplyr::filter(ClNr == my_ClNr) %>% 
    dplyr::pull(final_sim_sev) %>% 
    mean()
  
  
}, simplify = TRUE)

plot(x = log(act_ultimates$ultimate), y = log(sim_means))
points(x = c(2,10), y = c(2,10), col = 'red', type = 'l')
write.csv(cbind(act_ultimates, sim_means), file = 'test.csv')
sum(act_ultimates$paid)
sum(sim_means, na.rm = TRUE)
sim_means


plot_mtm <- function(transition_start){
  
  # transition_start <- 10 #testing
  if(transition_start != 'terminal'){
    observed_dat <- mtms[[transition_start]] %>% 
      prop.table(margin = 1) %>% 
      tibble::as_tibble()
  }else{
    observed_dat <- tibble::as_tibble(zero_mat)
  }
  
  p_observed <- ggplot(data = observed_dat,
    mapping = aes(x = to, y = from))+
    geom_tile(mapping = aes(fill = n), 
      show.legend = FALSE)+
    xlab('Ending State')+ylab('Starting State')+
    theme_bw()+
    scale_fill_gradient(low = 'white', 
      high = 'blue4')+
    geom_text(mapping = aes(label = 
        ifelse(round(n, digits = 2) == 0, "", 
          round(n, digits = 2))), size = 3)+
    ggtitle('Observed')+
    theme(axis.text.x = element_text(angle = 90, vjust = 0), 
      panel.grid = element_blank(),
      axis.text.y = element_text(hjust = 1, vjust = 0.35))
  
  if(transition_start != 'terminal'){
    selected_dat <- mtm_selections[[transition_start]] %>% 
      tibble::as_tibble()
  }else{
    selected_dat <- tibble::as_tibble(term_mat)
  }
  
  p_selected <- ggplot(data = selected_dat,
    mapping = aes(x = to, y = from))+
    geom_tile(mapping = aes(fill = n), show.legend = FALSE)+
    xlab('Ending State')+ylab('Starting State')+
    theme_bw()+
    scale_fill_gradient(low = 'white', 
      high = 'blue4')+
    geom_text(mapping = aes(label = 
        ifelse(round(n, digits = 2) == 0, "", 
          round(n, digits = 2))), size = 3)+
    ggtitle('Selected')+
    theme(axis.text.x = element_text(angle = 90, vjust = 0),
      panel.grid = element_blank(),
      axis.text.y = element_text(hjust = 1, vjust = 0.35))
  
  plots <- arrangeGrob(ncol = 2, p_observed, p_selected)
    
  return(plots)
  
}
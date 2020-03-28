square_mats <- function(mat, states) {
  
  
  square_df <- dplyr::left_join(
  # x has all possible values
      x = tidyr::crossing(from = states, to = states), 
  # y (may) only have a some of those
    y = as_tibble(mat))
  
  # Fix the missing starting states
  ## We want 1s on the diagonal
  square_df$n[is.na(square_df$n) & square_df$from == square_df$to] <- 1
  ## and 0s off diagonal
  square_df$n[is.na(square_df$n)] <- 0
  
  square_xtab <- xtabs(n ~ from + to, data = square_df)
  
  # cant convert xtab class to Markovchain
  class(square_xtab) <- "table" 
  
  return(square_xtab)
}
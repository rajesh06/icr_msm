val_date <- 2005

test_data <- dplyr::left_join(
  
  readRDS(file = './data/icr_data.RDS') %>% 
    dplyr::select(ClNr:RepDel, dplyr::starts_with('Pay')) %>% #keep IDs and payment amounts
    tidyr::pivot_longer(cols = Pay00:Pay11, names_to = 'paid_as_of', #wide to long
      values_to = 'paid') %>% 
    dplyr::mutate(maturity = stringr::str_extract(string = paid_as_of, #extract maturity
      pattern = '[0-9]{2}')) %>%
    dplyr::mutate(maturity = as.numeric(maturity) + 1) %>% 
    mutate(cal_year = AY + maturity - 1) %>% 
    #calculate cumulative payments
    dplyr::group_by(ClNr, AY) %>% 
    mutate(cum_paid = cumsum(paid)),
  
  readRDS(file = './data/icr_data.RDS') %>% 
    dplyr::select(ClNr:RepDel, dplyr::starts_with('Open')) %>% #keep IDs and payment amounts
    tidyr::pivot_longer(cols = Open00:Open11, names_to = 'open_as_of', #wide to long
      values_to = 'status') %>% 
    dplyr::mutate(maturity = stringr::str_extract(string = open_as_of, #extract maturity
      pattern = '[0-9]{2}')) %>%
    dplyr::mutate(maturity = as.numeric(maturity) + 1) %>% 
    mutate(cal_year = AY + maturity - 1) %>% 
    dplyr::mutate(status_year = status * cal_year) %>% 
    #calculate cumulative payments
    dplyr::group_by(ClNr, AY) %>% 
    mutate(last_open = max(status_year)) %>% 
    dplyr::select(-status_year)
)

#write.csv(x = test_data, file = 'final_test.csv')

settle_sim_test <- test_data %>% 
  dplyr::filter(ClNr %in% c(4057, 4058), cal_year == val_date) %>% 
  dplyr::mutate(mtm_code = case_when(
    status == 1 & cum_paid == 0 ~ "oz",
    status == 1 & cum_paid > 0 ~ "onz",
    status == 0 & cum_paid == 0 ~ "cz",
    status == 0 & cum_paid > 0 ~ "cnz",)) %>% #assign mtm_code
  dplyr::mutate(
    mtm_code_with_mat = paste(mtm_code, maturity, sep = '_'))

set.seed(14159) #for reproducibility
my_iters = 10
last_mat = 12 #last maturity
sim_claims <- settle_sim_test
state_sims <- lapply(X = list(4057, 4058), FUN = claims_simulation, last_mat)
rm(last_mat, sim_claims)
state_sims

#table(state_sims[[1]]$values[state_sims[[1]]$transition == 1]) #looks pretty good

#Attach Severity Model
full_sim_model <- lapply(X = state_sims, FUN = function(settle_sim) {
  
  cum_paid <- settle_sim$cum_paid
  
  settle_sim$full_sim <- settle_sim$settle_lag %>% 
    dplyr::rename(lag_to_close = settle_lag) %>% 
    dplyr::left_join(severity_model_df)
  
  
  return(settle_sim)
  
}) 


#Add the no pay probabilities


full_sim_model <- lapply(X = full_sim_model, FUN = function(claim){
  claim <- full_sim_model[[1]]
  cum_paid <- claim$cum_paid
  my_df <- claim$full_sim
  
  no_add_pays <- sapply(X = 1:nrow(my_df), FUN = function(row_no){
    row_no <- 1
    lag_to_close <- my_df$lag_to_close[row_no]
    
    no_add_pay <- if(lag_to_close != 0) {
      car_df <-  my_df$no_add_pay[[row_no]]
      car_df$no_add_pay_rate[sum(cum_paid >= car_df$layer_start)]
    } else {
      1
    }
    
    
  }, simplify = TRUE) 
  
  my_df$no_add_pay_pr <- no_add_pays
  my_df$add_pay_sim <- runif(n = my_iters)
  my_df$add_pay <- my_df$add_pay_sim > my_df$no_add_pay_pr
  
  
  claim$full_sim <- my_df
  
  return(claim)
  
}) %>% invisible()

# Now the severity values

full_sim_model <- lapply(X = full_sim_model, FUN = function(claim){
  
  claim <- full_sim_model[[1]]
  cum_paid <- claim$cum_paid
  my_df <- claim$full_sim
  my_newdata <- data.frame('cum_paid' = cum_paid)
  
  ultimates <- sapply(X = 1:nrow(my_df), FUN = function(row_no){
    
    if(my_df$add_pay[[row_no]]) {
      
      #less than exp(10)
      lt10_mod <- my_df$nz_lt_10[[row_no]]
      my_predicted <- predict.glm(object = lt10_mod, newdata = my_newdata)
      shape <- MASS::gamma.shape(lt10_mod)$alpha
      ult_lt10 <- rgamma(n = 1, shape = shape, rate = shape/my_predicted) %>% exp()
      
      #greater than exp(10)
      gt10_mod <- my_df$nz_gt_10[[row_no]]
      my_sd_log <- summary(gt10_model)$sigma
      if(is.nan(my_sd_log)) my_sd_log <- 0
      ult_gt10 <- rlnorm(n = 1, meanlog = predict.lm(object = gt10_model, 
        newdata = my_newdata), sdlog = my_sd_log)
      
      #zeros
      
      ult_z <- sample(my_df$z_dev[[row_no]], size = 1) %>% exp()
      
      dplyr::case_when(
        exp(cum_paid) > 10 ~ ult_gt10,
        cum_paid == 0 ~ ult_z,
        exp(cum_paid) <= 10 ~ ult_gt10,
      )
      
      
    } else {
      return(cum_paid)
    }
    
    
  }, simplify = TRUE) 
  
  my_df$ultimate <- ultimates
  
  claim$full_sim <- my_df
  
  return(claim)
  
    
}) %>% invisible()

full_sim_model
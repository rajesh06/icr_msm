glm(formula = y_var ~ x_var,
  data = plot_data, family = gaussian(link = 'identity'), 
  subset = x_var < 10) %>% summary()

glm(formula = y_var ~ x_var,
  data = plot_data, family = Gamma(link = 'identity'), 
  subset = x_var < 10) %>% summary()

glm(formula = y_var ~ x_var,
  data = plot_data, family = Gamma(link = 'identity'), 
  subset = x_var < 10) %>% MASS::gamma.dispersion()


lm(formula = y_var ~ x_var,
  data = plot_data,  
  subset = x_var < 10) %>% sigma()

plot(x = sev_model_lt10$data$x_var[sev_model_lt10$data$x_var < 10], 
  y = predict(object = sev_model_lt10), col = 'blue', type = 'l')
points(x = sev_model_lt10$data$x_var[sev_model_lt10$data$x_var < 10], 
  y = simulate(object = sev_model_lt10, nsim = 1)[[1]])

test <- simulate(object = sev_model_lt10, nsim = 10)


test_mat <- as.matrix(test)
apply(test_mat[1:100,], MARGIN = 1, FUN = var)

predict.glm(object = sev_model_lt10,  )

ciTools::add_pi(tb = tibble::tibble(x_var = 9), fit = sev_model_lt10)



require(graphics)

## example from Venables and Ripley (2002, pp. 190-2.)
ldose <- rep(0:5, 2)
numdead <- c(1, 4, 9, 13, 18, 20, 0, 2, 6, 10, 12, 16)
sex <- factor(rep(c("M", "F"), c(6, 6)))
SF <- cbind(numdead, numalive = 20-numdead)
budworm.lg <- glm(SF ~ sex*ldose, family = binomial)
summary(budworm.lg)

plot(c(1,32), c(0,1), type = "n", xlab = "dose",
  ylab = "prob", log = "x")
text(2^ldose, numdead/20, as.character(sex))
ld <- seq(0, 5, 0.1)
lines(2^ld, predict(budworm.lg, data.frame(ldose = ld,
  sex = factor(rep("M", length(ld)), levels = levels(sex))),
  type = "response"))
lines(2^ld, predict(budworm.lg, data.frame(ldose = ld,
  sex = factor(rep("F", length(ld)), levels = levels(sex))),
  type = "response"))

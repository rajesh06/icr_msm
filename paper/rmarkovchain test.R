test_mc_list <- my_mc_list
rmarkovchain(n = 10, object = test_mc_list[[2]], t0 = "onz", include.t0 = TRUE)
absorbingStates(test_mc_list)


library(markovchain)
library(dplyr)
#define markovchainList object
statesNames <- c("a", "b", "c")
mcA <- new("markovchain", states = statesNames, transitionMatrix = 
    matrix(c(0.2, 0.5, 0.3, 0, 0.2, 0.8, 0.1, 0.8, 0.1), nrow = 3, 
      byrow = TRUE, dimnames = list(statesNames, statesNames)))
mcB <- new("markovchain", states = statesNames, transitionMatrix = 
    matrix(c(0.2, 0.5, 0.3, 0, 0.2, 0.8, 0.1, 0.8, 0.1), nrow = 3, 
      byrow = TRUE, dimnames = list(statesNames, statesNames)))
mcC <- new("markovchain", states = statesNames, transitionMatrix = 
    #matrix(c(0.2, 0.5, 0.3, 0, 0.2, 0.8, 0.1, 0.8, 0.1), nrow = 3,
      matrix(c(0, 0, 1, 0, 0, 1, 0, 0, 1), nrow = 3, 
      byrow = TRUE, dimnames = list(statesNames, statesNames)))
mclist <- new("markovchainList", markovchains = list(mcA, mcB, mcC)) 
mclist <- new("markovchainList", markovchains = list(mcB, mcC)) 

# show the list of sequence
set.seed(123)
rmarkovchain(n = 3, object = mclist, what = "list", t0 = 'a', include.t0 = TRUE)
set.seed(123)
rmarkovchain(n = 3, object = mclist, what = "list", t0 = 'a', include.t0 = FALSE)


rmarkovchain(10, test_mc_list[[1]], "list",)

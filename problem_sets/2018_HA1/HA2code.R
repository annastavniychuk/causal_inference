### RANDOMIZATION ###
set.seed(42) #это важно

generate_stratum <- function(base_randomization, PO_name) {
  # your code here
  return(stratum)
}

generate_treatment <- function(base_randomization) {
  # your code here
  
}

allocate_canvassers <- function(randomization_with_s_and_t) {
  # your code here
  return(randomization)
}

randomize <- function(base_randomization) {
  # your code here
  return(randomization)
}

### BALANCE ON COVARIATES ###

summary_table <- function(analysis) {
  # your code here
  return(summary_table)
}

balance_on_covariates <- function(analysis) {
  # your code here
  return(balance_on_covariates)
}

### RESULTS ###

make_models <- function(response, previous, controls, data) {
  # your code here
  return(list(model_fe, model_prev, model_full))
}
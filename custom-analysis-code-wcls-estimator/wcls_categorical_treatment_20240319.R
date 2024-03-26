################################################################################
# Code for part 1-6 was received from Tianchen Qian
# Code for part 7 and 8 was added later on and confirmed with Tianchen Qian
################################################################################

library(rootSolve)

get_alpha_beta_from_multiroot_result <- function(root, p, q)
{
  if (p == 1) {
    beta_root <- root$root[q+1]
  } else {
    beta_root <- as.matrix(root$root[(q+1) : (q+p)])
  }
  if (q == 1) {
    alpha_root <- root$root[1]
  } else {
    alpha_root <- as.matrix(root$root[1:q])
  }
  return(list(alpha = alpha_root, beta = beta_root))
}

interact <- function(ind_matrix, ind_names, cov, cov_names){
  # This is a function to create dummy matrix of treatment and 
  ncol_interaction <- ncol(cov) * ncol(ind_matrix)
  df_names <- rep(NA, ncol_interaction)
  Interaction_matrix <- 
    matrix(rep(NA, ncol_interaction * nrow(cov)), ncol = ncol_interaction)
  
  count = 0
  
  for (i in 1:ncol(ind_matrix)) {
    for (j in 1:ncol(cov)) {
      count = count + 1 
      df_names[count] <- paste0(cov_names[j], ":", ind_names[i])
      Interaction_matrix [,count] <- cov[,j] * ind_matrix[,i]
    }
  }
  colnames(Interaction_matrix) <- df_names
  return(Interaction_matrix)
}

find_change_location <- function(v){
  #' Find the locations at which the value changes from the previous one
  # Used when total observation per individual is different, copied from TQ github
  
  n <- length(v)
  if (n <= 1) {
    stop("The vector need to have length > 1.")
  }
  return(c(1, 1 + which(v[1:(n-1)] != v[2:n])))
}

#---------------------------- TQ Estimator -------------------------------------
# TQ estimator can be found in R_Code/ estimator_MEE.R
wcls_categorical_treatment <- function(
    dta,
    id_varname,
    decision_time_varname,
    treatment_varname,
    outcome_varname,
    control_varname,
    moderator_varname,
    rand_prob_varname,
    avail_varname = NULL,
    p_tilde = NULL,
    estimator_initial_value = NULL
)
{
  ### 1. preparation ###
  
  sample_size <- length(unique(dta[, id_varname]))
  total_person_decisionpoint <- nrow(dta)
  total_T <- total_person_decisionpoint / sample_size
  A <- dta[, treatment_varname]
  
  p_t <- dta[, rand_prob_varname]
  
  # assign p_tilde equal for each treatment if p_tilde is not specified
  if (is.null(p_tilde)) {
    nA <- length(unique(A)) # number of treatment
    p_tilde <- rep(1/nA, nA)
  } else {
    nA <- length(p_tilde)
  }
  
  p_t_tilde <- ifelse(A == 0, p_tilde[1], 
                      ifelse(A ==1, p_tilde[2], p_tilde[3]))
  
  # indicator matrix, each column i represent  I(At = i) 
  ind_matrix <- matrix(rep(NA, (length(A) * (nA - 1) )), nrow = length(A))
  # this is the W = I - p_tilde function in our estimator
  ind_center <- matrix(rep(NA, (length(A) * (nA - 1) )), nrow = length(A))
  # column names of indicator matrix
  ind_names <- paste0("trt=", 1:(nA-1))
  for (trt in (1:(nA - 1))) {
    ind_matrix[,trt] <- ifelse(A == trt, 1, 0)
    ind_center[,trt] <- ind_matrix[,trt] - p_tilde[trt + 1]
  }
  
  Y <- dta[, outcome_varname]
  
  # St is the covariates of moderator 
  St <- as.matrix(cbind(1, dta[, moderator_varname]))
  St_names <- c("Intercept", moderator_varname)
  
  # Ht is the covariate of control 
  Ht <- as.matrix(cbind(1, dta[, control_varname[1]]))
  Ht_names <- c("Intercept", control_varname)
  
  
  
  # Xdm what we are interested, Zdm is the one we are not interested 
  Xdm <- interact(ind_matrix, ind_names, St, St_names)  # X (moderator) design matrix, intercept added
  Zdm <- Ht  # Z (control) design matrix, intercept added
  # Wdm is the right most vector of our estimating equation 
  Wdm <- interact(ind_center, ind_names, St, St_names)
  Wnames <- colnames(Wdm)
  if (is.null(avail_varname)) {
    avail <- rep(1, total_person_decisionpoint)
  } else {
    avail <- dta[, avail_varname]
  }
  
  weight <- p_t_tilde / p_t
  
  p <-  (nA - 1) * (length(moderator_varname) + 1) # dimension of beta, need to generalize it later
  q <- length(control_varname) + 1 # dimension of alpha
  
  # for now manually add the name for moderator variable  
  Znames <- c("Intercept", control_varname)
  colnames(Zdm) <- Znames
  
  ### 2. estimation ###
  
  estimating_equation <- function(theta) {
    alpha <- as.matrix(theta[1:q])
    beta <- as.matrix(theta[(q+1):(q+p)])
    
    Zdm_alpha <- Zdm %*% alpha
    Wdm_beta <-  Wdm %*% beta
    
    residual <- Y - Zdm_alpha - Wdm_beta
    
    ef <- rep(NA, length(theta)) # value of estimating function
    for (i in 1:q) {
      ef[i] <- sum( weight * residual * avail * Zdm[, i])
    }
    for (i in 1:p) {
      ef[q + i] <- sum( weight * residual * avail * Wdm[,i])
    }
    
    ef <- ef / sample_size
    return(ef)
  }
  
  if (is.null(estimator_initial_value)) {
    estimator_initial_value <- rep(0, length = p + q)
  }
  
  # browser()
  solution <- tryCatch(
    {
      multiroot(estimating_equation, estimator_initial_value)
    },
    error = function(cond) {
      message("\nCatched error in multiroot inside efficient_ee():")
      message(cond)
      return(list(root = rep(NaN, p + q), msg = cond,
                  f.root = rep(NaN, p + q)))
    })
  
  estimator <- get_alpha_beta_from_multiroot_result(solution, p, q)
  
  alpha_hat <- as.vector(estimator$alpha)
  names(alpha_hat) <- Znames   # give alpha variable names
  beta_hat <- as.vector(estimator$beta)
  names(beta_hat) <- Wnames
  
  ### 3. asymptotic variance ###
  
  ### 3.1 Compute M_n matrix (M_n is the empirical expectation of the derivative of the estimating function) ###
  
  Mn_summand <- array(NA, dim = c(total_person_decisionpoint, p+q, p+q))
  # Mn_summand is  D^{(t),T} \frac{\partial r^(t)}{\partial \theta^T}
  # see May2023week2/variance derivation
  
  D_term_collected <- matrix(NA, nrow = p+q, ncol = total_person_decisionpoint)
  partialr_partialtheta_collected <- matrix(NA, nrow = total_person_decisionpoint, ncol = p+q)
  
  for (it in 1:total_person_decisionpoint) {
    # this is to make R code consistent whether X_it, Z_it contains more entries or is just the intercept.
    if (p == 1) {
      Xbeta <- Xdm[it, ] * beta_hat
    } else {
      Xbeta <- as.numeric(Xdm[it, ] %*% beta_hat)
    }
    if (q == 1) {
      Zalpha <- Zdm[it, ] * alpha_hat
    } else {
      Zalpha <- as.numeric(Zdm[it, ] %*% alpha_hat)
    }
    
    pre_multiplier <-  weight[it]
    
    # D_term = D^{(t),T} (dim = (nA * p+q) * 1)
    D_term <- pre_multiplier * avail[it]  * c(Zdm[it, ], Wdm[it, ])
    D_term_collected[, it] <- D_term
    
    # partialr_partialtheta = \frac{\partial r^(t)}{\partial \theta^T}
    partialr_partialtheta <- - c(Zdm[it, ], Wdm[it, ]) 
    partialr_partialtheta_collected[it, ] <- partialr_partialtheta
    
    Mn_summand[it, , ] <-  D_term %o% partialr_partialtheta
  }
  Mn <- apply(Mn_summand, c(2,3), sum) / sample_size
  Mn_inv <- solve(Mn)
  
  ### 3.2 Compute \Sigma_n matrix (\Sigma_n is the empirical variance of the estimating function) ###
  Zdm_alpha <- Zdm %*% alpha_hat
  Wdm_beta <-  Wdm %*% beta_hat
  residual <-  as.matrix(Y - Zdm_alpha - Wdm_beta)
  
  Sigman_summand <- array(NA, dim = c(sample_size, p+q, p+q))
  # Sigman_summand is  \sum_{t=1}^T ( D^{(t),T} r^(t) )^{\otimes 2}
  
  person_first_index <- c(find_change_location(dta[, id_varname]), total_person_decisionpoint + 1)
  
  
  for (i in 1:sample_size) {
    D_term_i <- D_term_collected[, person_first_index[i] : (person_first_index[i+1] - 1)]
    r_term_i <- matrix(residual[person_first_index[i] : (person_first_index[i+1] - 1)], ncol = 1)
    
    Sigman_summand[i, , ] <- D_term_i %*% r_term_i %*% t(r_term_i) %*% t(D_term_i)
  }
  Sigman <- apply(Sigman_summand, c(2,3), sum) / sample_size
  
  varcov <- Mn_inv %*% Sigman %*% t(Mn_inv) / sample_size
  varcovnames <- c(Znames,Wnames)
  colnames(varcov) <- varcovnames
  rownames(varcov) <- varcovnames
  
  alpha_se <- sqrt(diag(varcov)[1:q]) 
  names(alpha_se) <- Znames
  
  beta_se <- sqrt(diag(varcov)[(q+1):(q+p)])
  names(beta_se) <- Wnames
  
  ### 4. small sample correction ###
  
  Sigman_tilde <- 0
  for (i in 1:sample_size) {
    D_term_i <- D_term_collected[, person_first_index[i]:(person_first_index[i + 1] - 1)]
    r_term_i <- matrix(residual[person_first_index[i] : (person_first_index[i+1] - 1)], ncol = 1)
    partialr_partialtheta_i <- partialr_partialtheta_collected[person_first_index[i]:(person_first_index[i + 1] - 1), ]
    H_ii <- partialr_partialtheta_i %*% Mn_inv %*% D_term_i / sample_size
    Ii_minus_Hii_inv <- solve(diag(nrow(H_ii)) - H_ii)
    
    Sigman_tilde <- Sigman_tilde + D_term_i %*% Ii_minus_Hii_inv %*% r_term_i %*% t(r_term_i) %*% t(Ii_minus_Hii_inv) %*% t(D_term_i)
  }
  Sigman_tilde <- Sigman_tilde / sample_size
  
  varcov_adjusted <- Mn_inv %*% Sigman_tilde %*% t(Mn_inv) / sample_size
  colnames(varcov_adjusted) <- varcovnames
  rownames(varcov_adjusted) <- varcovnames
  
  alpha_se_adjusted <- sqrt(diag(varcov_adjusted)[1:q])
  names(alpha_se_adjusted) <- Znames
  
  beta_se_adjusted <- sqrt(diag(varcov_adjusted)[(q + 1):(q + p)])
  names(beta_se_adjusted) <- Wnames
  
  ### 5. calculate confidence interval
  
  conf_int <- cbind(beta_hat - 1.96 * beta_se, beta_hat + 1.96 * beta_se)
  conf_int_adjusted_z <- cbind(beta_hat - 1.96 * beta_se_adjusted, beta_hat + 1.96 * beta_se_adjusted)
  c <- qt(1 - 0.05/2, df = sample_size - p - q)
  conf_int_adjusted_t <- cbind(beta_hat - c * beta_se_adjusted,
                               beta_hat + c * beta_se_adjusted)
  colnames(conf_int) <- colnames(conf_int_adjusted_t)<- colnames(conf_int_adjusted_z) <- c("2.5 %", "97.5 %")
  row.names(conf_int) <- rownames(conf_int_adjusted_t) <- rownames(conf_int_adjusted_z) <- Wnames
  
  ### 6. calculate p-value
  
  test_stat <- beta_hat / beta_se
  pval <- 2 * pnorm(abs(test_stat), lower.tail = FALSE)
  
  test_stat_adjusted <- beta_hat / beta_se_adjusted
  pval_adjusted_z <- 2 * pnorm(abs(test_stat_adjusted), lower.tail = FALSE)
  pval_adjusted_t <- 2 * pt(abs(test_stat_adjusted), df = sample_size - p - q, lower.tail = FALSE)
  
  ### 7. calculate confidence interval for non-causal parameters
  
  conf_int_alpha_hat <- cbind(alpha_hat - 1.96 * alpha_se, alpha_hat + 1.96 * alpha_se)
  conf_int_alpha_hat_adjusted_z <- cbind(alpha_hat - 1.96 * alpha_se_adjusted, alpha_hat + 1.96 * alpha_se_adjusted)
  c <- qt(1 - 0.05/2, df = sample_size - p - q)
  conf_int_alpha_hat_adjusted_t <- cbind(alpha_hat - c * alpha_se_adjusted,
                                         alpha_hat + c * alpha_se_adjusted)
  colnames(conf_int_alpha_hat) <- colnames(conf_int_alpha_hat_adjusted_t)<- colnames(conf_int_alpha_hat_adjusted_z) <- c("2.5 %", "97.5 %")
  row.names(conf_int_alpha_hat) <- rownames(conf_int_alpha_hat_adjusted_t) <- rownames(conf_int_alpha_hat_adjusted_z) <- Znames
  
  ### 8. calculate p-value for non-causal parameters
  
  test_stat_alpha_hat <- alpha_hat / alpha_se
  pval_alpha_hat <- 2 * pnorm(abs(test_stat_alpha_hat), lower.tail = FALSE)
  
  test_stat_alpha_hat_adjusted <- alpha_hat / alpha_se_adjusted
  pval_alpha_hat_adjusted_z <- 2 * pnorm(abs(test_stat_alpha_hat_adjusted), lower.tail = FALSE)
  pval_alpha_hat_adjusted_t <- 2 * pt(abs(test_stat_alpha_hat_adjusted), df = sample_size - p - q, lower.tail = FALSE)
  
  
  return(list(beta_hat = beta_hat, alpha_hat = alpha_hat, 
              beta_se = beta_se, alpha_se = alpha_se, 
              beta_se_adjusted_md = beta_se_adjusted,
              alpha_se_adjusted_md = alpha_se_adjusted,
              varcov = varcov,
              varcov_adjusted = varcov_adjusted, 
              # Causal part of the model
              beta_ci = conf_int, 
              beta_ci_adjusted_zquantile = conf_int_adjusted_z,
              beta_ci_adjusted_tquantile = conf_int_adjusted_t,
              beta_pval = pval,
              beta_pval_adjusted_zquantile = pval_adjusted_z,
              beta_pval_adjusted_tquantile = pval_adjusted_t,
              # Non-causal part of the model
              alpha_ci = conf_int_alpha_hat, 
              alpha_ci_adjusted_zquantile = conf_int_alpha_hat_adjusted_z,
              alpha_ci_adjusted_tquantile = conf_int_alpha_hat_adjusted_t,
              alpha_pval = pval_alpha_hat,
              alpha_pval_adjusted_zquantile = pval_alpha_hat_adjusted_z,
              alpha_pval_adjusted_tquantile = pval_alpha_hat_adjusted_t,
              # Other output
              dims = list(p = p, q = q), 
              p_tilde = p_tilde)
  )
  
}


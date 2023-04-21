


mice_ninr <- function(data,
                      m = 5,
                      maxit = 5,
                      print = FALSE,
                      control_data,
                      intervention_data,
                      control_delta = c(0, 0, 0, 0),
                      intervention_delta = c(0,-50, 100,-1000),
                      additive = TRUE,
                      dependent_var) {
  imp <- mice(data,
              maxit = 0,
              printFlag = FALSE)

  post <- imp$post

  delta_G0 <- as.list(control_delta)
  df_G0 <- rep(list(control_data), length(delta_G0))

  delta_G1 <- as.list(intervention_delta)
  df_G1 <- rep(list(intervention_data), length(delta_G1))

  additive <- as.list(rep(additive, length(delta_G0)))
  post <- rep(list(imp$post), length(delta_G0))
  dependent_var <- as.list(rep(dependent_var, length(delta_G0)))

  parameters_G0 <-
    list(delta_G0, df_G0, additive, post, dependent_var)
  parameters_G1 <-
    list(delta_G1, df_G1, additive, post, dependent_var)

  list_mids_G0 <- purrr::pmap(parameters_G0, postFunction, m = m, maxit = maxit, print = print)
  list_mids_G1 <- purrr::pmap(parameters_G1, postFunction, m = m, maxit = maxit, print = print)

  parameters_mids <- list(list_mids_G1, list_mids_G0)

  list_mids_G1_G0 <- purrr::pmap(parameters_mids, sens_combined)
}

postFunction <- function(i, data, additive, post, dependent_var, m = 5, maxit = 5, print = FALSE) {
  if (additive == TRUE) {
    operation = "+"
  }
  else if (additive == FALSE) {
    operation = "*"
  }
  d <- i
  cmd <- paste("imp[[j]][,i] <- imp[[j]][,i]", operation, d)
  post[paste(dependent_var)] <- cmd
  imp <- mice(
    data,
    post = post,
    maxit = maxit,
    m = m,
    print = print
  )
  return(imp)
}

sens_combined <- function(mids1, mids2) {
  data_G1 <- complete(mids1, action = 'long', include = TRUE)
  data_G0 <- complete(mids2, action = 'long', include = TRUE) %>%
    mutate(.id = as.character(as.numeric(.id) + 2000))

  data_combined <- rbind(data_G1, data_G0) %>%
    mutate(.id = as.numeric(.id)) %>%
    arrange(.imp, .id)

  mids_G1_G0 <- as.mids(data_combined)
  return(mids_G1_G0)
}

pool_function <- function(mids, delta_0, delta_1, equation) {
  eq <- parse(text = equation)
  df <- as.data.frame(summary(mice::pool(with(
    data = mids,
    expr = eq
  )))) %>%
    mutate(delta_0 = delta_0,
           delta_1 = delta_1)
  return(df)
}

combine_pool <- function(list_mids, delta_G0, delta_G1, equation, additive = TRUE){
  equation_vec <- rep(equation, length(list_mids))
  parameters <- list(list_mids, delta_G0, delta_G1, equation_vec)
  fit <- purrr::pmap_dfr(parameters, pool_function)
  if(additive == FALSE){
    fit <- fit %>%
      mutate(delta_0 = exp(delta_0),
             delta_1 = exp(delta_1))
  }
  return(fit)
}

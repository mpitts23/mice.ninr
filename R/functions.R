

#' Title
#'
#' @param data
#' @param m
#' @param maxit
#' @param printFlag
#' @param control_data
#' @param intervention_data
#' @param control_delta
#' @param intervention_delta
#' @param additive
#' @param dependent_var
#'
#' @return
#' @export
#'
#' @examples
mice_ninr <- function(data,
                      m = 5,
                      maxit = 5,
                      printFlag = FALSE,
                      control_data,
                      intervention_data,
                      control_delta = c(0, 0, 0, 0),
                      intervention_delta = c(0, -50, 100, -1000),
                      additive = TRUE,
                      dependent_var){

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

  parameters_G0 <- list(delta_G0, df_G0, additive, post, dependent_var)
  parameters_G1 <- list(delta_G1, df_G1, additive, post, dependent_var)

  list_mids_G0 <- purrr::pmap(parameters_G0, postFunction)
  list_mids_G1 <- purrr::pmap(parameters_G1, postFunction)

  parameters_mids <- list(list_mids_G1, list_mids_G0)

  list_mids_G1_G0 <- purrr::pmap(parameters_mids, sens_combined)
}

#' Title
#'
#' @param i
#' @param data
#' @param additive
#' @param post
#' @param dependent_var
#'
#' @return
#' @export
#'
#' @examples
postFunction <- function(i, data, additive, post, dependent_var) {

  if(additive == TRUE){
    operation = "+"
  }
  else if(additive == FALSE){
    operation = "*"
  }
  d <- i
  cmd <- paste("imp[[j]][,i] <- imp[[j]][,i]", operation, d)
  post[paste(dependent_var)] <- cmd
  imp <- mice(data,
              post = post,
              maxit = 1,
              m = 200,
              print = FALSE
  )
  return(imp)
}

#' Title
#'
#' @param mids1
#' @param mids2
#'
#' @return
#' @export
#'
#' @examples
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


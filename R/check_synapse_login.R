#' Check Synapse Login Status & Ability to Access Data
#'
#' The `check_synapse_login()` function assesses whether the
#' user is logged into Synapse and confirms whether the
#' user has permission to access the GENIE BPC data.
#'
#' @return Returns message indicating user is logged into
#' Synapse and has permission to access the GENIE BPC data.

check_synapse_login <- function() {
  if ("synapser" %in% rownames(utils::installed.packages()) == FALSE) {
    t1 <- FALSE
    # return(TRUE)
    stop("Please install the package synapser from http://ran.synapse.org")
  } else {
    t1 <- TRUE
  }
  t2 <- try(
    synapser::synLogin(),
   silent = TRUE)

  t3 <- try(
    synapser::synGet("syn26948075"), silent = TRUE)

  return( if(t1 == FALSE || inherits(t2,"try-error")||inherits(t3,"try-error")) {
    FALSE
  }else{
    TRUE
  }
  )
}

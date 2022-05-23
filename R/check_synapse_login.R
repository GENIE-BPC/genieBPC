#' Check Synapse Login Status & Ability to Access Data
#'
#' The `check_synapse_login()` function assesses whether the
#' user is logged into Synapse and confirms whether the
#' user has permission to access the GENIE BPC data.
#'
#' @return Returns message indicating user is logged into
#' Synapse and has permission to access the GENIE BPC data.

check_synapse_login <- function() {
  # if synapser is not installed, then return FALSE
  if ("synapser" %in% rownames(utils::installed.packages()) == FALSE) {
    t1 <- FALSE

  } else { # if synapser is installed, try to login and access the data
    t1 <- TRUE

    t2 <- try(
      synapser::synLogin(),
      silent = TRUE
    )

    t3 <- try(
      synapser::synGet("syn26948075"),
      silent = TRUE
    )

    # return FALSE if no synapse or unable to login or access data
    return(if (t1 == FALSE || inherits(t2, "try-error") || inherits(t3, "try-error")) {
      FALSE
    } else { # return TRUE if synapse installed and able to login and access data
      TRUE
    })
  }
}

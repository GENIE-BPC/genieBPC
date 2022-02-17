#' Check Synapse Login Status & Ability to Access Data
#'
#' The `check_synapse_login()` function assesses whether the
#' user is logged into Synapse and confirms whether the
#' user has permission to access the GENIE BPC data.
#'
#' @return Returns message indicating user is logged into
#' Synapse and has permission to access the GENIE BPC data.

check_synapse_login <- function() {
  if("synapser" %in% rownames(utils::installed.packages()) == FALSE) {
     t1 <- FALSE
    #install.packages("synapser", repos = "http://ran.synapse.org")
    stop("Please install the package synapser from http://ran.synapse.org")
  }else{
    t1 <- TRUE
  }
 t2 <- tryCatch(
    synapser::synLogin(),
    error =
      function(e) {
        cli::cli_alert_warning(
          paste("There was an error accessing Synapse.",
                "See error message below."))
        paste("You are not logged into your synapse account",
              "Please set credentials by using 'synapser::synLogin()'",
              "To store login credentials in your operating system, call:",
              "`synLogin(email, password, rememberMe = TRUE)`",
              "Note: Upon calling `rememberMe = TRUE`, the user can call:",
              "`synLogin()` in future uses without specifying credentials.",
              sep = "\n"
        ) %>%
          stop(call. = FALSE)
      }
  )
  t3 <- tryCatch(
    synapser::synGet("syn26948075"),
    error =
      function(e) {
        cli::cli_alert_warning(
          paste("There was an error pulling data from Synapse.",
                "See error message below."))
        paste("You are not able to access the data.",
              "Please make sure you have permission to access",
              "the GENIE BPC data on Synapse.",
              sep = "\n"
        ) %>%
          stop(call. = FALSE)
      }
  )
  print("Great job! You are logged into Synapse and
        can access the GENIE BPC data.")
  return( if(t1 == FALSE || inherits(t2,"try-error")||inherits(t3,"try-error")) {
    errorlist = TRUE
  }else{
    errorlist = FALSE
        }
)
}

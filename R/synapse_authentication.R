# Create Package Environment ---------------------------------------------------
# set environment in which to store URL variable that persists for that session
genieBPC_env <- rlang::new_environment()


# User-facing Authorization Functions -----------------------------------------

#' Connect to 'Synapse' API
#'
#' This function sets 'Synapse' credentials for the user's current session.
#'
#' To access data, users must have a valid 'Synapse' account with permission to
#' access the data set and they must have accepted any necessary 'Terms of Use'.
#' Users must authenticate themselves in their current R session.
#' (See README 'Data Access and Authentication' at https://genie-bpc.github.io/genieBPC/ for details).
#' To set your 'Synapse' credentials during each session, call:
#'  `set_synapse_credentials(username = "your_username", password = "your_password")`.
#'
#' In addition to passing your 'Synapse' username and password, you may choose to set
#' your 'Synapse' Personal Access Token (PAT) by calling:
#'  `set_synapse_credentials(pat = "your_pat")`.
#'
#' If your credentials are stored as environmental variables, you do not need to call
#' `set_synapse_credentials()` explicitly each session. To store authentication
#' information in your environmental variables, add the following to your
#' .Renviron file, then restart your R session
#' (tip: you can use `usethis::edit_r_environ()` to easily open/edit this file):
#'
#' \itemize{
#'    \item `SYNAPSE_USERNAME = <your-username>`
#'    \item `SYNAPSE_PASSWORD = <your-password>`
#'    \item `SYNAPSE_PAT = <your-pat>`
#'    }
#'
#' Alternatively, you can pass your username and password or your PAT to each individual
#' data pull function if preferred, although it is recommended that you manage
#' your passwords outside of your scripts for security purposes.
#'
#' @param username 'Synapse' username. If NULL, package will search
#' environmental variables for `SYNAPSE_USERNAME`.
#' @param password 'Synapse' password. If NULL, package will search
#' environmental variables for `SYNAPSE_PASSWORD`.
#' @param pat 'Synapse' Personal Access Token. If NULL, package will search
#' environmental variables for `SYNAPSE_PAT`.
#' @return A success message if you credentials are valid for 'Synapse'
#' platform; otherwise an error
#'
#' @author Karissa Whiting
#' @export
#'
#' @examples
#' \dontrun{
#' set_synapse_credentials(
#'   username = "your-username",
#'   password = "your-password"
#' )
#' set_synapse_credentials(
#'   pat = "your-personal-access-token"
#' )
#' }
#'
set_synapse_credentials <- function(username = NULL,
                                    password = NULL,
                                    pat = NULL) {

  # remove any former objects in environment
  suppressWarnings(
    rm(list = c("password", "username", "pat"), envir = genieBPC_env))

  # Use Passed Arguments First ----------------------------------------------

  # * If PAT passed, use that -----------
  if(!is.null(pat)) {

    verify_cred_works <- .get_token_by_pat(pat)
    assign("pat", value = pat, envir = genieBPC_env)
    cli::cli_alert_success("You are now connected to 'Synapse' with your {.field Personal Access Token} ({.code pat}) for this R session!")
    return(invisible())
  }

  # * If no PAT but username or password --------
  null_username_password <- sum(purrr::map_lgl(list(username, password), ~is.null(.x)))

  if(sum(null_username_password) < 2) {

    # Throw error if only username or password given but not both
    if(sum(null_username_password) == 1) {
      rlang::abort("User must specify either a `pat`, or BOTH a `username` & `password`.")
    }

    verify_cred_works <- .get_token_by_username(username, password)

    # assign username credentials to package environment
    assign("username",
           value = username,
           envir = genieBPC_env
    )

    assign("password",
           value = password,
           envir = genieBPC_env
    )

    cli::cli_alert_success("You are now connected to 'Synapse' as
                         {.field {username}} for this R session!")
    return(invisible())
  }

  # If None Passed, Use Env Arguments  -------------

  # Try saved PAT
  pat_from_env <- Sys.getenv("SYNAPSE_PAT", unset = NA)
  username_from_env <- Sys.getenv("SYNAPSE_USERNAME", unset = NA)
  password_from_env <- Sys.getenv("SYNAPSE_PASSWORD", unset = NA)

  if(!is.na(pat_from_env)) {

    verify_cred_works <- .get_token_by_pat(pat_from_env)
    assign("pat", value = pat_from_env, envir = genieBPC_env)
    cli::cli_alert_success("You are now connected to 'Synapse' with your {.field Personal Access Token} ({.code SYNAPSE_PAT}) for this R session!")
    return()

  } else if(all(!is.na(c(username_from_env, password_from_env)))) {

    verify_cred_works <- .get_token_by_username(username_from_env, password_from_env)

    # assign username credentials to package environment
    assign("username",
           value = username_from_env,
           envir = genieBPC_env
    )

    assign("password",
           value = password_from_env,
           envir = genieBPC_env
    )
    cli::cli_alert_success("You are now connected to 'Synapse' as
                         {.field {username_from_env}} for this R session!")
    return(invisible())
  } else {
    rlang::abort("No personal access token (`pat`), `username` or `password` specified and no
    `SYNAPSE_PAT`, `SYNAPSE_USERNAME` or `SYNAPSE_PASSWORD` in `.Renviron`.
    Try specifying `pat`, `username` or `password` arguments
                 or use `usethis::edit_r_environ()` to add to your global variables")
  }
}




#
# set_synapse_credentials <- function(username = NULL, password = NULL) {
#
#   # if no args passed, check sys environment
#   resolved_username <- username %||% Sys.getenv("SYNAPSE_USERNAME", unset = NA)
#   resolved_password <- password %||% Sys.getenv("SYNAPSE_PASSWORD", unset = NA)
#
#   switch(any(is.na(c(resolved_username, resolved_password))),
#          {
#            rlang::abort("No `username` or `password` specified and no
#                    `SYNAPSE_USERNAME` or `SYNAPSE_PASSWORD` in `.Renviron`.
#                    Try specifying `username` or `password` arguments or use
#                    `usethis::edit_r_environ()` to add to your global variables")
#          }
#   )
#
#   # assign credentials to package environment
#   assign("username",
#          value = resolved_username,
#          envir = genieBPC_env
#   )
#
#   assign("password",
#          value = resolved_password,
#          envir = genieBPC_env
#   )
#
#   # check the credentials with a call
#   x <- .get_synapse_token(
#     username = resolved_username,
#     password = resolved_password
#   )
#
#   cli::cli_alert_success("You are now connected to 'Synapse' as
#                          {.field {resolved_username}} for this R session!")
# }

#' Check Access to GENIE Data
#'
#' @param username 'Synapse' username. If NULL, package will search package
#'   environment for "username".
#' @param password 'Synapse' password. If NULL, package will search package
#'   environment for "password".
#' @param pat 'Synapse' Personal Access Token. If NULL, package will search package
#'   environment for "pat".
#' @param check_consortium_access Specifies whether access to GENIE BPC consortium
#'   data releases (vs. public data releases) is checked. Default is FALSE,
#'   indicating that access to GENIE BPC public data releases is checked instead.
#' @return A success message if you are able to access GENIE BPC data; otherwise
#'   an error
#' @author Karissa Whiting
#' @export
#'
#' @examples
#' \dontrun{
#' # if credentials are saved:
#' check_genie_access()
#' }
check_genie_access <- function(username = NULL,
                               password = NULL,
                               pat = NULL,
                               check_consortium_access = FALSE) {

  # first get token
  token <- .get_synapse_token(username = username,
                              password = password,
                              pat = pat)

  # now do genie-specific test query

  if(check_consortium_access) {
    # private GENIE (consortium)
    query_url <- "https://repo-prod.prod.sagebase.org/repo/v1/entity/syn21241322/bundle2"
  } else {
    # public GENIE
    query_url <- "https://repo-prod.prod.sagebase.org/repo/v1/entity/syn27056172/bundle2"
  }


  requested_objects <- jsonlite::toJSON(list(
    "includeEntity" = TRUE,
    "includeAnnotations" = TRUE,
    "includeFileHandles" = TRUE,
    "includeRestrictionInformation" = TRUE
  ),
  pretty = TRUE, auto_unbox = TRUE
  )

  res <- httr::POST(
    url = query_url,
    body = requested_objects,
    httr::add_headers(
      Authorization =
        paste("Bearer ", token, sep = "")
    ),
    httr::content_type("application/json")
  )

  if(check_consortium_access) {
    error_message <- c("access GENIE consortium data in 'Synapse'. Consortium releases are not available to individuals outside of the AACR Project GENIE BPC Consortium. Please check you've accepted Terms of use, or try accessing a publicly available release instead.")
  } else {
    error_message <- c("access GENIE data in 'Synapse'. Check that you have permission to view this data")
  }

  httr::stop_for_status(res, error_message)
  cli::cli_alert_success("{httr::http_status(res)$message}:
                         You are successfully connected to the GENIE database!")
}


#  Utility Functions for Authorization -----------------------------------------

#' Get an object from the genieBPC package environment (`genieBPC_env`)
#'
#' @param thing_to_check
#'
#' @return The object
#' @keywords internal
#' @export
#' @examples
#' \dontrun{
#' .get_env("username")
#' }
.get_env <- function(thing_to_check) {
  thing <- tryCatch(
    {
      get(thing_to_check,
        envir = genieBPC_env
      )
    },
    error = function(e) {
      return(NULL)
    }
  )
  thing
}

#' Retrieve a synapse token using username and password
#'
#' @inheritParams check_genie_access
#'
#' @return a 'Synapse' token. Will first look for PAT and return that if successful HTTP call, else
#' will try with username and password
#' @keywords internal
#' @export
#'
#' @examples
#' \dontrun{
#' .get_synapse_token(username = "test", password = "test")
#' }
.get_synapse_token <- function(username = NULL, password = NULL, pat = NULL) {

  # check user passed pat
  if(!is.null(pat)) {
    token <- .get_token_by_pat(pat)
    return(token)
  }

  # check user passed username/password
  if(!is.null(username)) {
    if(is.null(password)) {
      rlang::abort("User must specify either a `pat`, or BOTH a `username` & `password`.")
    } else {
      token <- .get_token_by_username(username = username, password = password)
      return(token)
    }
  }

  # if no user passed args
  resolved_pat <- pat %||% .get_env("pat")
  resolved_username <- username %||% .get_env("username")
  resolved_password <- password %||% .get_env("password")

  # if none found in env
  if(is.null(resolved_pat)) {
    if(is.null(resolved_username) | is.null(resolved_password)) {
    cli::cli_abort("No credentials found. Have you authenticated yourself?
                     Use {.code set_synapse_credentials()} to set credentials for this session, or pass a {.code pat}, or {.code username} AND {.code password}
                     (see README:'Data Access & Authentication' for details on authentication).")
    }
  }

  # use package env creds
  if(!is.null(resolved_pat)) {
    token <- .get_token_by_pat(resolved_pat)
    return(token)
  } else if(is.null(resolved_pat)) {
    token <- .get_token_by_username(resolved_username, resolved_password)
    return(token)
  }

}

#' Check Synapse Login Status & Ability to Access Data
#'
#' The `.is_connected_to_genie()` function assesses whether the
#' user is logged into 'Synapse' and confirms whether the
#' user has permission to access the GENIE BPC data. It returns TRUE or FALSE
#' @keywords internal
#' @return Returns message indicating user is logged into
#' 'Synapse' and has permission to access the GENIE BPC data.
#' @export
#' @examples
#' .is_connected_to_genie()
.is_connected_to_genie <- function(username = NULL, password = NULL,
                                   pat = NULL,
                                   check_consortium_access = FALSE) {
  tryCatch(check_genie_access(username = username,
                              password = password,
                              pat = pat,
                              check_consortium_access = check_consortium_access),
    error = function(e) FALSE,
    message = function(m) TRUE
  )
}


#' Retrieve a synapse token using username and password
#'
#' @inheritParams check_genie_access
#'
#' @return a 'Synapse' token.
#' @keywords internal
#' @export
#'
#' @examples
#' \dontrun{
#' .get_token_by_username(username = "test", password = "test")
#' }
.get_token_by_username <- function(username, password) {

  # query to get token
  requested_objects <- jsonlite::toJSON(list(
    "email" = username,
    "password" = password
  ), pretty = TRUE, auto_unbox = TRUE)

  resp <- httr::POST("https://auth-prod.prod.sagebase.org/auth/v1/session",
                     body = requested_objects,
                     encode = "json",
                     httr::add_headers(`accept` = "application/json"),
                     httr::content_type("application/json")
  )

  token <- httr::content(resp, "parsed")$sessionToken

  if (is.null(token) || any(token == "" | token == " ")) {
    cli::cli_abort("There was an error authenticating your
                   username ({username}) or password.
                   Please make sure you can login to the 'Synapse' website
                   with the given credentials.")
  }

  return(token)

}

#' Retrieve a synapse token using personal access token
#'
#' @inheritParams check_genie_access
#'
#' @return a 'Synapse' token
#' @keywords internal
#' @export
#'
#' @examples
#' \dontrun{
#' .get_token_by_pat(pat = "test")
#' }
.get_token_by_pat <- function(pat) {

  if(is.null(pat) | pat == "" | pat == " ") {
    cli::cli_abort("Failed to authenticate with PAT. Please check your PAT or supply a username and password")
  }
  resp <- httr::GET(
    paste0("https://auth-prod.prod.sagebase.org/repo/v1/userProfile"),
    httr::add_headers(Authorization = paste("Bearer ",
                                            pat,
                                            sep = "")),
    #   body = requested_objects,
    encode = "json",
    httr::add_headers(`accept` = "application/json"),
    httr::content_type("application/json")
  ) %>%
    httr::stop_for_status("Failed to authenticate with PAT. Please check your PAT or supply a username and password")

  return(pat)
}

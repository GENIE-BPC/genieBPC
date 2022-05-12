# ------------------------------------------------------------------------------
# set environment in which to store URL variable that persists for that session
genieBPC_env <- rlang::new_environment()

# ------------------------------------------------------------------------------
#' Connect to Synapse API
#'
#' This function sets Synapse credentials for the user's current session
#' @param username Synapse username
#' @param password Synapse password
#' @export
#' @examples
#' \dontrun{
#' set_synapse_credentials(username = "your-username"
#' password = "your-password")
#' }
#'
set_synapse_credentials <- function(username = NULL, password = NULL) {

  # if no args passed, check environment db
  resolved_username <- username %||% Sys.getenv("SYNAPSE_USERNAME", unset = NA)
  resolved_password <- password %||% Sys.getenv("SYNAPSE_PASSWORD", unset = NA)

  switch(
    any(is.na(c(resolved_username, resolved_password))),{
    rlang::abort("No `username` or `password` specified and no `SYNAPSE_USERNAME` or `SYNAPSE_PASSWORD` in `.Renviron`.
      Try specifying `username` or `password` arguments or use `usethis::edit_r_environ()` to add to your global variables")
  })

  assign("username",
         value = resolved_username,
         envir = genieBPC_env)

  assign("password",
         value = resolved_password,
         envir = genieBPC_env)

  x <- get_synapse_token(username = resolved_username, password = resolved_password)

  cli::cli_alert_success("You are now connected to Synpase as {.field {resolved_username}} for this R session!")
}

.get_env <- function(thing_to_check) {

  thing <- tryCatch({
    get(thing_to_check,
        envir = genieBPC_env)},
    error = function(e) return(NULL))

  thing
}

get_synapse_token <- function(username = NULL, password = NULL) {

  resolved_username <- username %||% .get_env("username") %||%
    Sys.getenv("SYNAPSE_USERNAME", unset = NA)

  resolved_password <- password %||% .get_env("password") %||%
    Sys.getenv("SYNAPSE_PASSWORD", unset = NA)


  # ensure a username and password is supplied---------------------------------
  switch(
    any(is.na(c(resolved_username, resolved_password))),
    cli::cli_abort("No credentials found. See {.code set_synapse_credentials()} or pass a {.code username} and {.code password}"))

  # query to get token --------------------------------------------------------
  requestedObjects = list(
    'email'= resolved_username,
    'password'= resolved_password)

  print(requestedObjects)
  body_format = jsonlite::toJSON(requestedObjects, pretty = T, auto_unbox = T)

  resp <- httr::POST("https://auth-prod.prod.sagebase.org/auth/v1/session",
                     body= body_format,
                     encode = "json",
                     httr::add_headers(`accept` = 'application/json'),
                     httr::content_type('application/json'))

  token <- httr::content(resp, "parsed")$sessionToken

  token %||%
    cli::cli_abort('There was an error authenticating your username ({resolved_username}) or password.
                   Please make sure you can login to the Synapse website with the given credentials.')

  return(token)

}

check_genie_access <- function(username = NULL, password = NULL) {

  token <- get_synapse_token(username, password)

    query_url = "https://repo-prod.prod.sagebase.org/repo/v1/entity/syn26948075/bundle2"

    requestedObjects = list(
          'includeEntity'= TRUE,
          'includeAnnotations'= TRUE,
          'includeFileHandles'= TRUE,
          'includeRestrictionInformation'= TRUE)

    body_format = jsonlite::toJSON(requestedObjects, pretty = T, auto_unbox = T)

    res <- httr::POST(url = query_url,
                      body = body_format,
                      httr::add_headers(Authorization = paste("Bearer ", token,
                                                                       sep = "")),
                      httr::content_type('application/json'))

    content <- httr::content(res)
    httr::stop_for_status(res, "access Genie data in Synapse. Check that you have permission to view this data")

    cli::cli_alert_success('{httr::http_status(res)$message}: You are successfully connected!')

}

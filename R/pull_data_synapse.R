#' Obtain Clinical Data Files for GENIE BPC Project
#'
#' The `pull_data_synapse` function accesses the specified
#'  version of the clinical and genomic GENIE BPC data from
#'  \href{https://www.synapse.org/#!Synapse:syn21226493/wiki/599164}{Synapse}
#'  and reads it into the R environment.
#'   Documentation corresponding to the clinical data files
#'   can also be found on Synapse in the Analytic Data Guide:
#' \itemize{
#'   \item \href{https://www.synapse.org/#!Synapse:syn23002641}{NSCLC v1.1-Consortium Analytic Data Guide}
#'   \item \href{https://www.synapse.org/#!Synapse:syn26008058}{NSCLC v2.1-Consortium Analytic Data Guide}
#'   \item \href{https://www.synapse.org/#!Synapse:syn23764204}{CRC v1.1-Consortium Analytic Data Guide}
#'   \item \href{https://www.synapse.org/#!Synapse:syn26077308}{CRC v1.2-Consortium Analytic Data Guide}
#'   \item \href{https://www.synapse.org/#!Synapse:syn26077313}{BrCa v1.1-Consortium Analytic Data Guide}
#' }
#' Users must log in to Synapse to access the data successfully.
#' To set your Synapse credentials during each session, call:
#' `set_synapse_credentials(email = "your_email", password = "your_password")`
#' To store authentication information in your environmental variables, add the
#' following to your .Renviron file (tip: you can use usethis::edit_r_environ() to easily open/edit this file):
#' `SYNAPSE_USERNAME = <your-email>`
#' `SYNAPSE_PASSWORD = <your-password>`
#' Alternatively, you can pass your username and password to each individual data pull function if preferred,
#' although it is recommended that you manage your passwords outside of your scripts for security purposes.
#'
#' @param cohort Vector or list specifying the cohort(s) of interest.
#'  Must be one of "NSCLC" (Non-Small Cell Lung Cancer),
#'   "CRC" (Colorectal Cancer), or "BrCa" (Breast Cancer).
#' @param version Vector specifying the version of the data. Must be one of the following:
#' "v1.1-consortium", "v2.1-consortium", "v1.2-consortium". When entering multiple cohorts, the order of the
#' version numbers corresponds to the order that the cohorts
#' are specified; the cohort and version number must be in
#' the same order in order to pull the correct data. See examples below.
#' @param download_location if `NULL` (default), data will be returned as a list of dataframes with
#' requested data as list items. Otherwise, specify a folder path to have data automatically downloaded there.
#' @param username Synapse username
#' @param password Synapse password
#'
#' @return Returns clinical and genomic data corresponding to
#' the specified cohort(s). Data frames have the suffix
#' indicating the cohort appended to their name,
#' e.g. pt_char_NSCLC for the pt_char dataset of
#' the NSCLC cohort.
#' @author Karissa Whiting, Michael Curry
#' @export
#'
#' @examples
#' if(genieBPC:::.check_synapse_login() == TRUE){
#' # Example 1 ----------------------------------
#' # Pull non-small cell lung cancer data
#'
#' set_synapse_credentials(username = "your-username",
#'  password = "your-password")
#' pull_data_synapse(cohort = "NSCLC", version = "v2.1-consortium")
#'
#' # Example 2 ----------------------------------
#' # Pull the most recent non-small cell lung cancer
#' # data and the most recent colorectal cancer data
#' pull_data_synapse(
#'   cohort = c("NSCLC", "CRC"),
#'   version = c("v2.1-consortium", "v1.2-consortium")
#' )
#'
#' # Example 3 ----------------------------------
#' # Pull version 2.1 for non-small cell lung cancer
#' # and version 1.1 for colorectal cancer data
#' t <- pull_data_synapse(
#'   cohort = c("CRC", "BrCa"),
#'   version = c("v1.2-consortium", "v1.1-consortium")
#' )
#' }
#'
#' @import
#' dplyr
#' dtplyr
#' purrr


pull_data_synapse <- function(cohort = NULL, version = NULL,
                              download_location = NULL,
                              username = NULL, password = NULL) {

  # Check parameters ---------------------------------------------------------

  # Make sure credentials are available and get token ---
  token <- .get_synapse_token(username = username, password = password)

  # get `cohort` ---
  select_cohort <- rlang::arg_match(cohort, c("NSCLC", "CRC", "BrCa"),
    multiple = TRUE
  )

  # check `version` ---
  version <- version %>%
    purrr::when(
      is.null(.) ~ cli::cli_abort("Version needs to be specified.
                            Use {.code synapse_version()} to see what data is available."),
      setdiff(., unique(synapse_tables$version)) > 0 ~
        cli::cli_abort("{.code version} must be one of the following: {unique(synapse_tables$version)}"),
      length(select_cohort) < length(.) ~ cli::cli_abort("You have selected more versions than cancer cohorts.
             Make sure cohort and version inputs have the same length.
         Use {.code synapse_version()} to see what data is available"),
      TRUE ~ rlang::arg_match(., unique(synapse_tables$version), multiple = TRUE))

  # create `version-number` ---
  sv <- select(genieBPC::synapse_tables, .data$cohort, .data$version) %>%
    distinct()

  version_num <- bind_cols(list("cohort" = select_cohort, "version" = version))

  version_not_available <- anti_join(version_num, sv, by = c("cohort", "version"))

  if (nrow(version_not_available) > 0) {
    cli::cli_abort(c("You have selected a version that is not available for this cohort
                   (use `synapse_tables` to see what versions are available):",
    "x" = "{.val {version_not_available}}"))
  }

  version_num <- version_num %>%
    inner_join(sv, ., by = c("cohort", "version")) %>%
    mutate(version_num = stringr::str_remove(paste(.data$cohort,
                                                   .data$version,
                                                   sep = "_"), "-consortium"))

  # check download_location ---

  # adds folders for each cohort/version (if doesn't exist)
  version_num <- version_num %>%
    mutate(download_folder = .check_download_path(download_location = download_location,
                                                 version_num))

  # Prep data for query -----------------------------------------------------

  # get synapse IDs
  version_num_df <-
    genieBPC::synapse_tables %>%
    left_join(version_num, ., by = c("version", "cohort"))

  version_num_df_nest <-  version_num_df %>%
   split(., .$version_num)

  return_items <- purrr::map(version_num_df_nest,
             ~.pull_data_by_cohort(version_num_df = .x, token = token,
                                   download_location = download_location))

  switch(is.null(download_location), return(return_items))
}


#' Function to retrieve data by synapse ID
#'
#' @param version_num_df a dataframe of synapse IDs
#' @param token a synapse token
#' @param download_location if `NULL` (default), data will be returned as a list of dataframes with
#' requested data as list items. Otherwise, specify a folder path to have data automatically downloaded there.
#'
#' @return downloaded synapse data as a list if `download_location`= `NULL, or to a local path
#' @keywords internal
#' @export
#'
#' @examples
#' syn_df <- data.frame(
#'   synapse_id =
#'     c("syn26046793", "syn26046791", "syn26046792"),
#'     df = c("pt_char", "ca_dx_index", "ca_dx_non_index"),
#'     version_num = c("CRC_v1.1", "CRC_v1.1", "CRC_v1.1")
#' )
#' syn_df <- data.frame(
#'   cohort = c("NSCLC", "NSCLC", "NSCLC"),
#'   version = c("v2.1-consortium", "v2.1-consortium", "v2.1-consortium"),
#'   version_num = c("NSCLC_v2.1", "NSCLC_v2.1", "NSCLC_v2.1"),
#'   download_folder = c("/Users/kwhiting/Repositories/github-repos/genieBPC/NSCLC_v2.1",
#'                       "/Users/kwhiting/Repositories/github-repos/genieBPC/NSCLC_v2.1",
#'                       "/Users/kwhiting/Repositories/github-repos/genieBPC/NSCLC_v2.1"),
#'   df = c("pt_char", "ca_dx_index", "ca_dx_non_index"),
#'   synapse_id = c("syn25985884", "syn25985882", "syn25985883")
#' )
#'
#' .pull_data_by_cohort(
#'   version_num_df = syn_df,
#'   token = .get_synapse_token(), download_location = here::here()
#' )
#'
#
.pull_data_by_cohort <- function(version_num_df,
                                 token, download_location) {


  repo_endpoint_url <- "https://repo-prod.prod.sagebase.org/repo/v1/entity/"
  file_endpoint_url <- "https://file-prod.prod.sagebase.org/file/v1/fileHandle/batch"


  # Get file metadata (python equivalent is getEntityBundle) -------------------

  # we need file handle ID and filename
  file_metadata <- version_num_df %>%
    mutate(query_url = paste0(repo_endpoint_url, synapse_id, "/bundle2")) %>%
    mutate(file_info = map(.data$query_url, function(x) {
      requestedObjects <- list(
        "includeEntity" = TRUE,
        "includeAnnotations" = TRUE,
        "includeFileHandles" = TRUE,
        "includeRestrictionInformation" = TRUE
      )


      res_per_id <- httr::POST(
        url = x,
        body = jsonlite::toJSON(requestedObjects,
          pretty = T,
          auto_unbox = T
        ),
        httr::add_headers(Authorization = paste("Bearer ", token, sep = "")),
        httr::content_type("application/json")
      )

      entityBundle <- httr::content(res_per_id, "parsed", encoding = "UTF-8")
      file_info <- entityBundle$fileHandles[[1]]

      bind_cols(
        type = file_info$contentType,
        name = file_info$fileName,
        file_handle_id = file_info$id
      )
    }))

  # Get data by URL -----------------------------------------------------------

  # file index- files must being csv or txt
  ids_txt_csv <- file_metadata %>%
    tidyr::unnest(cols = file_info) %>%
    filter(type %in% c("text/csv", "text/plain"))

  files <- ids_txt_csv %>%
    select(.data$version_num, .data$file_handle_id, .data$synapse_id, .data$df,
           .data$name, .data$download_folder) %>%
    purrr::pmap(., .get_and_query_file_url, download_location)

  return(files)
  # maybe get rid of the _cohort?- would be nice to keep synapse file name
  # switch(!is.null(files),
  #        files %>% rlang::set_names(., paste(ids_txt_csv$df, cohort, sep = "_")))
}

# Synapse Utility Functions ----------------------------------------------------

#' Check download_path user passed and create folder if needed
#'
#' @param download_location a local path or NULL
#' @param version_num vector of cohort/version_number
#'
#' @return a vector of file paths. If download_location is NULL, will return
#' temporary file path
#' @keywords internal
#' @export
#'
#' @examples
#' .check_download_path(download_location = NULL, version_num = "CRC_v2.1")
#'
.check_download_path <- function(download_location, version_num) {

  download_location_resolved <- download_location %||%
    tempdir()

  map_chr(version_num, function(single_version_num) {
    folder_path <- file.path(download_location_resolved, single_version_num)
    switch(!dir.exists(folder_path), dir.create(folder_path))
    return(folder_path)
  })


}


#' Get URL for a given synapse file and download to local machine
#'
#' @param version_num synpase cohort_version
#' @param file_handle_id synapse file handle ID
#' @param synapse_id synapse ID
#' @param df package designated name of file
#' @param name file name from synapse
#' @param version_num cohort name and version
#' @param download_folder location to download data
#'
#' @return
#' @keywords internal
#' @export
#'
#' @examples
#' file = data.frame(
#' version_num = "NSCLC_v2.1",
#' file_handle_id = c("79432768"),
#' synapse_id = c("syn25985884"),
#' df = c("pt_char"),
#' name = c("patient_level_dataset.csv"),
#' download_folder = file.path(tempdir(), "NSCLC_v2.1"))
#'
#' pmap(file, .get_and_query_url)
#'
.get_and_query_file_url <- function(version_num, file_handle_id, synapse_id,
                               df, name, download_folder, download_location) {

  body <- list(
    "includeFileHandles" = TRUE,
    "includePreSignedURLs" = TRUE,
    "requestedFiles" = as.data.frame(list(
      "fileHandleId" = file_handle_id,
      "associateObjectId" = synapse_id,
      "associateObjectType" = "FileEntity"
    ))
  )

  res <- httr::POST(
    url = file_endpoint_url,
    body = jsonlite::toJSON(body, pretty = T, auto_unbox = T),
    httr::content_type("application/json"),
    httr::add_headers(Authorization = paste("Bearer ", token, sep = ""))
  )

  parsed <- httr::content(res, "parsed", encoding = "UTF-8")
  pre_signed_url <- parsed$requestedFiles[1][[1]]$preSignedURL
  file_type <- parsed$requestedFiles[1][[1]]$fileHandle$contentType

  resolved_file_path <- file.path(download_folder, name)

  res2 <- httr::GET(
    url = pre_signed_url,
    httr::content_type("application/json"),
    httr::write_disk(resolved_file_path, overwrite = TRUE)
  )

  # `download_location` from outside function
  if(is.null(download_location)) {
    returned_files <- file_type %>%
      purrr::when(
        . == "text/csv" ~ read.csv(resolved_file_path),
        . == "text/plain" ~ utils::read.delim(resolved_file_path, sep = "\t"),
        TRUE ~ cli::cli_abort("Cannot read objects of type {file_type}.
                                    Try downloading directly to disk with {.code download_location}"))

      cli::cli_alert_success("{.field {df}} has been imported for {.val {version_num}}")
      return(returned_files)
  } else {
    cli::cli_alert_success("{.field {name}} has been downloaded to {.val {download_folder}}")
    return(invisible(NULL))
  }


}


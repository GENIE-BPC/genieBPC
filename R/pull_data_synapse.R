#' Obtain clinical & genomic data files for GENIE BPC Project
#'
#' Function to access specified
#'  versions of clinical and genomic GENIE BPC data from
#'  \href{https://www.synapse.org/#!Synapse:syn21226493/wiki/599164}{Synapse}
#'  and read them into the R environment. See the \href{https://genie-bpc.github.io/genieBPC/articles/pull_data_synapse_vignette.html}{pull_data_synapse vignette}
#' for further documentation and examples.
#'
#' @param cohort Vector or list specifying the cohort(s) of interest. Must be
#'   one of "NSCLC" (Non-Small Cell Lung Cancer), "CRC" (Colorectal Cancer), or
#'   "BrCa" (Breast Cancer), "PANC" (Pancreatic Cancer), "Prostate" (Prostate Cancer),
#'   and "BLADDER" (Bladder Cancer). This is not case sensitive.
#' @param version Vector specifying the version of the cohort. Must match one of the
#'   release versions available for the specified `cohort` (see `synapse_version()` for available cohort versions).
#'   When entering multiple cohorts, it is inferred that the order of the version
#'   numbers passed corresponds to the order of the cohorts passed.
#'   Therefore, `cohort` and `version` must be in the same
#'   order to ensure the correct data versions are pulled. See examples below for details.
#' @param download_location if `NULL` (default), data will be returned as a list
#'   of dataframes with requested data as list items. Otherwise, specify a
#'   folder path to have data automatically downloaded there. When a path is
#'   specified, data are not read into the R environment.
#' @param username 'Synapse' username
#' @param password 'Synapse' password
#' @param pat 'Synapse' personal access token
#'
#' @section Authentication:
#' To access data, users must have a valid 'Synapse' account with permission to
#' access the data set and they must have accepted any necessary 'Terms of Use'.
#' Users must always authenticate themselves in their current R session.
#' (see \href{https://genie-bpc.github.io/genieBPC/articles/pull_data_synapse_vignette.html}{README: Data Access and Authentication}
#'
#' for details).
#' To set your 'Synapse' credentials during each session, call:
#'
#' `set_synapse_credentials(username = "your_username", password = "your_password")`
#'
#' In addition to passing your 'Synapse' username and password, you may choose to set
#' your 'Synapse' Personal Access Token (PAT) by calling:
#'  `set_synapse_credentials(pat = "your_pat")`.
#'
#' If your credentials are stored as environmental variables, you do not need to
#' call `set_synapse_credentials()` explicitly each session. To store
#' authentication information in your environmental variables, add the following
#' to your .Renviron file, then restart your R session ' (tip: you can use
#' `usethis::edit_r_environ()` to easily open/edit this file):
#'
#' \itemize{
#'    \item `SYNAPSE_USERNAME = <your-username>`
#'    \item `SYNAPSE_PASSWORD = <your-password>`
#'    \item `SYNAPSE_PAT = <your-pat>`
#'    }
#' Alternatively, you can pass your username and password or your PAT to each individual
#' data pull function if preferred, although it is recommended that you manage
#' your passwords outside of your scripts for security purposes.
#'
#' @section Analytic Data Guides:
#'   Documentation corresponding to the clinical data files
#'   can be found on 'Synapse' in the Analytic Data Guides:
#' \itemize{
#'   \item \href{https://www.synapse.org/#!Synapse:syn23002641}{NSCLC v1.1-Consortium Analytic Data Guide}
#'   \item \href{https://www.synapse.org/#!Synapse:syn53463493}{NSCLC v2.2-Consortium Analytic Data Guide}
#'   \item \href{https://www.synapse.org/#!Synapse:syn30557304}{NSCLC v2.0-Public Analytic Data Guide}
#'   \item \href{https://www.synapse.org/#!Synapse:syn58597690}{NSCLC v3.1-Consortium Analytic Data Guide}
#'   \item \href{https://www.synapse.org/#!Synapse:syn53463650}{CRC v1.3-Consortium Analytic Data Guide}
#'   \item \href{https://www.synapse.org/#!Synapse:syn31751466}{CRC v2.0-Public Analytic Data Guide}
#'   \item \href{https://www.synapse.org/#!Synapse:syn26077313}{BrCa v1.1-Consortium Analytic Data Guide}
#'   \item \href{https://www.synapse.org/#!Synapse:syn32330194}{BrCa v1.2-Consortium Analytic Data Guide}
#'   \item \href{https://www.synapse.org/#!Synapse:syn30787692}{BLADDER v1.1-Consortium Analytic Data Guide}
#'   \item \href{https://www.synapse.org/#!Synapse:syn53018714}{BLADDER v1.2-Consortium Analytic Data Guide}
#'   \item \href{https://www.synapse.org/#!Synapse:syn29787285}{PANC v1.1-Consortium Analytic Data Guide}
#'   \item \href{https://www.synapse.org/#!Synapse:syn50612821}{PANC v1.2-Consortium Analytic Data Guide}
#'   \item \href{https://www.synapse.org/#!Synapse:syn30148714}{Prostate v1.1-Consortium Analytic Data Guide}
#'   \item \href{https://www.synapse.org/#!Synapse:syn50612204}{Prostate v1.2-Consortium Analytic Data Guide}
#' }
#'
#' @return Returns a nested list of clinical and genomic data corresponding to
#' the specified cohort(s).
#'
#' @author Karissa Whiting, Michael Curry
#' @export
#'
#' @examplesIf genieBPC::.is_connected_to_genie(pat = Sys.getenv("SYNAPSE_PAT"))
#' # Example 1 ----------------------------------
#' # Set up 'Synapse' credentials
#' set_synapse_credentials()
#'
#' # Print available versions of the data
#' synapse_version(most_recent = TRUE)
#'
#' # Pull version 2.0-public for non-small cell lung cancer
#' # and version 2.0-public for colorectal cancer data
#'
#'  ex1 <- pull_data_synapse(
#'    cohort = c("NSCLC", "CRC"),
#'    version = c("v2.0-public", "v2.0-public")
#'  )
#'
#'  names(ex1)
#'
#' @import
#' dplyr
#' dtplyr
#' purrr


pull_data_synapse <- function(cohort = NULL, version = NULL,
                              download_location = NULL,
                              username = NULL, password = NULL, pat = NULL) {

  # Check parameters ---------------------------------------------------------

  # Make sure credentials are available and get token ---
  token <- .get_synapse_token(username = username,
                                       password = password,
                                       pat = pat)

  # get `cohort` ---
  if(is.null(cohort)){

    cli::cli_abort("Cohort needs to be specified.
                Use {.code synapse_version()} to see what data is available.")

  }

  # make cohort term not be case sensitive - will require update as new disease areas are added
  cohort <- dplyr::case_when(
    stringr::str_to_upper(cohort)=="NSCLC" |
    stringr::str_to_upper(cohort)=="NON-SMALL CELL LUNG CANCER" |
    stringr::str_to_upper(cohort)=="NON SMALL CELL LUNG CANCER" |
    stringr::str_to_upper(cohort)=="NONSMALL CELL LUNG CANCER"~ "NSCLC",
    stringr::str_to_upper(cohort)=="CRC" | stringr::str_to_upper(cohort)=="COLORECTAL CANCER" ~ "CRC",
    stringr::str_to_upper(cohort)=="BRCA" | stringr::str_to_upper(cohort)=="BREAST CANCER"~ "BrCa",
    stringr::str_to_upper(cohort)=="BLADDER" ~ "BLADDER",
    stringr::str_to_upper(cohort)=="PANC" | stringr::str_to_upper(cohort)=="PANCREAS" ~ "PANC",
    stringr::str_to_upper(cohort)=="PROSTATE" ~ "Prostate",
    # last condition to avoid error message:
    # '`cohort` must be a single string, not a character `NA`.'
    # when an NA is fed into arg_match below
    TRUE ~ cohort
  )

  select_cohort <- rlang::arg_match(cohort, c("NSCLC", "CRC", "BrCa", "BLADDER", "PANC", "Prostate"),
    multiple = TRUE
  )

  # check `version` ---

  if(is.null(version)){

    cli::cli_abort("Version needs to be specified.
                Use {.code synapse_version()} to see what data is available.")

  } else if (length(select_cohort) < length(version)){

    cli::cli_abort(
        "You have selected more versions than cancer cohorts.
             Make sure cohort and version inputs have the same length.
         Use {.code synapse_version()} to see what data is available")

  } else {
    # create `version-number` ---
    sv <- dplyr::select(genieBPC::synapse_tables, "cohort", "version") %>%
      dplyr::distinct()

    version_num <- dplyr::bind_cols(list("cohort" = select_cohort,
                                         "version" = version))

    # specific messaging when a version that was previously available is no longer
    # available
    # removed versions
    removed_versions <- dplyr::tribble(~cohort, ~version,
                                       "NSCLC", "v2.1-consortium",
                                       "CRC", "v1.1-consortium",
                                       "CRC", "v1.2-consortium")

    # only print for one removed version at a time
    specified_version_removed <- dplyr::inner_join(removed_versions,
                                                   version_num,
                                                   by = c("cohort", "version")) %>%
      dplyr::slice(1)

    # version specified that doesn't exist
    version_not_available <- dplyr::anti_join(version_num, sv,
                                              by = c("cohort", "version"))

     if (nrow(specified_version_removed) >0){
      cli::cli_abort(c("The {.val {paste0(specified_version_removed, collapse = ' ')}} data release is no longer available. AACR is asking users to delete any local copies of the data and re-run analyses using more recently released data (use `synapse_tables` to see the currently available versions)",
                       "x" = "{.val {paste0(specified_version_removed, collapse = ' ')}}"
      ))
    } else if (nrow(version_not_available) > 0 |
               length(setdiff(version, unique(genieBPC::synapse_tables$version))) > 0) {
      cli::cli_abort(c("You have selected a version that is not available for
                     this cohort (use {.code synapse_version()} to see what versions
                     are available):",
                     "x" = "{.val {version_not_available}}"
      ))
      } else {
        rlang::arg_match(version, unique(genieBPC::synapse_tables$version),
                              multiple = TRUE)
      }
    }

  # If consortium data requested, check that consortium access is granted for account
  if(any(str_detect(version_num$version, "consortium"))) {
    suppressMessages(check_genie_access(pat = token, check_consortium_access = TRUE))
  }

  version_num <- version_num %>%
    dplyr::inner_join(sv, ., by = c("cohort", "version")) %>%
    dplyr::mutate(version_num = case_when(
      grepl("consortium", version) ~ stringr::str_remove(paste(.data$cohort,
        .data$version,
        sep = "_"
      ), "-consortium"),
      grepl("public", version) ~ stringr::str_remove(
        paste(.data$cohort,
          .data$version,
          sep = "_"
        ),
        "-public"
      )
    ))

  # check download_location ---

  # adds folders for each cohort/version (if doesn't exist)
  version_num <- version_num %>%
    dplyr::mutate(download_folder = .check_download_path(
      download_location = download_location,
      version_num
    ))

  # Prep data for query -----------------------------------------------------

  # get synapse IDs
  version_num_df <-
    genieBPC::synapse_tables %>%
    left_join(version_num, ., by = c("version", "cohort"))

  version_num_df_nest <- version_num_df %>%
    split(., .$version_num)

  return_items <- purrr::map(
    version_num_df_nest,
    ~ .pull_data_by_cohort(
      version_num_df = .x, token = token,
      download_location = download_location
    )
  )

  switch(is.null(download_location),
    return(return_items)
  )
}


#' Function to retrieve data by synapse ID
#'
#' @param version_num_df a dataframe of 'Synapse' IDs
#' @param token a 'Synapse' token
#' @param download_location if `NULL` (default), data will be returned as a list
#'   of dataframes with requested data as list items. Otherwise, specify a
#'   folder path to have data automatically downloaded there.
#'
#' @return downloaded 'Synapse' data as a list if `download_location`= `NULL, or
#'   to a local path
#' @keywords internal
#' @export
#'
#' @examplesIf genieBPC::.is_connected_to_genie(pat = Sys.getenv("SYNAPSE_PAT"))
#'
#' temp_directory <- tempdir()
#'
#' syn_df <- data.frame(
#'   cohort = c("NSCLC", "NSCLC"),
#'   version = c("v2.2-consortium", "v2.0-public"),
#'   version_num = c("NSCLC_v2.2", "NSCLC_v2.0"),
#'   download_folder = c(temp_directory, temp_directory),
#'   df = c("pt_char", "ca_dx_index"),
#'   synapse_id = c("syn53470868", "syn30350575")
#' )
#'
#' .pull_data_by_cohort(
#'   version_num_df = syn_df,
#'   token = .get_synapse_token(), download_location = NULL
#' )
#'
#' #
.pull_data_by_cohort <- function(version_num_df,
                                 token, download_location) {
  repo_endpoint_url <- "https://repo-prod.prod.sagebase.org/repo/v1/entity/"
  file_endpoint_url <- "https://file-prod.prod.sagebase.org/file/v1/fileHandle/batch"


  # Get file metadata (python equivalent is getEntityBundle) -------------------

  # we need file handle ID and filename
  file_metadata <- version_num_df %>%
    dplyr::mutate(query_url = paste0(repo_endpoint_url,
                                     .data$synapse_id, "/bundle2")) %>%
    dplyr::mutate(file_info = map(.data$query_url, function(x) {
      requestedObjects <- list(
        "includeEntity" = TRUE,
        "includeAnnotations" = TRUE,
        "includeFileHandles" = TRUE,
        "includeRestrictionInformation" = TRUE
      )


      res_per_id <- httr::POST(
        url = x,
        body = jsonlite::toJSON(requestedObjects,
          pretty = TRUE,
          auto_unbox = TRUE
        ),
        httr::add_headers(Authorization = paste("Bearer ", token, sep = "")),
        httr::content_type("application/json")
      )

      entityBundle <- httr::content(res_per_id, "parsed", encoding = "UTF-8")

      # If you haven't signed terms
      switch(entityBundle$restrictionInformation$hasUnmetAccessRequirement,
             cli::cli_abort("Your 'Synapse' account has unmet access requirements.
                          Have you accepted the 'Terms of Use' for this dataset?
                          See 'Synapse' portal (`https://www.synapse.org/`) for more info.")
      )

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
    tidyr::unnest(cols = "file_info") %>%
    filter(.data$type %in% c("text/csv", "text/plain"))

  files <- ids_txt_csv %>%
    dplyr::select(
      "version_num", "file_handle_id", "synapse_id", "df",
      "name", "download_folder"
    ) %>%
    purrr::pmap(
      ., .get_and_query_file_url, download_location,
      token, file_endpoint_url
    )


  # maybe get rid of the _cohort?- would be nice to keep synapse file name
  files <- rlang::set_names(files, ids_txt_csv$df)

  return(files)
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
    switch(!dir.exists(folder_path),
      dir.create(folder_path)
    )
    return(folder_path)
  })
}




#' Get URL for a given 'Synapse' file and download to local machine
#'
#' @param version_num 'Synapse' cohort_version
#' @param file_handle_id 'Synapse' file handle ID
#' @param synapse_id 'Synapse' ID
#' @param df package designated name of file
#' @param name file name from 'Synapse'
#' @param version_num cohort name and version
#' @param download_folder location to download data
#' @param token Synapse token
#' @param file_endpoint_url 'Synapse' endpoint for file info
#'
#' @return list of 'Synapse' data frames
#' @keywords internal
#' @export
#'
#' @examplesIf FALSE
#'
#' file <- data.frame(
#'   version_num = "NSCLC_v2.1",
#'   file_handle_id = c("79432768"),
#'   synapse_id = c("syn25985884"),
#'   df = c("pt_char"),
#'   name = c("patient_level_dataset.csv"),
#'   download_folder = file.path(tempdir(), "NSCLC_v2.1")
#' )
#'
#' purrr::pmap(file, .get_and_query_file_url)
#'
.get_and_query_file_url <- function(version_num, file_handle_id, synapse_id,
                                    df, name, download_folder,
                                    download_location,
                                    token, file_endpoint_url) {
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
    body = jsonlite::toJSON(body, pretty = TRUE, auto_unbox = TRUE),
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
  if (is.null(download_location)) {

    if (file_type == "text/csv"){

      returned_files <- utils::read.csv(resolved_file_path,
                                        na.strings = c("", "NA"))

    } else if (file_type == "text/plain") {

      returned_files <- utils::read.delim(resolved_file_path, sep = "\t",
                                          na.strings = c("", "NA"))

    } else {
      cli::cli_abort(
          "Cannot read objects of type {file_type}.
          Try downloading directly to disk with {.code download_location}")
    }

    cli::cli_alert_success(
      "{.field {df}} has been imported for {.val {version_num}}")
    return(returned_files)
  } else {
    cli::cli_alert_success(
      "{.field {name}} has been downloaded to {.val {download_folder}}")
    return(invisible(NULL))
  }
}

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
#' To log into Synapse during each session, call:
#' `synLogin(email = "your_email", password = "your_password")`
#' To store authentication information in your operating system, call:
#' `synLogin(email="your_email",password="your_password",rememberMe=TRUE)`
#' Upon calling the `rememberMe = TRUE` argument, the user can call:
#' `synLogin()` in future uses without specifying login credentials.
#'
#' @param cohort Vector or list specifying the cohort(s) of interest.
#'  Must be one of "NSCLC" (Non-Small Cell Lung Cancer) or
#'   "CRC" (Colorectal Cancer).
#' @param version Vector or list specifying the version of the data.
#' By default, the most recent version is pulled.
#' Currently, only version 1.1 is available.
#' When entering multiple cohorts, the order of the
#' version numbers corresponds to the order that the cohorts
#' are specified; the cohort and version number must be in
#' the same order in order to pull the correct data. See examples below.
#'
#' @return Returns clinical and genomic data corresponding to
#' the specified cohort(s). Data frames have the suffix
#' indicating the cohort appended to their name,
#' e.g. pt_char_NSCLC for the pt_char dataset of
#' the NSCLC cohort.
#' @author Michael Curry
#' @export
#'
#' @examples
#' if(genieBPC:::check_synapse_login() == TRUE){
#' # Example 1 ----------------------------------
#' # Pull non-small cell lung cancer data
#'
#'  pull_data_synapse(cohort = "NSCLC", version = "2.1-consortium")
#'
#' # Example 2 ----------------------------------
#' # Pull the most recent non-small cell lung cancer
#' # data and the most recent colorectal cancer data
#'  pull_data_synapse(cohort = c("NSCLC", "CRC"),
#'  version = c("2.1-consortium", "1.2-consortium"))
#'
#' # Example 3 ----------------------------------
#' # Pull version 2.1 for non-small cell lung cancer
#'  #and version 1.1 for colorectal cancer data
#'  pull_data_synapse(
#'   cohort = c("NSCLC", "CRC"),
#'   version = c("2.1-consortium", "1.1-consortium")
#'  )
#' }
#' @import
#' dplyr
#' dtplyr
#' purrr


pull_data_synapse <- function(cohort, version,
                              download_location = NULL,
                              username = NULL, password = NULL) {


  token <- get_synapse_token(username = username, password = password)
  synapse_tables <- genieBPC::synapse_tables

  tryCatch(
    {
      # check parameters
      if (missing(cohort)) {
        stop("Select cohort from 'NSCLC', 'CRC' or 'BrCa'")
      }
      if (sum(!grepl("^CRC$|^NSCLC$|^BRCA$", stringr::str_to_upper(cohort)))
          > 0) {
        stop("Select cohort from 'NSCLC', 'CRC' or 'BrCa")
      }
      if (missing(version)) {
        stop("Version needs to be specified. Use `synapse_version()` to see
             what data is available.")
      }
      if(length(cohort) < length(version)){
        stop("You have selected more versions than cancer cohorts.
             Make sure cohort and version inputs have the same length")
      }

      versionnum <- dplyr::distinct(synapse_tables,cohort, version)

      versionnum <- dplyr::mutate(versionnum,
                                  version = substr(version,2,nchar(version)),
                                  cohort = toupper(.data$cohort))

      cohortval <- toupper(cohort)

      versionnum <- dplyr::filter(versionnum, cohort %in% cohortval)

      if(!all(version %in% unique(versionnum$version))){
        stop("You have selected a version that is not
        available for this cohort. Please use `synapse_tables`
             to see what versions are available.")
      }
      # get lists of available versions for Synapse tables and
      # corresponding file names, appended with cohort name
      synapse_tables$version <- substr(
        synapse_tables$version, 2,
        nchar(synapse_tables$version))

      synapse_tables$filenames <- paste(
        synapse_tables$df, synapse_tables$cohort, sep = "_")

      cohort_version <- Map(
        function(x, y) {
          synapse_tables[stringr::str_to_upper(
            synapse_tables$cohort) %in% x & synapse_tables$version %in% y, ]
        },
        stringr::str_to_upper(cohort),
        version
      )

      synapse_tables2 <- do.call(rbind, cohort_version)

      .pull_by_synapse_ids(synapse_ids_df = synapse_tables2['synapse_id'],
                           token = token,
                           download_location)

    },

    # return error messages
    error = function(e) {
      cli::cli_alert_warning(paste("There was an error pulling the data.",
      "See error message below."))
      stop(e)
    }
  )
}


.pull_by_synapse_ids <- function(synapse_ids_df, token, download_location) {

  repo_endpoint_url <- "https://repo-prod.prod.sagebase.org/repo/v1/entity/"
  file_endpoint_url <- "https://file-prod.prod.sagebase.org/file/v1/fileHandle/batch"

  # Get file metadata (getEntityBundle) --------------------------------------

  file_metadata <- synapse_ids_df %>%
    mutate(query_url = paste0(repo_endpoint_url, synapse_id, "/bundle2")) %>%
    mutate(file_info = map(query_url, function(x) {

      requestedObjects <- list(
        "includeEntity" = TRUE,
        "includeAnnotations" = TRUE,
        "includeFileHandles" = TRUE,
        "includeRestrictionInformation" = TRUE
      )

      body_format <- jsonlite::toJSON(requestedObjects,
        pretty = T,
        auto_unbox = T
      )

      res_per_id <- httr::POST(
        url = x,
        body = body_format,
        httr::add_headers(Authorization = paste("Bearer ",
          token,
          sep = ""
        )),
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

  # files must being csv or txt
  ids_txt_csv <- file_metadata %>%
    tidyr::unnest(cols = file_info) %>%
    filter(type %in% c("text/csv", "text/plain"))


  t <- map2(
    ids_txt_csv$file_handle_id,
    ids_txt_csv$synapse_id,
    function(x, y) {
      body <- list(
        "includeFileHandles" = TRUE,
        "includePreSignedURLs" = TRUE,
        "requestedFiles" = as.data.frame(list(
          "fileHandleId" = x,
          "associateObjectId" = y,
          "associateObjectType" = "FileEntity"
        ))
      )

      body_format <-
        res <- httr::POST(
          url = file_endpoint_url,
          body = jsonlite::toJSON(body, pretty = T, auto_unbox = T),
          httr::content_type("application/json"),
          httr::add_headers(Authorization = paste("Bearer ", token, sep = ""))
        )

      parsed <- httr::content(res, "parsed", encoding = "UTF-8")
      pre_signed_url <- parsed$requestedFiles[1][[1]]$preSignedURL
      file_name <- parsed$requestedFiles[1][[1]]$fileHandle$fileName
      file_type <- parsed$requestedFiles[1][[1]]$fileHandle$contentType

      resolved_file_path <- download_location %||% tempdir() %>%
        file.path(., file_name)

      res2 <- httr::GET(
        url = pre_signed_url,
        httr::content_type("application/json"),
        httr::write_disk(resolved_file_path, overwrite = TRUE)
      )

      if(is.null(download_location)) {
        final_files <- file_type %>%
          purrr::when(
            . == "text/csv" ~ read.csv(resolved_file_path),
            . == "text/plain" ~ utils::read.delim(resolved_file_path, sep = "\t"),
            TRUE ~ cli::cli_abort("Cannot read objects of type {file_type}.
                                  Try downloading directly to disk with {.code download_loaction}"))

        return(final_files)
      } else {
        cli::cli_alert_success("{.field {file_name}} has been downloaded to {.val {resolved_folder_path}}")
      }

    }
  )
#
#   t <- t %>%
#     set_names(ids_txt_csv$name)
}



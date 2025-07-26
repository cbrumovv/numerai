#' Staked Models Performance Series Data Processing
#'
#' @title Query and Process Numerai Staked Models Performance Data
#' 
#' @description
#' This script queries payout information on staked Numerai models via the GraphQL API
#' and processes the nested JSON responses into flat CSV files for analysis.
#' 
#' @details
#' The script handles the complex nested data structures returned by the Numerai API,
#' specifically processing intraRoundSubmissionScores and filtering for MMC and FNC_v4
#' display names. Each model's performance data is saved as a separate CSV file.
#' 
#' @author Craigile J. Brumfield, CFA
#' @date 2022-09-04
#' @version 2.0
#' 
#' @section API Requirements:
#' Requires valid Numerai API credentials set as environment variables:
#' \itemize{
#'   \item NUMERAI_DATASERIES_PUBLIC_KEY
#'   \item NUMERAI_DATASERIES_API_KEY
#' }
#' 
#' @section Dependencies:
#' \itemize{
#'   \item tidyverse - Data manipulation and processing
#'   \item Rnumerai - Numerai API interface
#'   \item graphql_utils.R - Custom GraphQL query functions
#' }
#' 
#' @section Output:
#' CSV files saved to ./staked/ directory, one per model with format: {model_name}.csv
#' 
#' @keywords numerai api data-processing performance-analysis
#' 
#' @examples
#' \dontrun{
#' # Set environment variables first
#' Sys.setenv(NUMERAI_DATASERIES_PUBLIC_KEY = "your_public_key")
#' Sys.setenv(NUMERAI_DATASERIES_API_KEY = "your_api_key")
#' 
#' # Source and run
#' source("staked_models_perfseries.R")
#' 
#' # Or run specific functions
#' results <- process_all_staked_models()
#' }
#' 
#' @seealso 
#' \url{https://docs.numer.ai/} for Numerai API documentation
#' 
NULL

##
## Script Implementation
## Date:  4 sept 2022
## Last edit:  25 july 2025 - Improved with roxygen2 documentation and tidyverse practices
##

# Load required libraries ----
library(tidyverse)
library(Rnumerai)

# Configuration ----
data_dir <- getwd()
api_src_dir <- "~/Dropbox/Sennosen/apps/src/api/"
numerai_src_dir <- "~/Dropbox/Sennosen/apps/src/numerai/"

# Source required files
source(file.path(numerai_src_dir, "graphql_utils.R"))

# Set API credentials
Sys.getenv("NUMERAI_DATASERIES_PUBLIC_KEY") %>% set_public_id()
Sys.getenv("NUMERAI_DATASERIES_API_KEY") %>% set_api_key()

# Model configuration ----
staked_models <- c(
  "v1cdf", "v1bt", "v1bvlg", "v1vlg", "v1cmdt", "glb", "v1levs", 
  "v1liqs", "v1liq", "v1mom", "v1bm", "v1i", "v1cg", "v1f", 
  "v1h", "v1s", "v1t", "v1u", "v1evlg", "v1cdfn", "v1lr"
)

# Legacy compatibility - create alias for old variable name
stakedmodels <- staked_models

# Default structure for handling NULL values (keeping original structure) ----
default_NA_df <- data.frame(
  date = NA,
  day = NA,
  displayName = NA,
  payoutPending = NA,
  payoutSettled = NA,
  percentile = NA,
  value = NA  
)

#' Replace NULL values with NA data frame
#'
#' @description
#' Utility function to handle NULL values in API responses by replacing them
#' with a default NA data frame structure.
#'
#' @param a_x Input object to check for NULL
#'
#' @return Either the original object if not NULL, or default_NA_df if NULL
#'
#' @author Craig Ilebrumfield
#' @family utility-functions
#'
#' @examples
#' replace_null_with_na(NULL)  # Returns default_NA_df
#' replace_null_with_na(data.frame(x = 1))  # Returns the data frame
replace_null_with_na <- function(a_x) {
  if (a_x %>% is.null()) { 
    return(.GlobalEnv$default_NA_df) 
  } else { 
    return(a_x)
  }
}

#' Get model ID from model name
#'
#' @description
#' Queries the Numerai API to retrieve the model ID for a given model name
#' using the GraphQL endpoint.
#'
#' @param model_name Character string of the model name to look up
#'
#' @return Character string containing the model ID
#'
#' @author Craig Ilebrumfield
#' @family api-functions
#'
#' @examples
#' \dontrun{
#' model_id <- get_model_id("v1cdf")
#' }
#'
#' @export
get_model_id <- function(model_name) {
  # Get model ID for a given model name
  model_id <- midquery %>%
    gsub("mname", model_name, .) %>%
    run_query()
  
  return(model_id$data$v2SignalsProfile$id)
}

#' Extract round numbers from API response
#'
#' @description
#' Extracts unique round numbers from the v2RoundModelPerformances data
#' returned by the Numerai API.
#'
#' @param data List object containing API response data
#'
#' @return Numeric vector of unique round numbers
#'
#' @author Craig Ilebrumfield
#' @family api-functions
#'
#' @examples
#' \dontrun{
#' rounds <- get_round_numbers(api_response)
#' }
#'
#' @export
get_round_numbers <- function(data) {
  # Extract unique round numbers from data
  ldf <- data$data$v2RoundModelPerformances %>% 
    map_df(., ~ replace(.x, is.null(.x), NA))
  return(ldf$roundNumber %>% unique())
}

#' Process and flatten Numerai API query data
#'
#' @description
#' Processes the complex nested data structure returned by the Numerai API,
#' flattens the intraRoundSubmissionScores lists, and filters for specific
#' display names (mmc, fnc_v4). This function maintains the exact original
#' working logic to ensure compatibility with the API data structure.
#'
#' @param data List object containing the raw API response from run_query()
#'
#' @return Data frame with flattened performance data filtered for mmc and fnc_v4
#'
#' @author Craig Ilebrumfield
#' @family data-processing
#'
#' @details
#' The function performs these steps:
#' 1. Converts v2RoundModelPerformances to data frame using data.table::rbindlist
#' 2. Flattens nested intraRoundSubmissionScores using dplyr::bind_rows
#' 3. Adds safety row to handle edge cases
#' 4. Combines main data with submission scores using cbind
#' 5. Filters for displayName in c("mmc", "fnc_v4")
#' 6. Removes redundant intraRoundSubmissionScores column
#'
#' @examples
#' \dontrun{
#' api_data <- defaultquery %>%
#'   gsub("mid", "model123", .) %>%
#'   run_query()
#' 
#' processed_data <- process_dataquery(api_data)
#' }
#'
#' @export
process_dataquery <- function(data) {
  # Flattening lists and filter repetitive data - EXACT original approach
  
  ldf <- data$data$v2RoundModelPerformances %>% 
    data.table::rbindlist()
  
  lcol_detail <- ldf$intraRoundSubmissionScores %>%
    dplyr::bind_rows() %>%
    rbind(
      data.frame(
        date = I(list(NULL)),
        day = I(list(NULL)),
        displayName = I(list(NULL)),
        payoutPending = I(list(NULL)),
        payoutSettled = I(list(NULL)),
        percentile = I(list(NULL)),
        value = I(list(NULL))
      ),
      .
    )
  
  ldf <- cbind(ldf, lcol_detail) %>%
    filter(., displayName == c("mmc", "fnc_v4")) %>%
    select(., -starts_with("intraRoundSubmissionScores"))
  
  return(ldf)
}

#' Safely process a single staked model
#'
#' @description
#' Processes a single model by querying the API, processing the data,
#' converting list columns to character format, and saving to CSV.
#' Includes comprehensive error handling and progress reporting.
#'
#' @param model_name Character string of the model name to process
#'
#' @return Logical value: TRUE if processing succeeded, FALSE if failed
#'
#' @author Craig Ilebrumfield
#' @family main-functions
#'
#' @details
#' The function performs these steps:
#' 1. Creates output directory if it doesn't exist
#' 2. Gets model ID and queries API using defaultquery
#' 3. Processes data using process_dataquery()
#' 4. Converts list columns to character strings for CSV compatibility
#' 5. Writes result to ./staked/{model_name}.csv
#' 6. Reports success/failure with descriptive messages
#'
#' @examples
#' \dontrun{
#' success <- safely_process_model("v1cdf")
#' if (success) {
#'   cat("Model v1cdf processed successfully")
#' }
#' }
#'
#' @seealso \code{\link{process_dataquery}}, \code{\link{get_model_id}}
#'
#' @export
safely_process_model <- function(model_name) {
  cat("Processing model:", model_name, "\n")
  
  tryCatch({
    # Create output directory if it doesn't exist
    output_dir <- "./staked"
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }
    
    # Process using exact original approach
    result <- defaultquery %>%
      gsub("mid", get_model_id(model_name), .) %>%
      run_query() %>%
      process_dataquery() %>%
      # Convert all list columns to character (original approach)
      dplyr::mutate(across(where(is.list), ~sapply(., toString)))
    
    # Write to CSV
    output_file <- file.path(output_dir, paste0(model_name, ".csv"))
    write.csv(result, output_file, row.names = FALSE)
    
    cat("✓ Successfully processed:", model_name, "\n")
    return(TRUE)
    
  }, error = function(e) {
    cat("✗ Error processing", model_name, ":", conditionMessage(e), "\n")
    return(FALSE)
  })
}

#' Process all staked models and generate summary report
#'
#' @description
#' Processes all models defined in the staked_models vector, providing
#' progress tracking and a comprehensive summary report of successes and failures.
#'
#' @return Named logical vector indicating success (TRUE) or failure (FALSE) for each model
#'
#' @author Craig Ilebrumfield
#' @family main-functions
#'
#' @details
#' The function:
#' 1. Processes each model in staked_models using safely_process_model()
#' 2. Tracks successes and failures
#' 3. Generates a formatted summary report with:
#'    - Total number of models processed
#'    - Count of successful and failed models  
#'    - List of failed model names (if any)
#' 4. Returns results vector for programmatic use
#'
#' All CSV files are saved to ./staked/ directory with format: {model_name}.csv
#'
#' @examples
#' \dontrun{
#' # Process all models and get results
#' results <- process_all_staked_models()
#' 
#' # Check which models failed
#' failed_models <- names(results)[!results]
#' 
#' # Count successes
#' success_count <- sum(results)
#' }
#'
#' @seealso \code{\link{safely_process_model}}
#'
#' @export
process_all_staked_models <- function() {
  cat("Starting processing of", length(staked_models), "staked models...\n\n")
  
  # Process all models with error tracking
  results <- map_lgl(staked_models, safely_process_model)
  names(results) <- staked_models
  
  # Summary report
  successful_count <- sum(results)
  failed_count <- length(results) - successful_count
  
  cat("\n", rep("=", 50), "\n", sep = "")
  cat("PROCESSING SUMMARY\n")
  cat(rep("=", 50), "\n", sep = "")
  cat("Total models:", length(staked_models), "\n")
  cat("Successful:", successful_count, "\n")
  cat("Failed:", failed_count, "\n")
  
  if (failed_count > 0) {
    failed_models <- staked_models[!results]
    cat("Failed models:", paste(failed_models, collapse = ", "), "\n")
  }
  
  cat(rep("=", 50), "\n", sep = "")
  
  return(results)
}

# Legacy compatibility - original loop approach ----
#' Process models using original for-loop approach
#'
#' @description
#' Alternative processing method using the original for-loop structure.
#' Uncomment this section if you prefer the original sequential approach
#' without the enhanced error handling and reporting.
#'
#' @details
#' This commented code block provides the exact original processing logic:
#' \preformatted{
#' for(mname in staked_models) {
#'   defaultquery %>%
#'     gsub("mid", mname %>% get_model_id(), .) %>%
#'     run_query() %>%
#'     process_dataquery() %>%
#'     dplyr::mutate(across(where(is.list), ~sapply(., toString))) %>%
#'     write.csv(paste("./staked/", mname, ".csv", sep=""), row.names = FALSE)
#' }
#' }
#'
#' @export
# for(mname in staked_models) {
#   defaultquery %>%
#     gsub("mid", mname %>% get_model_id(), .) %>%
#     run_query() %>%
#     process_dataquery() %>%
#     # Convert all list columns to character
#     dplyr::mutate(across(where(is.list), ~sapply(., toString))) %>%
#     write.csv(paste("./staked/", mname, ".csv", sep=""), row.names = FALSE)
# }

# Execute main processing ----
if (interactive() || !exists("SKIP_EXECUTION")) {
  #' Main execution block
  #' 
  #' @description
  #' Automatically executes the main processing function when the script
  #' is run interactively or sourced (unless SKIP_EXECUTION is defined).
  #' 
  #' @details
  #' Sets processing_results in global environment for later inspection.
  #' To skip automatic execution, define SKIP_EXECUTION before sourcing:
  #' \preformatted{
  #' SKIP_EXECUTION <- TRUE
  #' source("staked_models_perfseries.R")
  #' }
  
  # Run the main processing function
  processing_results <- process_all_staked_models()
}
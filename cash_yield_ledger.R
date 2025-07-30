#' Cash Yield Ledger Analysis
#'
#' @title Automated Cash Yield Ledger Performance Calculator
#' @description Automated version of 'Cash_Yield_Ledger.xls' to calculate 
#'   supplemental performance metrics for Credit Defensive strategy. This script
#'   processes Numerai staking data and calculates daily returns, payouts, and
#'   performance metrics across multiple models.
#'
#' @author Craigile J. Brumfield, CFA
#' @date Created: 2022-04-11, Last edit: 2025-07-29
#' @note Total Runtime: ~5 minutes
#'
#' @section Dependencies:
#' - glue: String interpolation
#' - tidyquant: Financial data analysis  
#' - tidyverse: Data manipulation and visualization
#' - timeperiodsR: Time period utilities
#' - vctrs: Vector tools
#'
#' @examples
#' \dontrun{
#' # Run the entire cash yield ledger analysis
#' source("cash_yield_ledger.R")
#' }

# Clear environment and set options
rm(list = ls(all.names = TRUE))
options(scipen = 999, stringsAsFactors = FALSE)

# Load required libraries
library(glue)
library(tidyquant)
library(tidyverse)
library(timeperiodsR)
library(vctrs)

# Set working directory and source dependencies
setwd("~/Dropbox/Sennosen/apps/analytics/perf+numerai/")
source("~/Dropbox/Sennosen/apps/src/calendar/calendar_settings.R")
source("~/Dropbox/Sennosen/apps/src/numerai/performance_utils.R")

#' Check if dataframe is empty
#'
#' @description Utility function to check if a dataframe contains no rows
#'   using tidyverse approach
#'
#' @param a_df A dataframe to check
#' @return Logical value indicating if dataframe is empty (TRUE) or not (FALSE)
#' @export
#'
#' @examples
#' \dontrun{
#' empty_df <- data.frame()
#' is_object_empty(empty_df)  # Returns TRUE
#' }
is_object_empty <- function(a_df) {
  # Check if dataframe is empty using dplyr approach
  a_df %>%
    nrow() %>%
    `==`(0)
}

#' Check if series contains only NA values
#'
#' @description Utility function to check if all values in a series are NA
#'   using tidyverse pipe operations
#'
#' @param a_series A vector or series to check for NA values
#' @return Logical value indicating if all values are NA (TRUE) or not (FALSE)
#' @export
#'
#' @examples
#' \dontrun{
#' na_series <- c(NA, NA, NA)
#' is_empty_series(na_series)  # Returns TRUE
#' }
is_empty_series <- function(a_series) {
  # Check if all values in series are NA using tidyverse pipe
  a_series %>% 
    is.na() %>% 
    all()
}

#' Asset Configuration
#'
#' @description Define asset configuration tibble with symbols, categories,
#'   active status, and priority levels for trading analysis
#'
#' @format A tibble with 7 rows and 4 variables:
#' \describe{
#'   \item{symbol}{Character. Asset symbol (e.g., "GLD", "ETH-USD")}
#'   \item{category}{Character. Asset category ("commodity", "crypto", "etf")}
#'   \item{active}{Logical. Whether asset is currently active for analysis}
#'   \item{priority}{Numeric. Priority level for ordering (1 = highest)}
#' }
asset_config <- tibble(
  symbol = c(
    "GLD",
    "ETH-USD", 
    "BTC-USD",
    "SOL-USD",
    "MATIC-USD",
    "NMR-USD",
    "SPXZ"
  ),
  category = c(
    "commodity",
    "crypto",
    "crypto", 
    "crypto",
    "crypto",
    "crypto",
    "etf"
  ),
  active = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
  priority = c(1, 1, 1, 2, 2, 1, 3)
) %>%
  filter(active == TRUE) %>%  # Easy to disable assets
  arrange(priority, symbol)   # Control ordering

#' Asset symbols vector
#'
#' @description Vector of active asset symbols extracted from asset_config
coin_mnemonics <- asset_config$symbol

#' Configuration parameters
#'
#' @description List containing configuration parameters for data paths,
#'   file names, and date ranges used throughout the analysis
#'
#' @format A list with 4 elements:
#' \describe{
#'   \item{persistdir}{Character. Directory for persistent data storage}
#'   \item{datadir}{Character. Directory for input data files}
#'   \item{filenames}{List. Named list of CSV filenames for different data types}
#'   \item{date_range}{List. Start and end dates for analysis period}
#' }
config <- list(
  persistdir = "~/Desktop/TRADING/",
  datadir = "./data/",
  filenames = list(
    investments = "investments.csv",
    fixedincome = "fixedincome.csv", 
    cashbalance = "cashbalance.csv"
  ),
  date_range = list(
    start = as.Date("2020-12-07"),
    end = as.Date("2025-07-25")
  )
)

#' Get asset prices
#'
#' @description Retrieve asset prices for specified symbols and date range
#' using the get_asset_prices function from performance_utils
assetprices <- coin_mnemonics %>% 
  get_asset_prices(config$date_range$start, config$date_range$end)

#' Remove empty price series
#'
#' @description Remove any asset price columns that contain only NA values
#' using tidyverse approach, while ensuring date column is preserved
assetprices <- assetprices %>%
  select(where(~ !is_empty_series(.))) %>%
  # Ensure date column is always kept even if it somehow gets filtered
  select(date, everything())

#' Generate asset returns
#'  
#' @description Calculate asset returns from price data and prepare for analysis
#' by converting dates to character format and replacing NA values with 0
assetrets <- assetprices %>%
  select(-date) %>%
  names() %>%
  get_asset_rets(assetprices, config$date_range$start) %>%
  mutate(
    date = as.character(date),
    across(everything(), ~ replace_na(., 0))
  ) %>%
  arrange(date)

#' Initialize asset values dataframe
#'
#' @description Create initial dataframe structure for asset values with 
#' date column and zero-initialized columns for each asset
asset_columns <- coin_mnemonics %>% str_remove("-USD")

assetvalues <- tibble(
  date = assetprices$date,
  !!!set_names(rep(list(0), length(asset_columns)), asset_columns)
)

#' Read and combine investment data
#'
#' @description Function to safely read and combine investment and fixed income
#'   CSV files with proper error handling and NA replacement
#'
#' @param datadir Character. Directory path containing the data files
#' @param filenames List. Named list containing filenames for different data types
#' @return A tibble containing combined investment and fixed income data
#'
#' @examples
#' \dontrun{
#' combined_data <- read_investment_data("./data/", config$filenames)
#' }
read_investment_data <- function(datadir, filenames) {
  # Helper function to safely read CSV files
  safe_read_csv <- function(filepath) {
    if (file.exists(filepath)) {
      read_csv(filepath, show_col_types = FALSE)
    } else {
      warning(glue("File not found: {filepath}"))
      tibble()
    }
  }
  
  # Read and combine investment files
  investments_path <- file.path(datadir, filenames$investments)
  fixedincome_path <- file.path(datadir, filenames$fixedincome)
  
  investments_data <- safe_read_csv(investments_path)
  fixedincome_data <- safe_read_csv(fixedincome_path)
  
  # Combine with full_join and handle NAs
  combined_data <- investments_data %>%
    full_join(fixedincome_data, by = "date") %>%
    mutate(across(everything(), ~ replace_na(., 0)))
  
  return(combined_data)
}

#' Read investment blotter data
#'
#' @description Load and combine investment data using the read_investment_data function
investmentblotter <- read_investment_data(config$datadir, config$filenames)

#' Read cash balance data
#'
#' @description Load cash balance data from CSV file and ensure date format consistency
cashbalance <- file.path(config$datadir, config$filenames$cashbalance) %>%
  read_csv(show_col_types = FALSE) %>%
  mutate(date = as.character(date))  # Ensure date is character for joining

#' Create asset names vector
#'
#' @description Extract asset names from assetvalues dataframe (excluding date column)
assetnames <- assetvalues %>%
  select(-date) %>%
  names()

#' Build working dataframe
#'
#' @description Combine asset values, investment blotter, and cash balance data
#' using left joins with proper date format handling and NA replacement
workingdf <- assetvalues %>%
  mutate(date = as.character(date)) %>%
  left_join(
    investmentblotter %>% mutate(date = as.character(date)), 
    by = "date"
  ) %>%
  left_join(
    cashbalance %>% mutate(date = as.character(date)), 
    by = "date"
  ) %>%
  mutate(across(everything(), ~ replace_na(., 0))) %>%
  arrange(date)

#' Generate asset balances
#'
#' @description Calculate asset balances using cleaned asset names that match
#' the column names in workingdf by removing "-USD" suffix
asset_names_for_balances <- assetprices %>% 
  select(-date) %>% 
  names() %>%
  str_remove("-USD")  # Remove -USD suffix to match workingdf column names

workingdf <- workingdf %>%
  generate_asset_balances(asset_names_for_balances, assetrets)

#' Calculate cash balance
#'
#' @description Calculate cash balance using improved column selection for
#' cash balance and investment columns
cash_balance_columns <- c("date", "Principal", "Interest", "Adjustments")
investment_columns <- workingdf %>%
  select(contains("Inv")) %>%
  names()

workingdf <- workingdf %>%
  mutate(
    cashbalance = select(., all_of(c(cash_balance_columns, investment_columns))) %>%
      generate_cash_balance()
  )

#' Create NMR ledger
#'
#' @description Extract NMR-related columns from working dataframe for analysis
NMRledger <- workingdf %>%
  select(date, contains("NMR"))

#' Persist data with error handling
#'
#' @description Function to save data to RDS files with proper error handling
#'   and informative messages
#'
#' @param data Object to be saved
#' @param filepath Character. Full path where data should be saved
#' @param description Character. Description of the data being saved for messages
#'
#' @examples
#' \dontrun{
#' persist_data(my_data, "path/to/file.rds", "my dataset")
#' }
persist_data <- function(data, filepath, description = "") {
  tryCatch({
    saveRDS(data, filepath)
    message(glue("Successfully saved {description} to {filepath}"))
  }, error = function(e) {
    warning(glue("Failed to save {description} to {filepath}: {e$message}"))
  })
}

#' Save processed data
#'
#' @description Save investment blotter and cash balance data to RDS files
investmentblotter %>%
  persist_data(file.path(config$datadir, "investmentBlotter.rds"), "investment blotter")

workingdf %>%
  select(date, cashbalance) %>%
  persist_data(file.path(config$datadir, "cashBalance.rds"), "cash balance")

# Change to sandbox directory for Numerai data processing
setwd("../../numerai/sandbox/")   #Note: parameterize when more time

#' Source staked models performance series
#'
#' @description Load staked models data from numerai (execute time = 115.6 secs)
#' @note This sources external script that defines stakedmodels vector
source('staked_models_perfseries.R')   ##staked models from numerai

#' Initialize staked model time series list
#'
#' @description Create list to store time series data for each staked model
stakedmodel_ts <- list()  #timeseries for models

#' Load staked model data
#'
#' @description Read CSV files for each staked model and combine into named list
for (mnm in stakedmodels) {
  newlist <-
    list(paste("./staked/", mnm, ".csv", sep = "") %>% read.csv())
  names(newlist) <- mnm
  stakedmodel_ts <- newlist %>% append(stakedmodel_ts, .)
}

#' Process staked model time series data
#'
#' @description Improved processing using tidyverse and dplyr to calculate
#'   daily payouts, returns, and performance metrics for each staked model.
#'   This replaces the original nested loop approach with more efficient
#'   grouped operations.
#'
#' @details 
#' For each model in the list, this processing:
#' - Cleans column names by removing "intraRoundSubmissionScores." prefix
#' - Converts day, payoutPending, and atRisk to numeric formats
#' - Calculates cumulative returns (payoutPending / atRisk)
#' - Groups by roundNumber to calculate daily metrics:
#'   - dailypayout: Daily change in payout (first day = total payout)
#'   - stakeandpayout: Sum of stake and payout amounts
#'   - dailyrets: Daily change in returns (first day = 0)
#' - Handles NA values appropriately with defaults
stakedmodel_ts <- stakedmodel_ts %>%
  map(~ {
    .x %>%
      rename_with(~ str_remove(.x, "intraRoundSubmissionScores\\.")) %>%
      mutate(
        day = map_dbl(day, ~ as.numeric(.x)),
        across(c(payoutPending, atRisk), as.numeric)
      ) %>%
      arrange(roundNumber, day) %>%
      mutate(returns = payoutPending / atRisk) %>%
      group_by(roundNumber) %>%
      arrange(day, .by_group = TRUE) %>%
      mutate(
        dailypayout = if_else(row_number() == 1, 
                              payoutPending, 
                              payoutPending - lag(payoutPending, default = 0)),
        stakeandpayout = atRisk + payoutPending,
        dailyrets = if_else(row_number() == 1, 
                            0, 
                            returns - lag(returns, default = 0))
      ) %>%
      ungroup() %>%
      replace_na(list(dailypayout = 0, dailyrets = 0)) %>%
      # Final cleanup for first day values
      group_by(roundNumber) %>%
      mutate(
        dailypayout = if_else(row_number() == 1 & is.na(dailypayout), 
                              payoutPending, 
                              dailypayout)
      ) %>%
      ungroup()
  })

#' Aggregate staked model data by date
#'
#' @description Transform staked model time series data to daily aggregates
#'   with proper date handling, weekend adjustments, and weighted return calculations
#'
#' @details
#' For each model, this creates daily summaries including:
#' - totalpayout: Sum of daily payouts
#' - cumstake: Sum of at-risk amounts  
#' - walletestimate: Estimate of available NMR for staking
#' - NMRwtret: Weighted returns based on stake and payout amounts
#' - Derived metrics: daily changes and return calculations
stakedmodel_by_date <- stakedmodel_ts %>%
  map(~ {
    # Data preparation
    .x %>%
      arrange(date) %>%
      filter(!is.na(date)) %>%
      mutate(date = str_sub(date, 1, 10)) %>%
      # Main aggregation by date
      group_by(date) %>%
      summarise(
        totalpayout = sum(dailypayout, na.rm = TRUE),
        cumstake = sum(atRisk, na.rm = TRUE),
        walletestimate = first(atRisk[day == min(day)]),  # assume avail. NMR staked in most recent round
        NMRwtret = {
          temp_data <- cur_data()
          if(sum(temp_data$stakeandpayout, na.rm = TRUE) > 0) {
            temp_data %>%
              mutate(wts = stakeandpayout / sum(stakeandpayout, na.rm = TRUE)) %>%
              mutate(wtret = wts * dailyrets) %>%
              pull(wtret) %>%
              sum(na.rm = TRUE)
          } else {
            0
          }
        },
        .groups = "drop"
      ) %>%
      # Calculate lagged and derived metrics
      arrange(date) %>%
      mutate(
        totalpayoutdailychg = totalpayout - lag(totalpayout),
        return = totalpayoutdailychg / cumstake,
        leveredreturn = totalpayoutdailychg / walletestimate
      )
  })

#' Adjust dates for business days and weekends
#'
#' @description Apply business day adjustments to handle weekend dates and
#'   lag trading days by one business day using calendar functions
#'
#' @details
#' This processing:
#' - Identifies weekend dates that need to be shifted to next business day
#' - Lags all trading days by 1 business day
#' - Handles missing dates with fallback logic
#' - Removes duplicate dates, keeping the last occurrence
stakedmodel_by_date <- stakedmodel_by_date %>%
  map( ~ {
    df <- .x
    
    # Identify weekends that need to be shifted - create xts object properly
    daystoshift <- df %>%
      select(date) %>%
      mutate(date = as.Date(date)) %>%
      xts(order.by = as.Date(df$date)) %>%
      index() %>%
      weekends()
    
    df %>%
      mutate(
        # Shift weekend dates to next business day
        date = case_when(
          as.Date(date) %in% daystoshift ~ add.bizdays(as.Date(date), 1, cal) %>% as.character(),
          TRUE ~ date
        ),
        # Lag all trading days by 1 business day
        date = add.bizdays(as.Date(date),-1, cal) %>% as.character(),
        # Handle missing dates by using original row names approach
        date = if_else(is.na(date),
                       (as.Date(df$date[row_number()]) - 1) %>% as.character(),
                       date)
      ) %>%
      # Remove duplicates, keeping the last occurrence 
      group_by(date) %>%
      slice_tail(n = 1) %>%
      ungroup() %>%
      # Set row names 
      column_to_rownames("date") %>%
      rownames_to_column("date")
  })

#' Aggregate all models performance
#'
#' @description Combine performance data across all staked models to create
#'   portfolio-level metrics and add to the stakedmodel_by_date list
#'
#' @details
#' Creates "allmodels" summary by:
#' - Summing totalpayout, cumstake, walletestimate across all models by date
#' - Calculating portfolio-level return and levered return metrics
#' - Adding the aggregated data back to the main list as "allmodels" element
allmodels <- stakedmodel_by_date %>%
  map_dfr(~ .x %>% 
            select(date, totalpayout, cumstake, walletestimate, totalpayoutdailychg),
          .id = "model_name") %>%
  group_by(date) %>%
  summarise(
    totalpayout = sum(totalpayout, na.rm = TRUE),
    cumstake = sum(cumstake, na.rm = TRUE), 
    walletestimate = sum(walletestimate, na.rm = TRUE),
    totalpayoutdailychg = sum(totalpayoutdailychg, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    return = totalpayoutdailychg / cumstake,
    leveredreturn = totalpayoutdailychg / walletestimate
  )

# Add allmodels back to the stakedmodel_by_date list
stakedmodel_by_date[["allmodels"]] <- allmodels

#' Calculate NMR returns
#'
#' @description Extract NMR price data for dates matching the allmodels data
#'   and calculate daily NMR returns for currency-adjusted performance metrics
#'
#' @details
#' - Filters asset prices to match allmodels dates
#' - Calculates daily NMR returns using lag function
#' - Replaces NA values with 0 for missing return data
#' - Ensures date format consistency for joining
nmrprices <- assetprices %>%
  filter(as.character(date) %in% allmodels$date) %>%
  select(date, NMR) %>%
  arrange(date) %>%
  mutate(
    NMRreturn = NMR / lag(NMR) - 1,
    NMRreturn = coalesce(NMRreturn, 0),  # Replace NA with 0
    date = as.character(date)
  )

#' Add currency-adjusted returns to allmodels
#'
#' @description Join NMR price data with allmodels and calculate currency-adjusted
#'   return metrics that account for NMR price movements
#'
#' @details
#' Adds two new metrics to allmodels:
#' - return_currency: Base return plus NMR return
#' - leveredreturn_currency: Levered return plus NMR return
stakedmodel_by_date$allmodels <- stakedmodel_by_date$allmodels %>%
  left_join(nmrprices, by = "date") %>%
  mutate(
    return_currency = return + NMRreturn,
    leveredreturn_currency = leveredreturn + NMRreturn
  )

#' Calculate cumulative staked model performance by date
#'
#' @description Generate cumulative performance metrics for each staked model
#'   with improved processing using tidyverse and dplyr, including special
#'   handling for the "allmodels" aggregate and proper NMR pricing integration
#'
#' @details
#' This complex transformation:
#' - Handles "allmodels" with simplified processing (no time series data needed)
#' - For individual models, processes corresponding time series data if available
#' - Applies weekend date shifts and business day adjustments
#' - Calculates cumulative balances in both NMR and USD terms
#' - Joins with NMR pricing data for USD conversions
#' - Handles missing data gracefully with appropriate defaults
cumstakedmodel_by_date <- stakedmodel_by_date %>%
  imap(~ {
    # Get the corresponding time series data using the list name
    mname <- .y  # .y contains the name from the named list
    
    # Special handling for "allmodels" - simplified processing
    if (mname == "allmodels") {
      return(.x %>%
               # Only keep the essential columns to avoid duplicates
               select(date, cumstake, walletestimate) %>%
               mutate(
                 cumbalance = 0,
                 cumstakebalance = 0
               ) %>%
               # Add NMR pricing for allmodels  
               left_join(nmrprices %>% select(date, NMR), by = "date") %>%
               mutate(
                 NMR = if("NMR" %in% names(.)) coalesce(NMR, 1) else 1,
                 cumUSD = cumbalance * NMR,
                 cumstakeUSD = cumstakebalance * NMR
               ))
    }
    
    df <- stakedmodel_ts[[mname]]
    
    # Check if the corresponding time series data exists
    if (is.null(df)) {
      # If no time series data exists, return the original data with minimal processing
      return(.x %>%
               arrange(date) %>%
               select(-return, -leveredreturn) %>%
               mutate(
                 totalpayout = 0,
                 totalpayoutdailychg = 0,
                 cumbalance = walletestimate,
                 cumstakebalance = cumstake
               ) %>%
               mutate(across(where(is.numeric), ~ coalesce(.x, 0))) %>%
               left_join(
                 nmrprices %>% select(date, NMR), 
                 by = "date"
               ) %>%
               mutate(
                 # Ensure NMR column exists before using it
                 NMR = if("NMR" %in% names(.)) coalesce(NMR, 1) else 1,
                 cumUSD = cumbalance * NMR,
                 cumstakeUSD = cumstakebalance * NMR
               ))
    }
    
    # Clean and prepare the time series data
    df_processed <- df %>%
      filter(!is.na(date)) %>%
      arrange(date) %>%
      mutate(date = str_sub(date, 1, 10)) %>%
      # Handle weekend shifts
      mutate(
        date_obj = as.Date(date),
        is_weekend = date_obj %in% (df %>% 
                                      filter(vec_duplicate_id(date) %in% unique(vec_duplicate_id(date))) %>%
                                      pull(date) %>% 
                                      as.Date() %>%
                                      .[weekdays(.) %in% c("Saturday", "Sunday")])
      ) %>%
      mutate(
        date = if_else(is_weekend, 
                       as.character(add.bizdays(date_obj, 1, cal)), 
                       date)
      ) %>%
      # Lag trading days by 1 business day
      mutate(date = as.character(add.bizdays(as.Date(date), -1, cal))) %>%
      # Remove duplicates, keeping the last occurrence
      group_by(date) %>%
      slice_tail(n = 1) %>%
      ungroup() %>%
      # Handle any remaining NA dates
      mutate(date = coalesce(
        date, 
        as.character(max(as.Date(date), na.rm = TRUE) - 1)
      )) %>%
      select(-date_obj, -is_weekend)
    
    # Calculate daily aggregates with different names to avoid conflicts
    daily_aggregates <- df_processed %>%
      group_by(date) %>%
      summarise(
        ts_totalpayout = sum(payoutPending, na.rm = TRUE),
        ts_totalpayoutdailychg = sum(dailypayout, na.rm = TRUE),
        .groups = "drop"
      )
    
    # Process the main dataframe
    .x %>%
      arrange(date) %>%
      select(-return, -leveredreturn) %>%
      left_join(daily_aggregates, by = "date") %>%
      # Use the time series data if available, otherwise use existing values
      mutate(
        totalpayout = if("ts_totalpayout" %in% names(.)) coalesce(ts_totalpayout, totalpayout, 0) else coalesce(totalpayout, 0),
        totalpayoutdailychg = if("ts_totalpayoutdailychg" %in% names(.)) coalesce(ts_totalpayoutdailychg, totalpayoutdailychg, 0) else coalesce(totalpayoutdailychg, 0),
        cumbalance = totalpayout + walletestimate,
        cumstakebalance = totalpayout + cumstake
      ) %>%
      # Remove the temporary columns
      select(-starts_with("ts_")) %>%
      # Only apply coalesce to numeric columns, exclude date and other character columns
      mutate(across(where(is.numeric), ~ coalesce(.x, 0))) %>%
      left_join(
        nmrprices %>% select(date, NMR), 
        by = "date"
      ) %>%
      mutate(
        # Ensure NMR column exists before using it
        NMR = if("NMR" %in% names(.)) coalesce(NMR, 1) else 1,
        cumUSD = cumbalance * NMR,
        cumstakeUSD = cumstakebalance * NMR
      )
  })

#' Final processing and output
#'
#' @description Complete the cash yield ledger analysis with all calculated
#'   metrics including cumulative performance, currency-adjusted returns,
#'   and business-day adjusted dates
#'
#' @section Output Variables:
#' - stakedmodel_ts: Time series data for individual models with daily metrics
#' - stakedmodel_by_date: Daily aggregated data by model with date adjustments  
#' - cumstakedmodel_by_date: Cumulative performance metrics in NMR and USD
#' - allmodels: Portfolio-level aggregated performance across all models
#' - nmrprices: NMR price and return data for currency adjustments
#'
#' @section Key Metrics:
#' - dailypayout: Daily change in payout amounts
#' - dailyrets: Daily change in return percentages
#' - return_currency: Returns adjusted for NMR price movements
#' - cumbalance: Cumulative balance including payouts and wallet estimates
#' - cumUSD: Cumulative balance converted to USD using NMR prices
#'
#' @note This script produces comprehensive performance analytics for Numerai
#'   staking strategies, replacing manual Excel calculations with automated
#'   R processing using modern tidyverse approaches.
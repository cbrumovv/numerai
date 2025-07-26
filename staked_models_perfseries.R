##
## Date:  4 sept 2022
## Last edit:  27 may 2024
##
## query payout information on staked models and write to spreadsheets
##

library( tidyverse )
library( Rnumerai )

data_dir <- getwd()
apiSrcDir <- "~/Dropbox/Sennosen/apps/src/api/"  #source code repository
numeraiSrcDir <- "~/Dropbox/Sennosen/apps/src/numerai/"  #source code repository
# source( paste0(apiSrcDir,"keys.R") ) #api keys
source( paste0(numeraiSrcDir,"graphql_utils.R") )  #query function

Sys.getenv( "NUMERAI_DATASERIES_PUBLIC_KEY" ) %>% set_public_id()
Sys.getenv( "NUMERAI_DATASERIES_API_KEY" ) %>% set_api_key()


stakedmodels <- c("v1cdf",
                  "v1bt",
                  "v1bvlg",
                  "v1vlg",
                  "v1cmdt",
                  "glb",
                  "v1levs",
                  "v1liqs",
                  "v1liq",
                  "v1mom",
                  "v1bm",
                  "v1i",
                  "v1cg",
                  "v1f",
                  "v1h",
                  "v1s",
                  "v1t",
                  "v1u",
                  "v1evlg",
                  "v1cdfn",
                  "v1lr"
                )


default_NA_df <- data.frame(
  date=NA,
  day=NA,
  displayName=NA,
  payoutPending=NA,
  payoutSettled=NA,
  percentile=NA,
  value=NA  
)
                
replace_null_with_na <- function( a_x){
  if( a_x %>% is.null() ){ 
    return ( .GlobalEnv$default_NA_df ) } 
  else { 
    return( a_x )
    }
}

replace_null_with_list_null <- function(x) {
  ifelse(is.null(x), list(NULL), x)
}

replace_nulls_in_list <- function( a_list ){ #replace NULL's in a_list with 'default_NA_DF'
  lres <- a_list %>% sapply( ., replace_null_with_na )
  return( lres )
}


# replace_nulls_in_list <- function( a_list ){ #replace NULL's in a_list with NA's
#   lres <- a_list %>% sapply( ., replace_null_with_na )
#   return( lres )
# }

get_model_id <- function( a_model_name ){ #get model id given 'a_model_name'
  lid <- midquery %>% 
          gsub( "mname", a_model_name, . ) %>% 
          run_query()
  
  return( lid$data$v2SignalsProfile$id )
}

get_round_numbers <- function ( a_data ){
  #get vector of round numbers from 'a_data
  ldf <- a_data$data$v2RoundModelPerformances %>% map_df( ., ~ replace(.x, is.null(.x),NA) )
  return ( ldf$roundNumber %>% unique() )
  }

process_dataquery <- function(a_data) {
  #flattening lists and filter repetitive data
  
  ldf <-
    a_data$data$v2RoundModelPerformances %>% data.table::rbindlist()
  
  lcol_detail <-
    ldf$intraRoundSubmissionScores %>%
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
    filter(., displayName == c("mmc","fnc_v4")) %>%
    select(., -starts_with("intraRoundSubmissionScores"))
  
  
  #Note:  query fails -- (1) on null replacements; and (2) change to unnest function
  #27 may 2024
  #
  # ldf <- a_data$data$v2RoundModelPerformances %>%
  #         map_df( ., ~ replace(.x, is.null(.x),NA) ) %>%
  #         mutate( intraRoundSubmissionScores=intraRoundSubmissionScores %>% map_depth(.,1,~ifelse(is.null(.x),NA,.x), .ragged=T) ) %>%
  #         mutate( intraRoundSubmissionScores=intraRoundSubmissionScores %>% map_depth(.,2,~ifelse(is.null(.x),NA,.x)) )
  #         mutate( intraRoundSubmissionScores=map(intraRoundSubmissionScores,~as_tibble(t(.x))) )
  #         
  
          # unnest( cols = c(intraRoundSubmissionScores) )

  #         filter( .,displayName == "tc" ) %>%
  # mutate_if( is_list,replace_nulls_in_list )
  
  return(ldf)
}

# process_dataquery <- function( a_data ){ #flattening lists and filter repetitive data
#   ldf <- a_data$data$v2RoundModelPerformances %>% 
#     map_df( ., ~ replace(.x, is.null(.x),NA) ) %>%
#     # mutate( intraRoundSubmissionScores=intraRoundSubmissionScores %>% map_depth(.,1,~ifelse(is.null(.x),list(),.x)) )
#     mutate( intraRoundSubmissionScores=intraRoundSubmissionScores %>% map_depth(.,2,~ifelse(is.null(.x),NA,.x)) )
#     mutate( intraRoundSubmissionScores=map(intraRoundSubmissionScores,~as_tibble(t(.x))) ) %>%
#     # unnest( cols = c(intraRoundSubmissionScores) )
#     # filter( .,displayName == "tc" ) %>%
#     # mutate_if( is_list,replace_nulls_in_list )
#   
#   return( ldf )
# }

# for( mname in stakedmodels ){
#     
#   defaultquery %>%
#   gsub( "mid", mname %>% get_model_id(), . ) %>%
#   run_query() %>%
#   process_dataquery() %>%
#   write.csv( .,paste("./staked/",mname,".csv",sep="") )
# 
# }

for(mname in stakedmodels) {
  defaultquery %>%
    gsub("mid", mname %>% get_model_id(), .) %>%
    run_query() %>%
    process_dataquery() %>%
    # Convert all list columns to character
    dplyr::mutate(across(where(is.list), ~sapply(., toString))) %>%
    write.csv(paste("./staked/", mname, ".csv", sep=""))
}

#' Relative Cultivar Frequency (RCF)
#' @description calculation of the overall cultivar frequency by household
#' @param dfr data.frame table name that constains the data
#' @param vname numeric total number of household
#' @param hh character household column name
#' @param community character community name where. A community o communities belongs to certain locaiton
#' @param location character location or admin level column name where cultivars were sampled or monitored
#' @param shorten logical \code{shorten=TRUE} only show a simplified version of the original data. Whether \code{shorten=TRUE} append OCF and OCF indexes to original data
#' @return it returns the RCF index and RCF scale
#' @author Omar Benites
#' @examples
#' library(askha)
#' data(samdata)
#' ocf(dfr = samdata , vname="variety", hh="code_farmer", 
#' community = "cu_community", location = "ADM3_Name",shorten = TRUE)
#' @import dplyr
#' @importFrom dplyr group_by summarise distinct left_join mutate case_when n_distinct
#' @export

ocf <- function(dfr, vname, hh, community, location, shorten = FALSE){

  index_varname <- which(names(dfr) == vname)
  names(dfr)[index_varname] <- "variety_name"
  index_varname <- which(names(dfr) == hh)
  names(dfr)[index_varname] <- "hh"
  index_varname <- which(names(dfr) == community)
  names(dfr)[index_varname] <- "community"
  index_varname <- which(names(dfr) == location)
  names(dfr)[index_varname] <- "location"
  
  dfr_ccf <- ccf(dfr, "variety_name", "hh", "community", "location") 
  
  ##number of communities 
  ncom <- dfr[, "community"] %>% unique() %>% length()
  
  ## Sum of community cultivar frequency
  dfr_ccf <- dfr_ccf %>%
    group_by(variety_name) %>%
    mutate(sumccf = sum(ccf, na.rm = TRUE)) %>%
    ungroup()
  
  ## OCF values
  dfr_ocf <- dfr_ccf %>% mutate(OCF = sumccf/ncom)

  ## OCF sCALE
  dfr_ocf <- dfr_ocf %>% mutate(OCF_scale = case_when(
      OCF < 1 ~ "very few households",
      OCF < 5 & OCF >= 1 ~ "few households",
      OCF < 25 & OCF >= 5 ~ "many households",
      OCF > 25 ~ "most households",
  ))
 
  #TODO: agregar un OCF_AJUSTADO
  
  if(shorten){
     dfr_ocf <- left_join(dfr , dfr_ocf %>% select(community, variety_name, OCF, OCF_scale), by = c("community", "variety_name")) 
  } 
  
 return(dfr_ocf)
}
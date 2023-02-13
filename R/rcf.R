#' Relative Cultivar Frequency (RCF)
#' @description calculation of the relative cultivar frequency by household
#' @param dfr data.frame table name that constains the data
#' @param vname numeric total number of household
#' @param hh character household column name
#' @param nsvarie character number of sampled varieties
#' @param community character vector of one or multiple communities that belong to locations
#' @param location character location or admin level column name where cultivars 
#' were sampled or monitored
#' @param shorten logical \code{shorten=TRUE} only show a simplified version of the original data. Whether \code{shorten=TRUE} append OCF and OCF scales to original data
#' @return it returns the RCF index and RCF scale
#' @author Omar Benites
#' @examples
#' library(askha)
#' data(samdata)
#' rcf(samdata, "variety" ,"code_farmer", "number_tuber_by_sampling", "cu_community"  ,"ADM3_Name")
#' @import dplyr
#' @importFrom dplyr group_by summarise ungroup left_join mutate case_when n_distinct
#' @export

rcf <- function(dfr, vname, hh, nsvarie, community, location, shorten = FALSE) {
  
  #Rename columns in order to use DPLYR (non-standard evaluation)
  index_varname <- which(names(dfr) == vname)
  names(dfr)[index_varname] <- "variety_name"
  index_varname <- which(names(dfr) == hh)
  names(dfr)[index_varname] <- "hh"
  index_varname <- which(names(dfr) == nsvarie)
  names(dfr)[index_varname] <- "nsvarie"
  index_varname <- which(names(dfr) == location)
  names(dfr)[index_varname] <- "location"
  index_varname <- which(names(dfr) == community)
  names(dfr)[index_varname] <- "community"
  
  
  dfr_hcf <- hcf(dfr, "hh", "nsvarie")
  
  ## Calculo de la suma de HCFs por variedad del numerador: suma total de hcf de las familias o households
  temp_hcfxvarie <- dfr_hcf %>%
    group_by(variety_name, location) %>%
    summarise(totalhcfxvarie = sum(HCF, na.rm = TRUE)) %>%
    ungroup()

  ##  JOIN  tablas por variedad y location (o admin)
  dfr <- left_join(dfr, temp_hcfxvarie, by = c("variety_name", "location"))

  # ## conteo de household o agricultores agrupados por location (o admin)
  smry_conteo_hh_location <- dfr %>%
    group_by(location) %>%
    summarise(total_hh = n_distinct(hh, na.rm = TRUE))

  ## JOIN de los datos con los conteos
  dfr <- left_join(dfr, smry_conteo_hh_location, by = c("location"))

  ## Calculate the RCF index dividing total hcf x variety and total households
  dfr_rcf <- dfr %>% mutate(RCF = 100 * (totalhcfxvarie / total_hh))

  dfr_rcf <- dfr_rcf %>% mutate(RCF_scale = case_when(
    RCF < 0.05 ~ "very scarse",
    RCF >= 0.05 & RCF < 0.1 ~ "scarse",
    RCF >= 0.1 & RCF < 0.25 ~ "uncommon",
    RCF < 1 & RCF >= 0.25 ~ "common",
    RCF >= 1 ~ "abundant",
    TRUE ~ "no-specified"
  ))
  
  if(shorten){
      dfr_rcf <- dfr_rcf %>% distinct(variety_name, location, .keep_all = TRUE)
  }
  return(dfr_rcf)
}

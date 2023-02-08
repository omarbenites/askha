#' Relative Cultivar Frequency (RCF)
#' @description calculation of the overall cultivar frequency by household
#' @param dfr data.frame table name that constains the data
#' @param vname numeric total number of household
#' @param hh character household column name
#' @param community character community name where. A community o communities belongs to certain locaiton
#' @param location character location or admin level column name where cultivars were sampled or monitored
#' @return it returns the RCF index and RCF scale
#' @author Omar Benites
#' @examples
#' library(askha)
#' data(samdata)
#' dfr_hfc <- hcf(samdata, "code_farmer", "number_tuber_by_sampling")
#' @import dplyr
#' @importFrom dplyr group_by summarise distinct left_join mutate case_when n_distinct
#' @export

ocf <- function(dfr, vname, hh, community, location){

  index_varname <- which(names(dfr) == vname)
  names(dfr)[index_varname] <- "variety_name"
  index_varname <- which(names(dfr) == hh)
  names(dfr)[index_varname] <- "hh"
  index_varname <- which(names(dfr) == community)
  names(dfr)[index_varname] <- "community"
  index_varname <- which(names(dfr) == location)
  names(dfr)[index_varname] <- "location"
  
  #tamaño
  ncom <- dfr[, "cu_community"] %>% unique() %>% nrow()
  ## Calculate comunity cultivar frequency (CCF)
  datos_muestreo5 <- ccf(dfr, "variety_name", "hh", "community", "location", pctn = TRUE)
  
  smry_conteo_commu_admin3 <- datos_muestreo5 %>%
    group_by(location) %>%
    summarise(ntolcomunidades = n_distinct(community, na.rm = TRUE))
  
  datos_muestreo6 <- left_join(datos_muestreo5, smry_conteo_commu_admin3, by = c("location"))
  
  # datos_muestreo6 <- datos_muestreo5 %>% mutate(ntolcomunidades = total_comun)
  
  ## Calculo del total de CCF (commnunity cultivar freq) por ADM3 (tomando distintos cu_variety_name y cu_community)
  temp_ccf_varie6 <- datos_muestreo6 %>%
    distinct(variety_name, community, .keep_all = TRUE) %>%
    group_by(location, variety_name) %>%
    summarise(total_ccf_varie = sum(CCF, na.rm = TRUE))
  
  
  temp_ccf_varie6 <- temp_ccf_varie6 %>% mutate(OCF = 100 * (total_ccf_varie / 28))
  
  datos_muestreo7 <- left_join(datos_muestreo6, temp_ccf_varie6)
  
  
  out <- datos_muestreo7 %>% mutate(OCF_scale = case_when(
    OCF < 1 ~ "very few households",
    OCF < 5 & OCF >= 1 ~ "few households",
    OCF < 25 & OCF >= 5 ~ "many households",
    OCF > 25 ~ "most households",
  ))

}
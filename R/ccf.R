#' Community Cultivar Frequency (CCF)
#' @description calculation of the frequency of varieties by community
#' @param dfr data.frame table name that constains the data
#' @param vname numeric total number of household
#' @param hh character household column name
#' @param community character community name where. A community o communities belongs to certain locaiton
#' @param location character location or admin level column name where cultivars were sampled or monitored
#' @param pctn logical by default TRUE. FALSE if do not requiere in percentage mode
#' @return it returns the RCF index and RCF scale
#' @author Omar Benites
#' @examples
#' library(askha)
#' data(samdata)
#' dfr_hfc <- hcf(samdata, "code_farmer", "number_tuber_by_sampling")
#' @import dplyr
#' @importFrom dplyr group_by summarise ungroup left_join mutate case_when n_distinct
#' @export

ccf <- function(dfr, vname, hh, community, location, pctn = TRUE) {

  ncut <- NULL

  #Rename columns in order to use DPLYR (non-standard evaluation)
  index_varname <- which(names(dfr) == vname)
  names(dfr)[index_varname] <- "variety_name"
  index_varname <- which(names(dfr) == hh)
  names(dfr)[index_varname] <- "hh"
  index_varname <- which(names(dfr) == community)
  names(dfr)[index_varname] <- "community"
  index_varname <- which(names(dfr) == location)
  names(dfr)[index_varname] <- "location"
 
  #Conteo de variedades por localidad
  smry_totalhh_varie_community <- dfr %>%
    group_by(community) %>%
    count(variety_name) %>%
    ungroup()
  
  #JOIN de los dfr (dfr) y el conteo de variedades por localidad (smry_totalhh_varie_comunidad)
  dfr <- left_join(dfr, smry_totalhh_varie_community)
  #renombramos la columna "n" por "totalhh_varie_commu"
  dfr <- rename(dfr, totalhh_varie_commu = n )
 
  ## Nro de hogares que cultivan la variedad (i) en una comunidad (j) (denominador)
  smry_ntotalhhcomu <- dfr %>%
    group_by(community) %>%
    summarize(totalhh_comu = n_distinct(hh))
  
  dfr <- left_join(dfr, smry_ntotalhhcomu)
  
  
  ## numerador / denominadors
  dfr <- dfr %>% mutate(
    CCF = (totalhh_varie_commu / totalhh_comu),
  )
  if(pctn){
    dfr <- dfr %>% mutate(
    CCF100 = (totalhh_varie_commu / totalhh_comu) * 100
    )
  }
  
}  
  
  


  
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

  # Rename columns in order to use DPLYR (non-standard evaluation)
  index_varname <- which(names(dfr) == vname)
  names(dfr)[index_varname] <- "variety_name"
  index_varname <- which(names(dfr) == hh)
  names(dfr)[index_varname] <- "hh"
  index_varname <- which(names(dfr) == community)
  names(dfr)[index_varname] <- "community"
  index_varname <- which(names(dfr) == location)
  names(dfr)[index_varname] <- "location"

  ## nnh: count distinct households (or farmers) by community
  smry_codefarmer <- dfr %>%
    group_by(community) %>%
    summarise(nhh = n_distinct(hh))

  # calculation of nvxhh: number of household by variety and community
  smry_ntotalhhcomu <- dfr %>%
    group_by(community, variety_name) %>%
    summarize(nhhxvarie = n_distinct(hh))

  out <- left_join(smry_codefarmer, smry_ntotalhhcomu, by = c("community"))

  out <- out %>%
    mutate(ccf = (nhhxvarie / nhh) * 100) %>%
    ungroup()

  ##
 
  ## TODO': AGREGAR NUMERO DE comunidades del amd3
  # kk <- left_join(dfr, out, c("community", "variety_name"))
  # left_join(kk, out, by = "variety_name")

  ## TODO' HACER UN JOIN CON LA DATA ORIGINAL

}


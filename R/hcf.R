#' Household Cultivar Frequency (HCF)
#' @description calculation of the frequency of cultivars by houesholds
#' @param dfr data.frame table name that constains the data
#' @param hh character household colum name
#' @param nsvarie numeric number of sampled varieties 
#' @param ncut integer minimum (from the total) number of sampled varied
#' @param pctn logical by default TRUE. FALSE if do not requiere in percentage mode
#' @author Omar Benites
#' @return it returns the HCF
#' @author Omar Benites
#' @examples
#' library(askha)
#' data(samdata)
#' dfr_hfc <- hcf(samdata, "code_farmer", "number_tuber_by_sampling")
#' @import dplyr
#' @importFrom dplyr group_by summarise ungroup left_join mutate case_when
#' @export
  
hcf <- function(dfr, hh, nsvarie, ncut, pctn = TRUE ){
  
  
  #convert to data.frame if tibble
  if(inherits(iris,"tibble")){
    dfr <- as.data.frame(dfr,stringsAsFactors=FALSE)  
  }
  
  
  #TODO: cut or thresshold 
  ncut <- NULL
  
  #rename columns in order to use dplyr functions
  index_varname <- which(names(dfr)==hh)
  names(dfr)[index_varname] <- "hh"
  index_varname <- which(names(dfr)==nsvarie)
  names(dfr)[index_varname] <- "nsvarie"
  
  #Calculate the sum of sampled varieties by household (tn_hh)
  smry_muestreo_tntuber <- dfr %>%
    group_by(hh) %>%
    summarize(tn_hh = sum(nsvarie, na.rm = TRUE)) %>%
    ungroup()
  
  #Calculates House Cultivar Frequency (HCF) diving: number of sampled varieties / total number of tuber by households
  dfr_hcf <- left_join(dfr, smry_muestreo_tntuber, by = "hh") %>% mutate(
    HCF = (nsvarie / tn_hh)
  )
  # HCF in percetage mode
  if(pctn){
    dfr_hcf <- dfr_hcf %>% mutate(
      HCF_percent = 100 * (nsvarie / tn_hh)
    )
  }
  
  #dfr_hcf <- dfr_hcf %>% select(hh,nsvarie,tn_hh,HCF, HFC_percent)
  
}





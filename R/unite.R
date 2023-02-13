#' Gather RCF AND OCF indexes in one data.frame
#' @param x data.frame RCF index table
#' @param y data.frame OCF index table
#' @return data.frame with both indexes ordered according to common atributes
#' @author Omar Benites
#' @description this function binds RCF and OCF indexes in two consecutives columns, in order to look at glance both indexes.
#' @export
#' 
both_indexes <- function(x, y){
  out <- inner_join(x,y)
}
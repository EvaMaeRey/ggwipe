#' Title
#'
#' @param index 
#'
#' @return
#' @export
#'
#' @examples
last_plot_wipe <- function(index = NULL){
  
  
  p <- ggplot2::last_plot()
  
  # removes all layers specification
  if(is.null(index)){ p[[2]] <- NULL }else{p[[2]][[index]] <- NULL}
  
  return(p)
  
}

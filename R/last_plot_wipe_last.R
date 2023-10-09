#' Title
#'
#' @return
#' @export
#'
#' @examples
last_plot_wipe_last <- function(){
  
  
  p <- ggplot2::last_plot()
  
  index <- length(p[[2]])
  
  # removes all layers specification
  p[[2]][[index]] <- NULL
  
  return(p)
  
}

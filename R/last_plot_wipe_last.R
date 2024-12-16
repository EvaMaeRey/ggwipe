#' Title
#'
#' @return
#' @export
#'
#' @examples
last_plot_wipe_last <- function(){
  
  
  p <- ggplot2::last_plot()
  
  index <- length(p$layers)
  
  # removes all layers specification
  p$layers[[index]] <- NULL
  
  return(p)
  
}

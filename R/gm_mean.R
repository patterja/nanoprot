gm_mean = function(x){
#' geometric mean 
#'
#' @param parameters (vector)
#' @return numeric
#' @export
  g=exp(mean(log(x+1, base = exp(1))))
  g=g-1
  return(g)
}

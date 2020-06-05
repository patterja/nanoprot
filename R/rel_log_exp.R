rel_log_exp <- function (Y) 
#' relative log expression calculation
#'
#' @param Y data matrix. Rows are observations and columns are features (e.g. genes).
#' @return numeric
#' @export
{
features = colnames(Y)
print(paste0("Using columns as features.\nFeatures are: ", paste0(features[1:10], collapse=","), "..."))

Yrle = apply(Y, 2, function(x) x-median(x))

return(Yrle)
}

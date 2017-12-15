

#' Convenience function for getting a cond. inference tree
#' @param seed If you want to fix the results so that they will be consistent
#' between each rounds
#' @importFrom party ctree
#' @export

GetTree <- function(seed=F){
    if(seed) set.seed(seed)
    mytree <- ctree(responsename ~ paste0(predictors, ,collapse="+"),mydf)

}

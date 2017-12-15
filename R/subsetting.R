
#' Collect a subset for random selection
#'
#' @param parametry.1 list (attr=x, values=x)
#' @param parametry.2 list (attr=x, values=x)
#' @export

GetRandomSubset <- function(mydf, parametry.1, parametry.2, n){
    all.samples <- list()

    for(parametr in parametry.1$values){
        ss <- mydf[mydf[[parametry.1$attr]] == parametr, ]
        for(sub.parametr in parametry.2$values){
            ss2 <- ss[ss[[parametry.2$attr]] == sub.parametr, ]
            this.n <- ifelse(nrow(ss2)<n,nrow(ss2),n)
            all.samples[[paste0(parametr,sub.parametr,collapse=".")]] <- ss2[sample(c(1:nrow(ss2)),this.n),]
        }
    }

    return(all.samples)

}


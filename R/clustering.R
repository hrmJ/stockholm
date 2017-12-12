# Cluster analysis
# см. Левшина  303

#' Get a matrix between patterns organized in a data frame
#' with only categorical variables.
#' Expects the column identifying the patterns to be the first one
#'
#' @param mydf the dataframe
#' @export 

GetPatternMatrix <- function(mydf){
    patterns  <- setNames(lapply(levels(mydf[,1]),function(x,d)return(d[d[,1]==x,-1]),d=mydf),levels(mydf[,1]))
    predictor.probs <- setNames(lapply(patterns, function(pat){
           return(unlist(sapply(names(pat), function(pred.name, d)return(prop.table(table(d[[pred.name]]))),d=pat)))
            }),names(patterns))
    mydf.bp <- t(as.data.frame(predictor.probs))
    return(mydf.bp)
}

#' Compute the widths of the silhouttes of different clustering solutions
#'
#' @param myhc a hierarchically clustered object
#' @param mydist a distance matrix on which the clustering was based
#' @param maxclust maximum number of clusters
#' 
#' @importFrom cluster silhouette
#' @export

GetSilhouettes <- function(myhc, mydist, maxclust){
    silh.vector <- sapply(2:maxclust, function(thisn){
                            return(summary(silhouette(cutree(myhc, k=thisn),mydist))$avg.width)
                        })
    names(silh.vector) <- c(2:maxclust)
    return(silh.vector)
}


#caus.bp <- GetPatternMatrix(caus)
#caus.dist <- dist(caus.bp,method="canberra")
#caus.hc <- hclust(caus.dist,method="ward.D2")
#plot(caus.hc, hang=-1)
#cut the tree in n clusters:
#cutree(caus.hc, 2)
#GetSilhouettes(caus.hc, caus.dist,8)




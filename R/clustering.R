# Cluster analysis
# см. Левшина  303

#' Get a matrix between patterns organized in a data frame
#' with only categorical variables.
#' Expects the column identifying the patterns to be the first one
#'
#' @param mydf the dataframe
#' @export 

GetPatternMatrix <- function(mydf){
    for(colname in colnames(mydf)){
        mydf[,colname] <- as.factor(mydf[,colname])
    }

    patterns  <- setNames(lapply(levels(mydf[,1]),function(x,d)return(d[d[,1]==x,-1]),d=mydf),levels(mydf[,1]))
    predictor.probs <- setNames(lapply(patterns, function(pat){
           return(unlist(sapply(names(pat), function(pred.name, d)return(prop.table(table(d[[pred.name]]))),d=pat)))
            }),names(patterns))

    allnames <- c()
    for(this.predictor in predictor.probs){
        for (combination in names(this.predictor)){
            if(!combination %in% allnames){
                allnames <- c(allnames, combination)
            }
        }
    }

    new.predictor.probs <- list()
    for(this.predictorname in names(predictor.probs)){
        this.predictor  <- predictor.probs[[this.predictorname]]
        for(thisname in allnames){
            if(!thisname %in% names(this.predictor)){
                prev.names <- names(this.predictor)
                new.names <- c(names(this.predictor),thisname)
                this.predictor <- c(this.predictor,0.0)
                names(this.predictor) <- new.names
            }
        }
        new.predictor.probs[[this.predictorname]] <- this.predictor
    }

    mydf.bp <- t(as.data.frame(new.predictor.probs))
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
#' @examples
#' 
#' #Example1:
#' 
#' primery <- GetExamplesFromTextFiles("/home/juho/data/ira_txt/")
#' primery.subset  <- primery[,c("adj","participants","pattern","tonalnost")]
#' primery.matrix <- GetPatternMatrix(primery.subset)
#' primery.dist <- dist(primery.matrix, method="canberra")
#' primery.hc <- hclust(primery.dist,method="ward.D2")
#' GetSilhouettes(primery.hc, primery.dist,length(unique(primery.subset$adj))-1)
#' plot(primery.hc)
#' 
#' #Example2:
#' 
#' 
#' 
GetSilhouettes <- function(myhc, mydist, maxclust){
    silh.vector <- sapply(2:maxclust, function(thisn){
                            return(summary(silhouette(cutree(myhc, k=thisn),mydist))$avg.width)
                        })
    names(silh.vector) <- c(2:maxclust)
    return(silh.vector)
}






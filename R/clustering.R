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

    patterns  <- setNames(lapply(levels(mydf[,1]),function(x,d)return(as.data.frame(d[d[,1]==x,-1])),d=mydf),levels(mydf[,1]))
    if(length(colnames(mydf)) < 3){
        #If only one variable
        name <- colnames(mydf)[2]
        for(patname in names(patterns)){
            colnames(patterns[[patname]]) <- name
        }
        predictor.probs <- setNames(lapply(patterns, function(pat){
                props <- unlist(sapply(names(pat), function(pred.name, d)return(prop.table(table(d[[pred.name]]))),d=pat))
                pnames <- paste(as.character(dimnames(props)[[2]]),as.vector(dimnames(props)[[1]]),sep=".")
                props <- as.vector(props)
                names(props) <- pnames
                   return(props)
                }),names(patterns))
    }
    else{
        predictor.probs <- setNames(lapply(patterns, function(pat){
                props <- unlist(sapply(names(pat), function(pred.name, d)return(prop.table(table(d[[pred.name]]))),d=pat))

               return(props)
                }),names(patterns))
    }

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
#' # Example 1
#' primery <- GetExamplesFromTextFiles("/home/juho/data/ira_txt/")
#' primery.subset  <- primery[,c("adj","participants","pattern","tonalnost")]
#' primery.matrix <- GetPatternMatrix(primery.subset)
#' primery.dist <- dist(primery.matrix, method="canberra")
#' primery.hc <- hclust(primery.dist,method="ward.D2")
#' silhouette.widths <- GetSilhouettes(primery.hc, primery.dist,length(unique(primery.subset$adj))-1)
#' plot(primery.hc)
#' 
#' # Example 2
#' primery <- GetExamplesFromTextFiles("/home/juho/data/ira_txt/")
#' primery.subset  <- primery[,c("adj","participants","pattern","tonalnost")]
#' primery.subset$adj <- as.character(primery.subset$adj)
#' primery.subset <- subset(primery.subset,adj != "ljubimyj")
#' primery.subset$adj <- apply(primery.subset,1,function(r)gsub("\\[adj\\]",r["adj"],r["pattern"],))
#' primery.subset  <- primery.subset[,c("adj","participants")]
#' primery.matrix<-GetPatternMatrix(primery.subset)
#' primery.dist <- dist(primery.matrix, method="canberra")
#' primery.hc <- hclust(primery.dist,method="ward.D2")
#' silhouette.widths <- GetSilhouettes(primery.hc, primery.dist,length(unique(primery.subset$adj))-1)
#' plot(primery.hc)
#' 
#' 
#' # Example 3
#' primery <- GetExamplesFromTextFiles("/home/juho/data/ira_txt/")
#' primery.subset  <- primery[,c("adj","participants","pattern","tonalnost")]
#' primery.subset$adj <- apply(primery.subset,1,function(r)gsub("\\[adj\\]",r["adj"],r["pattern"],))
#' primery.subset  <- primery.subset[,c("adj","participants")]
#' primery.matrix<-GetPatternMatrix(primery.subset)
#' primery.dist <- dist(primery.matrix, method="canberra")
#' primery.hc <- hclust(primery.dist,method="ward.D2")
#' silhouette.widths <- GetSilhouettes(primery.hc, primery.dist,length(unique(primery.subset$adj))-1)
#' plot(primery.hc)
#' 
#' 
#' # Example 4
#' primery <- GetExamplesFromTextFiles("/home/juho/data/ira_txt/")
#' primery.subset  <- primery[,c("adj","participants","pattern","tonalnost")]
#' primery.subset$adj <- as.character(primery.subset$adj)
#' primery.subset <- subset(primery.subset,adj != "ljubimyj")
#' primery.subset$adj <- apply(primery.subset,1,function(r)gsub("\\[adj\\]",r["adj"],r["pattern"],))
#' primery.matrix<-GetPatternMatrix(primery.subset)
#' primery.dist <- dist(primery.matrix, method="canberra")
#' primery.hc <- hclust(primery.dist,method="ward.D2")
#' silhouette.widths <- GetSilhouettes(primery.hc, primery.dist,length(unique(primery.subset$adj))-1)
#' plot(primery.hc)
#'
GetSilhouettes <- function(myhc, mydist, maxclust){
    silh.vector <- sapply(2:maxclust, function(thisn){
                            return(summary(silhouette(cutree(myhc, k=thisn),mydist))$avg.width)
                        })
    names(silh.vector) <- c(2:maxclust)
    return(silh.vector)
}






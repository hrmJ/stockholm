#' @importFrom PythonInR pyExecfile, pyCall
#' @export
ConvertExamplesToTextFiles <- function(){
    fname <- "/home/juho/projects/stockholm/inst/python/collector.py"
    examples_path <- "/home/juho/juho.harme@gmail.com/events/stockholm17"
    pyExecfile(fname)
    ex.list <- pyCall("ConvertToList", examples_path)
}

#' Collect all examples from the text files
#'
#' @return a data frame consisting of all the examples in the specified folder
#' @importFrom PythonInR pyExecfile, pyCall
#' @export

GetExamplesFromTextFiles <- function(){
    #fname <- system.file("python", "collector.py", package="stockholm")
    fname <- "/home/juho/projects/stockholm/inst/python/collector.py"
    examples_path <- "/home/juho/juho.harme@gmail.com/events/stockholm17"
    pyExecfile(fname)
    ex.list <- pyCall("ConvertToList", examples_path)
    examples <- data.frame()
    cat("Converting the list of examples to data frame...","\n")
    for(idx in 1:length(ex.list)){
        cat(idx,"/",length(ex.list),"\n")
        thisrow <- ex.list[[idx]]
        r <- as.vector(thisrow)
        names(r) <- names(thisrow)
        r <- t(as.data.frame(r))
        examples <- rbind(examples,r)
    }
    cat("Done!","\n")
    return(examples)
}



#' Convert the word files to plain txt files using the python 
#' function supplied with this package.
#'
#' @param path the path to the folder including the examples
#' @param txt_path path to the folder where the txt files will be saved
#' @importFrom PythonInR pyExecfile @importFrom PythonInR pyCall
#' @export

ConvertExamplesToTextFiles <- function(path, txt_path){
    fname <- system.file("python", "collector.py", package="stockholm")
    pyExecfile(fname)
    pyCall("ConvertToTxt", c(path, txt_path))
}

#' Collect all examples from the text files
#'
#' @param path path to the folder where the txt files will be saved 
#' @return a data frame consisting of all the examples in the specified folder
#' @importFrom PythonInR pyExecfile
#' @importFrom PythonInR pyCall
#' @importFrom stringi stri_trans_general
#' @export

GetExamplesFromTextFiles <- function(path){
    fname <- system.file("python", "collector.py", package="stockholm")
    pyExecfile(fname)
    ex.list <- pyCall("ConvertToList", path)
    examples <- do.call(rbind.data.frame, ex.list)
    colnames(examples) <- names(ex.list[[1]])
    examples$participants <- stri_trans_general(examples$participants,"cyrillic-latin")
    examples$participants <- gsub("â","ja",examples$participants)
    examples$participants <- gsub("û","ju",examples$participants)
    examples$participants <- gsub("č","ch",examples$participants)
    examples$participants <- gsub("š","sh",examples$participants)
    examples$participants <- gsub("\\ʹ","",examples$participants)
    examples$participants <- gsub("tovaris1","tovarish",examples$participants)
    examples$participants <- gsub("dorogj","[adj]",examples$participants)
    examples$participants <- gsub("rodoj","[adj]",examples$participants)
    examples$participants <- gsub("aggress","[adj]",examples$participants)

    #Separating patterns
    examples$pattern <- as.character(examples$pattern)

    allpats <- list(
                    list(pat="viele adjektive",repl="[adj] + [other adj]"),
                    list(pat="plus andere adjektive",repl="[adj] + [other adj]"),
                    list(pat="flera adjektive",repl="[adj] + [other adj]"),
                    list(pat="substaniv \\[adj\\] moj",repl="subst [adj]"),
                    list(pat="substant plus moj \\[adj\\]",repl="subst moj [adj]"),
                    list(pat="s plus moj \\[adj\\]",repl="subst moj [adj]"),
                    list(pat="substantiv \\[adj\\] moj",repl="subst [adj] moj"),
                    list(pat="\\[adj\\] substantiv moj",repl="[adj] subst moj"),
                    list(pat="substantiv moj \\[adj\\]",repl="subst moj [adj]"),
                    list(pat="substantiv plus \\[adj\\]",repl="subst [adj]"),
                    list(pat="\\[adj\\] plus substantive",repl="[adj] subst"),
                    list(pat="\\[adj\\] plus s plus moj",repl="[adj] subst moj"),
                    list(pat="\\[adj\\] moj substantiv",repl="[adj] moj subst"),
                    list(pat="\\[adj\\] moj plus s",repl="[adj] moj subst"),
                    list(pat="moj \\[adj\\] plus substantiv", repl="moj [adj] subst"),
                    list(pat="moj \\[adj\\] substantiv", repl="moj [adj] subst"),
                    list(pat="moj \\[adj\\] nomen", repl="moj [adj] nomen"),
                    list(pat="nomen \\[adj\\] moj", repl="nomen [adj] moj"),
                    list(pat="nomen moj \\[adj\\]", repl="nomen moj [adj]"),
                    list(pat="nomen \\[adj\\]", repl="nomen [adj]"),
                    list(pat="\\[adj\\] nomen", repl="[adj] nomen"),
                    list(pat="nomen plus \\[adj\\]", repl="nomen [adj]"),
                    list(pat="\\[adj]\\s?\\+n",repl="[adj] nomen"),
                    list(pat="\\[adj\\] n",repl="[adj] nomen"),
                    list(pat="moj \\[adj\\]",repl="moj [adj]"),
                    list(pat="miloe n",repl="[adj] nomen"),
                    list(pat="moj [adj] niania",repl="moj [adj] subst"),
                    list(pat="moj [adj] njanja",repl="moj [adj] subst"),
                    list(pat="\\[adj\\] moja?",repl="[adj] moj")
                    )
    for(thispat in allpats){
        examples$pattern[grepl(thispat$pat,examples$participants)] <- thispat$repl
        examples$participants <- gsub(thispat$pat," ",examples$participants)
    }

    examples$pattern <- as.factor(examples$pattern)
    #Cleaning up
    examples$participants <- gsub("vokativ","",examples$participants)
    examples$participants <- gsub("vok ","",examples$participants)
    examples$participants <- gsub("\\s*\\[adj\\]\\s*"," ",examples$participants)
    examples$participants <- gsub("\\s+"," ",examples$participants)
    examples$participants <- gsub("\\s+$","",examples$participants)
    examples$participants <- gsub("^\\s+","",examples$participants)
    examples$participants <- gsub("iania","njanja",examples$participants)
    examples$participants <- gsub("nnjanja","njanja",examples$participants)
    examples$participants <- gsub("^janja","njanja",examples$participants)
    examples$participants <- gsub("(unbekannt1|unbekannte)","unbekannt",examples$participants)
    examples$participants <- gsub(" uns "," und ",examples$participants)
    examples$participants[examples$participants==""] <- "?"
    examples$context <- as.character(examples$context)

    #fix categories

    fname <- system.file("data", "utshastniki.csv", package="stockholm")
    fixes <- read.csv(fname)
    for(idx in c(1:nrow(fixes))){
        examples$participants[examples$participants==trimws(fixes[idx,1])] <- trimws(fixes[idx,2])
    }

    #clean up adjective names
    examples$adj <- as.character(examples$adj)
    examples$adj <- stri_trans_general(examples$adj,"cyrillic-latin")
    examples$adj <- gsub("û","ju",examples$adj)
    examples$adj <- as.factor(examples$adj)
    examples <- examples[examples$participants!="X",]


    return(examples)
}




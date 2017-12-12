library(readr)
adjectives  <- c("Любимый","Милый","Дорогой","Родной")
adjectives_trans  <- c("lubimyj","milyj","dorogoj","rodnoj")
examples <- data.frame("adjective"=c(),"pattern"=c(),"context"=c(),stringsAsFactors=F)
for(adj in adjectives){
    path <- paste0("Beispiele_txt/",tolower(adj))
    files <- list.files(path)
    show(adj)
    idx <- 0
    for(this_file in files){
        show(paste0(idx,"/",length(files)))
        this_text  <- read_file(paste0("Beispiele_txt/", tolower(adj),"/", this_file))
        contexts <- as.vector(strsplit(this_text,"\n{2,}")[[1]])
        pattern  <- gsub(".txt","",this_file,)
        #pattern  <- gsub(paste0("\\s*(",paste0(adjectives,collapse="|"),paste0(adjectives_trans,collapse="|"),")\\s*"), "",this_file,ignore.case=T)
        for (context in contexts){
            examples <- rbind(examples,data.frame("adjective"=adj,"pattern"=pattern,"context"=context))
        }
        idx <- idx +1
    }
}
saveRDS(examples,"examples.rds")

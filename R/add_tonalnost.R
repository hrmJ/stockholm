#' Collect all examples from the text files and return them as a data frame (unprocessed)
#'
#' @param path path to the folder where the txt files have been saved 
#' @importFrom pbapply pbsapply
#' @import dplyr
#' @export
AddTonalnost <- function(path, temp=F){
    bolshaja.temp <- bolshaja %>% 
                    mutate(context=as.character(context)) %>% 
                    mutate(context=gsub("\\s+"," ",context)) %>% 
                    mutate(tonalnost=as.character(tonalnost)) %>%
                    as_tibble
    if(!temp){
        temp <- GetExamplesFromTextFiles(path) %>% filter(context != "" & !is.na(context)) %>% mutate(context=as.character(context))
        temp$matched <- pbsapply(temp$context,findmatch, temp2=bolshaja.temp) %>% unname 
    }

    for(i in c(1:nrow(temp))){
         r <- temp[i,]
         m <- r[["matched"]]
         if(! m %in% c("no match","multiple")){
             #Old context, add tonalnost
             bolshaja.temp[as.integer(m),"tonalnost"]  <- "aggr"
         }
         else if (m == "no match"){
             #New context, let's add it as a new row
                 gsub(".*\\]([^]]+)$","\\1",r[["context"]]) %>%
                 gsub("\\s+"," ",.)  %>% gsub("\\s*\\d+\\s*","",.)  %>% 
                 gsub("sverhuvniz", "sverhu vniz", .)  %>% 
                 gsub(".*(znakom druz kolleg|snizu vverh|sverhu vniz|prochee|neznakomye|roditeli deti|brat sestra).*", "\\1", .)  %>% 
                 trimws(.)  %>% 
                 gsub("rodsvenniki", "rodstvenniki", .) -> part
             bolshaja.temp  %>% add_row(context=r$context,
                                        adj=r$adj,
                                        participants=part,
                                        year=r$year,
                                        pattern=r$pattern,
                                        other=r$other,
                                        tonalnost="aggr"
                                        ) -> bolshaja.temp
         }
         else{
             #multiple matches
             show(i)
         }
    }
    bolshaja.temp[nchar(bolshaja.temp$participants)>50, "participants"]  <- "?"
    bolshaja.temp$participants[bolshaja.temp$participants=="lubovniki"]  <- "vlublennye"
    bolshaja.temp$participants[grepl("В гробу я видал вашего Гиппократа вместе с его клятвой. Да ваш",bolshaja.temp$context)] <- "znakom druz kolleg"
    bolshaja.temp$participants[grepl("Не знаю никакого гинеколога. А Нугзара год не видел",bolshaja.temp$context)] <- "snizu vverh"
    bolshaja.temp$participants[grepl("Он повернул к Левинсону бритую",bolshaja.temp$context)] <- "znakom druz kolleg"

    return (bolshaja.temp)


}


#' Find out whether this context is already in the data or not
#'
#' @export
findmatch <- function(x, temp2, showmulti=F){
               y  <- gsub("^.{10}(.{50}).*","\\1",x)
               y <- gsub("\\s+"," ",y)
               x <- as.character(x)
               x <- gsub("\\s+"," ",x)
               allmatches <- which(grepl(y,temp2$context,fixed=T))
               if(length(allmatches)==1){
                  return(allmatches)
               }
               else if(length(allmatches)==0){
                  return("no match")
               }
               else{
                   patlength <- ifelse(nchar(x)-11 > 250,250,nchar(x)-11)
                   y  <- gsub(paste0("^.{10}(.{", patlength , "}).*"),"\\1",x)
                   allmatches <- which(grepl(y,temp2$context,fixed=T))
                   if(length(allmatches)==1){
                      return(allmatches)
                   }
                   else if(length(allmatches)==0){
                      return("no match")
                   }
                   if(showmulti){
                      return(allmatches)
                   }
                   #IF multiple matches, return the first (just to make things less complicated)
                  return(allmatches[1])
               }

}


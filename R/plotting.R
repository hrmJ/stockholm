#' Prints a plot comparing the profiles of different adjectives
#'
#' @importFrom ggplot2 ggplot geom_bar geom_text coord_flip position_stack
#' @importFrom dplyr  %>% as_tibble
#'
#' @param abs_table cross-tabulated table with absolute numbers
#' @param adjectives a vector consisting of th adjectives / constructions you want to compare
#' @param min_pc minimum percentage value to be printed as text on the plot
#' @param vertical wether or not to output the text labels vertically
#'
#'
#' @export 
#' @examples
#' 
#' # Example 1:
#' 
#' library(stockholm)
#' bolshaja$otdelno  <- apply(bolshaja,1,function(r)gsub("\\[adj\\]",r[["adj"]],r[["pattern"]])) 
#' bolshaja_moj <- subset(bolshaja, grepl("moj",pattern))
#' bolshaja_bez_moj <- subset(bolshaja, !grepl("moj",pattern))
#' tablica_s_absoljutnymi_tsiframi_moj <- table(bolshaja_moj$participants, bolshaja_moj$otdelno)
#' tablica_s_absoljutnymi_tsiframi_bez_moj <- table(bolshaja_bez_moj$participants, bolshaja_bez_moj$otdelno)
#' PrintProfilePlot(tablica_s_absoljutnymi_tsiframi_moj,c("milyj moj","moj milyj","dorogoj moj","moj dorogoj"))
#' 
#' 

PrintProfilePlot <- function(abs_table, adjectives, min_pc=4, vertical=FALSE){ 


    ot <- abs_table %>% 
        prop.table(.,2) %>% 
        round(4)*100 
    ot <- ot %>% as.data.frame  %>%  as_tibble

    if (vertical == TRUE) {
      return(ot %>% dplyr::filter(Var2 %in% adjectives) %>% ggplot(.,
          aes(x = Var2, y = Freq, fill = Var1)) + coord_flip() +
          geom_bar(stat = "identity") + geom_text(aes(label = ifelse(Freq >=
          4, paste0(Var1, " ", Freq, "%"), "")), angle=90, position = position_stack(vjust = 0.5),
          colour = "white"))
    }
    else{
      return (ot  %>% dplyr::filter(Var2 %in% adjectives) %>% 
          ggplot(.,aes(x=Var2,y=Freq,fill=Var1)) + 
          coord_flip() + 
              geom_bar(stat="identity") + 
           geom_text(aes(label=ifelse(Freq >= 4, paste0(Var1,"=\n",  Freq,"%"),"")), position=position_stack(vjust=0.5), colour="white"))
    }

}


#' Returns a table filtered by participants
#'
#'
#' @param dannyje bolshaja, bolshaja_moj, bolshaja_bez_moj etc..
#' @param spisok_utshastnikov list of participants to include in the table
#'
#'
#' @export 
#' @examples
#' 
#' # Example 1:
#' 
#' tablitsa  <-  sozdai_tablitsa_s_utshastnikamki(bolshaja_bez_moj, c('roditeli deti', 'deti roditeli'))
#' PrintProfilePlot(tablitsa ,c("dorogoj", "milyj", "rodnoj", "lubimyj"), vertical=T) 
#' 
#' 
sozdai_tablitsa_s_utshastnikamki <- function(dannyje, spisok_utshastnikov){
  subsetted <- subset(dannyje, participants %in% spisok_utshastnikov)
  return(table(subsetted$participants, subsetted$otdelno))
}

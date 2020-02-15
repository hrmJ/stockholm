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



#' Returns a table filtered by adj
#'
#'
#' @param dannyje bolshaja, bolshaja_moj, bolshaja_bez_moj etc..
#' @param this_adj the adjective in question (dorogoj, milyj...)
#'
#'
#' @examples
#' 
#' # Example 1:
#' 
#' tablitsa  <-  sozdai_tablitsa_s_prilagatelnym(bolshaja, 'dorogoj')
#' 
#' 
#' @export 

sozdai_tablitsa_s_prilagatelnym <- function(dannyje, this_adj){
  subsetted <- subset(bolshaja, adj==this_adj)
  return(table(subsetted$participants, subsetted$otdelno))
}

#' set the theme of a plot
#' 
#' @import ggplot2 

setTheme <- function(thispl, y_axis_all_percents){
  pl <- thispl + 
         coord_flip( ) + 
         theme(axis.text.x = element_text(angle=90, hjust=1))
  if(y_axis_all_percents){
     pl <- pl + scale_y_continuous(limits=c(0,100))
  }
  return(pl)
}

#' Returns a barplot of adjectives
#'
#'
#' @param dannyje bolshaja, bolshaja_moj, bolshaja_bez_moj etc..
#' @param adj the adjective ('dorogoj', 'milyj'...)
#' @param spisok_utshastnikov vector of participants to include in the plot (if not set, then all)
#' @param protsenty whether to use percentages instead of counts
#' @param freqlabels whether to print labels of percentages on the plot
#' @param nlabels whether to print labels of counts on the plot
#' @param y_axis_all_percents whether to use scale of 0 to 100 with percentages 
#'
#' @import ggplot2 
#' @import dplyr 
#'
#' @export 
#' @examples
#' 
#' 
#'# samyj prostoi:
#' sozdai_barplot_s_prilagatelnym(bolshaja, 'dorogoj')
#'
#'# esli hotshetsa ogranitsit spisok utshastnikoc
#'sozdai_barplot_s_prilagatelnym(bolshaja, 'dorogoj', spisok_utshastnikov=c('niania', 'snizu vverh', 'zhivotnye'))
#'
#'# esli hotshetsa pokazat protsenty
#'sozdai_barplot_s_prilagatelnym(bolshaja, 'dorogoj', protsenty=T)
#'
#'# ili mozhet byt lutshe vot tak
#'sozdai_barplot_s_prilagatelnym(bolshaja, 'dorogoj', protsenty=T, y_axis_all_percents=T)
#'
#'# potom mozhno eshe napisat tsifry nad barami
#'sozdai_barplot_s_prilagatelnym(bolshaja, 'dorogoj', protsenty=T, y_axis_all_percents=T, freqlabels=T)
#'
#'# ...ili eshe
#'sozdai_barplot_s_prilagatelnym(bolshaja, 'dorogoj', protsenty=T, y_axis_all_percents=T, freqlabels=T, nlabels=T)
#'
sozdai_barplot_s_prilagatelnym <- function(dannyje, this_adj, spisok_utshastnikov = c(), protsenty = F, freqlabels = F, nlabels = F, y_axis_all_percents = F){
  tab <- bolshaja %>% filter(adj == this_adj)
  if(length(spisok_utshastnikov) > 0){
    tab <- tab %>% filter(participants %in% spisok_utshastnikov)
  }
  tab <- tab %>% count(participants, otdelno) %>%
    group_by(otdelno) %>%
    mutate(freq = n / sum(n) *100) %>%
    ungroup %>%
    arrange(desc(freq))
  if(protsenty){
    pl <- tab %>% ggplot(aes(x = participants, y=freq) ) 
  }
  else{
    pl <- tab %>% ggplot(aes(x = participants, y=n) )
  }

  pl <- pl +geom_bar(stat='identity') + facet_wrap(~otdelno)  

  if(nlabels && freqlabels){
    pl <- pl  + geom_text(aes(label=paste0("n=",n," (",round(freq,1)," %)")),  hjust=-0.3)
  }
  else if(freqlabels){
    pl <- pl  + geom_text(aes(label=paste0(round(freq,1), ' %')),  hjust=-0.3)
  }
  else if(nlabels){
    pl <- pl  + geom_text(aes(label=paste0('n=',n)),  hjust=-0.3)
  }
  return (setTheme(pl, y_axis_all_percents))
}



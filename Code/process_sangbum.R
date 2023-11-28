complaint <- read.csv("complaints.csv")
library(dplyr)
library(textstem)
library(tm)
library(stringr)
View(complaint)




# group and paste comments 

Issue.complaint <- complaint %>% filter(Consumer.complaint.narrative != "") %>%
  group_by(Issue) %>%
  summarise(comments = paste(Consumer.complaint.narrative, collapse = " "))

State.complaint <- complaint %>% filter(Consumer.complaint.narrative != "" & State != "None") %>%
  group_by(State) %>%
  summarise(comments = paste(Consumer.complaint.narrative, collapse = " ")) 

## pr-proccesi?  

process_text <- function(df) {
  df$comments <- df$comments %>% 
    str_remove_all("[:punct:]") %>% 
    str_remove_all("[:digit:]") %>%
    str_remove_all("\\n|\\$|X{2,}") %>%
    str_replace_all("[:blank:]{2,}", " ") %>%
    str_remove_all("#|>|\\+|=") %>%
    tolower()
  return(df)
}



Issue.complaint.new <- process_text(Issue.complaint)
State.complaint.new <- process_text(State.complaint)

State.complaint.new






library(magrittr)
Issue.complaint.new %<>% rename(doc_id = Issue, text = comments) %>% data.frame()
State.complaint.new %<>% rename(doc_id = State, text = comments) %>% data.frame()




## text-quantification 


State.complaint.new %>% c
library(tm)


text.df <- data.frame()
list <-c(Issue.complaint$Issue %>% unique)


text.df$doc_id <- list
text.df$text <- Issue.complaint.new$text




State.df.dtm <-State.complaint.new %>% 
  DataframeSource %>%
  Corpus %>% 
  DocumentTermMatrix(control = list(wordLenths = c(1,Inf),
                                    weighting = function(x)
                                      weightTfIdf(x,normalize =TRUE)))

State.df.dtm




state.cos <- State.df.dtm %>% 
  as.matrix %>%
  proxy::dist(method = "cosine") %>% 
  as.matrix
state.cos[1:5,]
  



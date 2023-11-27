complaint <- read.csv("complaints.csv")
library(dplyr)
library(textstem)
library(tm)
library(stringr)
View(complaint)


# group and paste comments 

Sub.complaint <- complaint %>% filter(Consumer.complaint.narrative != "") %>%
  group_by(Sub.product) %>%
  summarise(comments = paste(Consumer.complaint.narrative, collapse = " "))

Issue.complaint <- complaint %>% filter(Consumer.complaint.narrative != "") %>%
  group_by(Issue) %>%
  summarise(comments = paste(Consumer.complaint.narrative, collapse = " "))

subIs.complaint <- complaint %>% filter(Consumer.complaint.narrative != "") %>%
  group_by(Sub.issue) %>%
  summarise(comments = paste(Consumer.complaint.narrative, collapse = " "))

State.complaint <- complaint %>% filter(Consumer.complaint.narrative != "") %>%
  group_by(State) %>%
  summarise(comments = paste(Consumer.complaint.narrative, collapse = " ")) 

## pre-processing

process_text <- function(df) {
  df$comments <- df$comments %>% 
    str_remove_all("[:punct:]") %>% 
    str_remove_all("[:digit:]") %>%
    str_remove_all("\\n|\\$|X{2,}") %>%
    str_replace_all("[:blank:]{2,}", " ") %>%
    tolower() %>%
    lemmatize_words()
  return(df)
}

Sub.complaint <- process_text(Sub.complaint)
Issue.complaint < -process_text(Issue.complaint)
subIs.complaint <- process_text(subIs.complaint)
State.complaint <- process_text(State.complaint)









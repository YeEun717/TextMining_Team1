complaint <- read.csv("../Data/complaints-2023-11-08_04_47.csv")
library(dplyr)
library(textstem)
library(tm)
library(stringr)
View(complaint)


# group and paste comments 
Issue.complaint <- complaint %>% filter(Consumer.complaint.narrative != "") %>%
  group_by(Issue) %>%
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

Sub.complaint %>% head
Issue.complaint %>% head
subIs.complaint %>% head
State.complaint %>% head


###############################################################
rm(complaint)
### 이슈별 개수
complaint %>% group_by(Issue) %>% 
  summarise(count = n()) %>%  arrange(desc(count)) %>%
  head(10) %>% 
  select(count) %>% 
  sum()

### 날짜 데이터로 변환
library(lubridate)
# 날짜 데이터를 날짜 타입으로 변환
complaint$Date <- mdy(complaint$Date.received)

# day, month, year 변수 생성
complaint$day <- day(complaint$Date)
complaint$month <- month(complaint$Date)
complaint$year <- year(complaint$Date)

# 결과 확인
head(complaint)

### 년도별 데이터 개수 시각화
library(ggplot2)
# 년도별 데이터 개수 계산
yearly_counts <- complaint %>%
  group_by(year) %>%
  summarise(count = n())

# 년도별 데이터 개수 시각화
ggplot(yearly_counts, aes(x = year, y = count)) +
  geom_bar(stat = "identity", fill = "blue") +
  theme_minimal() +
  labs(title = "년도별 데이터 개수", x = "년도", y = "데이터 개수")


### state
complaint %>% group_by(State) %>% 
  summarise(count = n()) %>%  arrange(desc(count))


### state(주)별 text mining 시작
complaint %>% names
complaint %>%
  select(Date, State, Consumer.complaint.narrative) %>% 
  head

# 각 문장을 단어로 분할하여 리스트 형태로 저장
complaint$words <- str_split(complaint$Consumer.complaint.narrative, " ")

# 1st Preprocessing: 패턴에 맞는 문자열 제거 (예: XXXX, XXX/XX/XXX, \n)
complaint$words_cleaned_1 <- lapply(complaint$words, function(word_list) {
  words_list <- gsub("X{2,}", "", word_list) # "X"가 2번 이상 연속된 경우 제거
  word_list <- gsub("X+/[X/]+", "", word_list) # "X/" 패턴을 포함하는 문자열 제거 
  word_lsit <- gsub("\n", "", word_list) # "\n" 제거 
  word_list <- word_list[word_list != ""] # 빈 문자열 제거
  return(words_list)
})

# 2nd Preprocessing: remove punctuation, tolower, remove number, remove stopwords, lemmatization
library(stringr)
library(tm)
library(textstem)

# 불용어 리스트를 불러옵니다.
data("stop_words", package = "tidytext")

# 전처리 함수를 정의합니다.
preprocess_text <- function(text) {
  text <- tolower(text)  # 소문자 변환
  text <- gsub("[[:punct:]]", "", text) # 구두점 제거
  text <- gsub("[0-9]+", "", text) # 숫자 제거
  words <- str_split(text, "\\s")
  words <- sapply(words, function(word) word[!word %in% stop_words$word]) # 불용어 제거
  text <- paste(unlist(words), collapse = " ")
  text <- lemmatize_strings(text) # 표제어 추출
  text <- str_trim(text)
  return(text)
}

# 전처리를 적용합니다.
complaint$words_cleaned_2 <- sapply(complaint$words_cleaned_1, preprocess_text)

complaint %>% names()
### 필요없는 컬럼들 제거
complaints <- complaint %>% 
  select(Date, State, Consumer.complaint.narrative, words_cleaned_2)
View(complaints)

### 전처리 완료 


### 
complaints %>% head(2)
complaints %>% names




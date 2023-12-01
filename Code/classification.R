complaint.data <- read.csv("complaints_add_label.csv") # Read data
View(complaint.data) # Check the data

complaint <- complaint.data %>% filter(Complaint != "") %>% # Filter out blank complaints 
  select(Label, Complaint) # Select label and complaint columns

View(complaint) # Check the data
nrow(complaint) # Check the number of data

colnames(complaint) <- c("type", "text") # Rename columns

library(dplyr)
library(tibble)

complaint <- complaint %>% 
  select(text, type) %>% # Column order switch
  add_column(doc_id=1:nrow(.), .before=1) %>% # Add doc_id column
  mutate(text=iconv(text, to="ascii", sub="")) # Change to ascii code, and delete unchanged words

library(tm)

docs <- VCorpus(DataframeSource(complaint)) # Make a corpus
docs # Check the corpus

lapply(docs, content)[c(13, 16, 20)] # Check the content
meta(docs)$type[c(13, 16, 20)] # Check the meta variable

myRemove <- content_transformer(function(x, pattern) # Function that removes the input pattern
{return(gsub(pattern, "", x))})

docs <- tm_map(docs, myRemove, "X+") # Remove 'XXXX'

library(stopwords)

## !!!Takes some time to run this code!!!
docs <- docs %>% 
  tm_map(removePunctuation) %>% # Remove punctuation
  tm_map(removeNumbers) %>% # Remove numbers
  tm_map(stripWhitespace) %>% # Strip spaces
  tm_map(content_transformer(lemmatize_strings)) %>% # Lemmatize
  tm_map(content_transformer(tolower)) %>% # To lower
  tm_map(content_transformer(trimws)) %>% # Remove margin spaces
  tm_map(content_transformer(removeWords), 
         stopwords('en', source='stopwords-iso')) %>% # Remove stopwords
  tm_map(content_transformer(str_squish)) # Every spaces into single space

## doc_save <- docs # Save docs just in case

lapply(docs, content)[c(13, 16, 20)] # Check the content

dtm <- DocumentTermMatrix(docs) # Make DTM
inspect(dtm) # Check the DTM -> There are too many words included

dtm <- DocumentTermMatrix(
  docs,
  control=list(wordLengths=c(3,Inf), # Limit the word length
               bounds=list(global=c(13,26159)))) # Lower and upper limits for the number of documents that appear
                                                 # Except for words that appear in less than 0.05% of all documents and words that appear in more than 95% of all documents                                
inspect(dtm) # Check the DTM

termfreq <- colSums(as.matrix(dtm)) # Frequency of each word
termfreq[head(order(termfreq, decreasing = TRUE), 10)] # Top 10 words
termfreq[tail(order(termfreq, decreasing = TRUE), 10)] # Bottom 10 words

findFreqTerms(dtm, # Words with high frequencies
              lowfreq = 500) # Minimum frequency number

# Check which words are used with other words that appear frequently
findAssocs(dtm, c("bank", "write"), c(0.25, 0.25)) # Check the high frequency words with correlation

# Wordcloud
library(wordcloud)
library(RColorBrewer)
set.seed(123) 
#dev.new(height=500, width=500)
wordcloud(words=names(termfreq), freq=termfreq,
          scale=c(4, 0.5), # Range of word sizes
          min.freq = 30, # Minimum word frequency
          max.words = 200, # Maximum number of words
          rot.per = 0, # Rotation percentage (0 for horizontal)
          random.order = FALSE, # Order words by frequency (not random)
          random.color = FALSE, # Use consistent color for each word
          colors = brewer.pal(6, "Set2") # Color palette from Set2 with 6 colors
)

# To create a comparison cloud, you need a matrix where rows represent words, columns represent categories,
# and cell values indicate the frequency of occurrence.
comparision.label <- as.matrix(dtm)
rownames(comparision.label) <- complaint$type # Set row names to words
comparision.label[1:5, 1:5] # Displaying a subset of the matrix for illustration

# Calculate the sum of each row
comparision.label <- rowsum(comparision.label, 
                            group = rownames(comparision.label)) # Vector specifying the categories to include
comparision.label[, 1:10] # Displaying a subset of the matrix where each cell represents the frequency of each word

set.seed(123)
#dev.new(height=50, width=50)
# Create a comparison cloud by transposing the matrix (rows become words, columns become categories)
comparison.cloud(t(comparision.label), 
                 title.size = 0.7, # Title size
                 colors = brewer.pal(3, "Set2"), # Color palette from Set2 with 3 colors
                 title.colors = brewer.pal(3, "Set2"), # Title color palette
                 random.order=FALSE, # Random order False
                 title.bg.colors = "wheat", # Title background color
                 rot.per = 0, # Rotation percentage (0 for horizontal)
                 scale = c(3, 0.5),  # Scale for word sizes
                 max.words = 300) # Maximum number of words

### Creating a Predictive Model Using Naive Bayes
# Predictive variable: Words extracted from documents
# Label variable: Labels of documents
# To perform Naive Bayes analysis:
#   Predictive variable should be in matrix or data frame format
#   Label variable should be in factor format
# Predictive variable is stored in dtm, as seen in inspect(dtm)
# Label variable: complaint$type

# 70:30 = train:test
set.seed(123)
train <- sample(nrow(complaint), 0.7 * nrow(complaint)) # Splitting the data into training and testing tets
y.train <- complaint[train, ]$type # Training response variable
y.test <- complaint[-train, ]$type # Testing response variable

table(y.train) # Number of labels in train data
table(y.test) # Number of labels in test data

# Checking if the split datasets reflect the characteristics of the entire dataset
prop.table(table(complaint$type)) # Entire dataset
prop.table(table(y.train)) # Train dataset
prop.table(table(y.test)) # Test dataset
# Both reflect the proportions of the entire dataset

# Naive Bayes works based on categorical predictive variables
# Currently, the predictive variable is the frequency of word occurrences,
# so the predictive variable needs to be converted to categorical format (1 if the word appears, 0 otherwise)
toFactor <- function(x) {
  x <- ifelse(x > 0, 1, 0) # If the cell value is greater than 0, set it to 1; otherwise, set it to 0
  x <- factor(x, level = c(0, 1), labels = c("no", "yes")) # Convert 0 to "no" and 1 to "yes"
  return(x)
}

## !!!Takes some time to run this code!!!
complaint.dtm <- apply(dtm, MARGIN = 2, toFactor) # Execute the function in the column direction
str(complaint.dtm) # Display the structure of the resulting matrix

complaint.dtm[10:20, 10:20] # Check the matrix

x.train <- complaint.dtm[train, ] # Training set for predictive variable
x.test <- complaint.dtm[-train, ] # Testing set for predictive variable

library(e1071)
complaint.nb <- naiveBayes(x=as.matrix(x.train), y=y.train) # Train the model
## !!!Takes some time to run this code!!!
complaint.nb.pred <- predict(complaint.nb, newdata=as.matrix(x.test)) # Predict the test data
head(complaint.nb.pred) # Check the predicted result

# Model evaluation
evaluate_model <- function(actual, predicted) {
  # Calculate the evaluation metrics 
  confusion_mat <- table(Actual = actual, Predicted = predicted, dnn=c("Actual", "Predicted"))
  accuracy <- sum(diag(confusion_mat)) / sum(confusion_mat) # Accuracy
  precision <- confusion_mat[2, 2] / sum(confusion_mat[, 2]) # Precision
  recall <- confusion_mat[2, 2] / sum(confusion_mat[2, ]) # Recall
  f1_score <- 2 * (precision * recall) / (precision + recall) # F1 Score
  
  # Print the result
  cat("Accuracy:", accuracy, "\n")
  cat("Precision:", precision, "\n")
  cat("Recall:", recall, "\n")
  cat("F1 Score:", f1_score, "\n")
}

table(y.test, complaint.nb.pred, dnn=c("Actual", "Predicted")) # Confusion matrix
evaluate_model(y.test, complaint.nb.pred) # Print the calculated evaluation metrics

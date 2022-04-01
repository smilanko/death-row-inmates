library("dplyr")
library("rapport")
library("tm")
library("wordcloud")
library("RColorBrewer")
library("wordcloud2")
library("syuzhet")
library("lubridate")
library("ggplot2")
library("scales")
library("reshape2")

source("prepareInmateDocument.r")

# load the inmates
Inmates = prepareInmateDocument()

# do some transformations on the text
documents <- Corpus(VectorSource(Inmates$last_statement))
documents = tm_map(documents, content_transformer(tolower))
documents = tm_map(documents, removePunctuation)
documents = tm_map(documents, removeWords, stopwords("english"))
documents <- tm_map(documents, stripWhitespace)

# create a document term matrix
dtm <- DocumentTermMatrix(documents)
spokenWords = rowSums(as.matrix(dtm))
set.seed(3) # sed for reproducability
# Histogram of the number of words in the last statement
hist(spokenWords, prob = TRUE)
lines(density(spokenWords), col = 4, lwd = 2)

# prepare sentiments
tweets <- iconv(documents)
s <- get_nrc_sentiment(tweets[1])
barplot(colSums(s), las = 2, col = rainbow(10), ylab = 'Count', main = 'Sentiment Scores Last Statements')


# create a matrix for a word cloud chart
matrix = as.matrix(TermDocumentMatrix(documents))
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)

# for reproducibility
set.seed(1234)

# create a bar chart
w <- rowSums(matrix)
w <- subset(w, w>=25)
barplot(w, las = 2, col = rainbow(50))

# create a word cloud
wordcloud(words = names(w), freq = w, max.words = 150, random.order = F, min.freq = 5, colors = brewer.pal(8, 'Dark2'), scale = c(5, 0.3), rot.per = 0.7)

# create a word cloud 2
w <- data.frame(names(w), w)
colnames(w) <- c('word', 'freq')
wordcloud2(w, size = 0.7, shape = 'triangle', rotateRatio = 0.5, minSize = 1)

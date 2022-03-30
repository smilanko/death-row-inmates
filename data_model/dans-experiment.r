library("dplyr")
library("rapport")
library("tm")
library("wordcloud")
library("RColorBrewer")
library("wordcloud2")
source("prepareInmateDocument.r")

# load the inmates
Inmates = prepareInmateDocument()

# do some transformations on the text
documents <- Corpus(VectorSource(Inmates$last_statement))
documents = tm_map(documents, content_transformer(tolower))
documents = tm_map(documents, removePunctuation)
documents = tm_map(documents, removeWords, stopwords("english"))

# create a matrix for a word cloud chart
words <- sort(rowSums(as.matrix(TermDocumentMatrix(documents) ) ),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)

set.seed(1234) # for reproducibility 
wordcloud(words = df$word, freq = df$freq, min.freq = 1, max.words=100, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))

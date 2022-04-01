library("dplyr")
library("rapport")
library("tm")
source("prepareInmateDocument.r")

# load the inmates
Inmates = prepareInmateDocument()

# do some transformations on the text
documents <- Corpus(VectorSource(Inmates$last_statement))
documents = tm_map(documents, content_transformer(tolower))
documents = tm_map(documents, removePunctuation)
documents = tm_map(documents, removeWords, stopwords("english"))
documents <- tm_map(documents, stripWhitespace)

# set the seed for reproducability
set.seed(3)

# create a document term matrix
dtm <- DocumentTermMatrix(documents)
rowTotals <- apply(dtm , 1, sum) # Find the sum of words in each Document
dtm <- dtm[rowTotals> 2, ] # get documents with at least 2 ter,s
spokenWords = rowSums(as.matrix(dtm))

# how long is the inmate in jail
days_in_jail = difftime(as.Date(Inmates$date_received, format = "%m/%d/%y"), as.Date(Inmates$execution_date, format = "%m/%d/%y"), units = "days")
hist(as.numeric(days_in_jail), prob = TRUE)


# qq norm plot
qqnorm(log(spokenWords), pch = 1, frame = FALSE)
qqline(log(spokenWords), col = "steelblue", lwd = 2)

# Log transform of the number of words in the last statement
hist(log(spokenWords), prob = TRUE)
lines(density(log(spokenWords)), col = 4, lwd = 2)


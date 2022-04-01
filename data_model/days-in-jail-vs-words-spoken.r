library("dplyr")
library("rapport")
library("tm")
library("data.table") 
source("prepareInmateDocument.r")

# set the seed for reproducability
set.seed(3)

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
rowTotals <- apply(dtm , 1, sum) # Find the sum of words in each Document
document_count_filter = rowTotals> 2 # get documents with at least 2 words
dtm <- dtm[document_count_filter, ] 
spokenWords = as.data.frame(log(rowSums(as.matrix(dtm))))[,1]

# how long is the inmate in jail
days_in_jail = difftime(as.Date(Inmates$date_received, format = "%m/%d/%Y"), as.Date(Inmates$execution_date, format = "%m/%d/%Y"), units = "days")
# apply the same filter to remove inmates with little words
days_in_jail = days_in_jail[document_count_filter]

# we do not want data that has no days in fail over over 19 years
jail_sentance_filter = which(is.na(days_in_jail) | (days_in_jail < -7000))
spokenWords = spokenWords[-jail_sentance_filter]
# qq norm plot of spokenWords
par(mfrow=c(2,2))
qqnorm(spokenWords, pch = 1, frame = FALSE)
qqline(spokenWords, col = "steelblue", lwd = 2)
hist(spokenWords, prob = TRUE)
lines(density(spokenWords), col = 4, lwd = 2)

days_in_jail = as.numeric(days_in_jail[-jail_sentance_filter])
qqnorm(days_in_jail, pch = 1, frame = FALSE)
qqline(days_in_jail, col = "steelblue", lwd = 2)
hist(days_in_jail, prob = TRUE)
lines(density(days_in_jail), col = 4, lwd = 2)

m1 = lm(spokenWords~days_in_jail)
par(mfrow=c(1,4))
plot(m1)
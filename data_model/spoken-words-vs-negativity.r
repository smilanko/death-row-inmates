library("data.table") 
library("psych")
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
library("DAAG")
library("datasets")
library("caret")
library("pROC")
library("nnet")
library("e1071")
source("prepareInmateDocument.r")

# silence the warning, since this script is verified
options(warn=-1)

extractAnswer <- function(Inmates) {
	# check if we have any empty statements
	empty_last_statement_filter = which(Inmates$last_statement == "")
	Inmates <- Inmates[-empty_last_statement_filter, ]

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
	Inmates <- Inmates[document_count_filter, ]
	spokenWords = as.data.frame(log(rowSums(as.matrix(dtm))))[,1]
	
	set.seed(3)
	s = get_nrc_sentiment(Inmates$last_statement)
	positive_filter = which(s$negative < s$positive)
	sentiments <- rep(0, length(spokenWords))
	sentiments[positive_filter] = 1

	mydata <- data.frame(spokenWords, sentiments)
	mydata$sentiments <- factor(mydata$sentiments)
	print(table(mydata$sentiments))

	ind <- sample(2, nrow(mydata), replace = T, prob = c(0.5, 0.5))
	train <- mydata[ind == 1,]
	test <- mydata[ind == 2,]
	m <- glm(sentiments~., data = train, family = 'binomial' )

	setEPS()
	postscript("plots/spoken-words-vs-negativity/spoken_words_negativity_corr.eps",width=5,height=4)
	plot_colors = c("#de536b", "black")
	plot(spokenWords, pch = 19, col = plot_colors, xlab="Last Statement Id", ylab="log(Spoken Words)")
	legend("topright",c("Negative Sentiment", "Positive Sentiment"),cex=.8,col= plot_colors,pch=19)
	dev.off()

	setEPS()
	postscript("plots/spoken-words-vs-negativity/roc_spoken_words_negativity.eps",width=5,height=4)
	p1 <- predict(m, train, type = 'response')
	print(table(Predicted = ifelse(p1 > 0.71, 0, 1), Actual = train$sentiments))
	r <- multiclass.roc(train$sentiments, p1, percent = TRUE)
	roc <- r[['rocs']]
	r1 <- roc[[1]]
	plot.roc(r1, col = 'red', lwd = 5, print.auc = T, auc.polygon = T, max.auc.polygon = T, auc.polygon.col = 'lightblue', print.thres = T)
	auc(r1)
	coords(r1, "best", ret="threshold", transpose = FALSE)
	dev.off()

}

# load the inmates
Inmates = prepareInmateDocument()
extractAnswer(Inmates)
print("we've answered the question!! take a look at the model! 🤞")
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
library("tidytext")
library("tidyr")
source("prepareInmateDocument.r")

# silence the warning, since this script is verified
options(warn=-1)

classifySentimentViaDoc <- function(dtm, library) {
	s = tidy(dtm) %>%
	  inner_join(get_sentiments(library), by = c(term = "word")) %>%
	  count(document, sentiment, wt = count) %>%
	  spread(sentiment, n, fill = 0) %>%
	  mutate(sentiment = positive - negative) %>%
	  arrange(sentiment)
	return(which(s$negative < s$positive))
}

calculateAccuracy <- function(matrix) {
	TN = matrix[1]
	FN = matrix[2]
	FP = matrix[3]
	TP = matrix[4]
	print(paste("Sensitivity:", ((TP) / (TP+FN)), "Specificity:", ((TN) / (TN+FP)),  "Accuracy:", ((TP + TN) / (TP + FP + TN + FN))))
}

classifySentiment <- function(Inmates) {
	s = get_nrc_sentiment(Inmates$last_statement)
	return(which(s$negative < s$positive))
}

extractAnswer <- function(Inmates) {
	# check if we have any empty statements
	empty_last_statement_filter = which(Inmates$last_statement == "")
	Inmates <- Inmates[-empty_last_statement_filter, ]

	# do some transformations on the text
	documents <- Corpus(VectorSource(Inmates$last_statement))
	documents = tm_map(documents, content_transformer(tolower))
	documents = tm_map(documents, removePunctuation)
	documents = tm_map(documents, removeWords, c(stopwords("english"),"spoken","verbal","written","mumble","recite","garble","unintelligible", "english", "spanish", "french", "vietnamese", "translate", "irish", "statement", "mouthed", "listed", "ahh"))
	documents <- tm_map(documents, stripWhitespace)

	# create a document term matrix
	dtm <- DocumentTermMatrix(documents)
	rowTotals <- apply(dtm , 1, sum) # Find the sum of words in each Document
	document_count_filter = rowTotals> 2 # get documents with at least 2 words
	dtm <- dtm[document_count_filter, ]
	Inmates <- Inmates[document_count_filter, ]
	spokenWords = as.data.frame(log(rowSums(as.matrix(dtm))))[,1]
	
	set.seed(3)
	# positive_filter = classifySentiment(Inmates)
	positive_filter = classifySentimentViaDoc(dtm, "nrc") # c("bing", "loughran", "nrc")
	sentiments <- rep(0, length(spokenWords))
	sentiments[positive_filter] = 1

	mydata <- data.frame(spokenWords, sentiments)
	mydata$sentiments <- factor(mydata$sentiments)
	print(table(mydata$sentiments))

	ind <- sample(2, nrow(mydata), replace = T, prob = c(0.7, 0.3))
	train <- mydata[ind == 1,]
	test <- mydata[ind == 2,]
	m <- glm(sentiments~., data = train, family = 'binomial' )
	print(summary(m))

	setEPS()
	postscript("plots/spoken-words-vs-negativity/spoken_words_negativity_corr.eps",width=5,height=4)
	plot_data <- as.data.frame(cbind(spokenWords, sentiments))
	plot(jitter(plot_data$sentiments), plot_data$spokenWords, pch = 16, col = "#de536b", xaxt="n", ylab="log(Spoken Words)", xlab="Sentiment")
	axis(1, xaxp=c(0, 1, 1), las=1)
	dev.off()

	setEPS()
	postscript("plots/spoken-words-vs-negativity/roc_spoken_words_negativity.eps",width=10,height=4)
	p1 <- predict(m, train, type = 'response')
	p2 <- predict(m, test, type = 'response')

	par(mfrow=c(1,2))
	r_one <- multiclass.roc(train$sentiments, p1, percent = TRUE)
	roc_one <- r_one[['rocs']]
	r1 <- roc_one[[1]]
	plot.roc(r1, main = 'ROC Curve for Train Data', col = 'red', lwd = 5, print.auc = T, auc.polygon = T, max.auc.polygon = T, auc.polygon.col = 'lightblue', print.thres = T)
	auc(r1)
	coords(r1, "best", ret="threshold", transpose = FALSE)

	r_two <- multiclass.roc(test$sentiments, p2, percent = TRUE)
	roc_two <- r_two[['rocs']]
	r2 <- roc_two[[1]]
	plot.roc(r2, main = 'ROC Curve for Test Data', col = 'red', lwd = 5, print.auc = T, auc.polygon = T, max.auc.polygon = T, auc.polygon.col = 'lightblue', print.thres = T)
	auc(r2)
	coords(r2, "best", ret="threshold", transpose = FALSE)
	dev.off()

	pval = 0.706
	print("### Test confusion matrix")
	print(table(Predicted = ifelse(p2 > pval, 1, 0), Actual = test$sentiments))
	calculateAccuracy(table(Predicted = ifelse(p2 > pval, 1, 0), Actual = test$sentiments))
	print("### Train confusion matrix")
	print(table(Predicted = ifelse(p1 > pval, 1,0), Actual = train$sentiments))
	calculateAccuracy(table(Predicted = ifelse(p1 > pval, 1,0), Actual = train$sentiments))
}

# load the inmates
Inmates = prepareInmateDocument()
extractAnswer(Inmates)
print("we've answered the question!! take a look at the model! 🤞")
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

extractAnswer <- function(Inmates) {
	# check if we have any empty statements
	empty_last_statement_filter = which(Inmates$last_statement == "")
	Inmates <- Inmates[-empty_last_statement_filter, ]

	# how long is the inmate in jail
	days_in_jail = difftime(as.Date(Inmates$date_received, format = "%m/%d/%Y"), as.Date(Inmates$execution_date, format = "%m/%d/%Y"), units = "days")
	
	# we do not want data that has no days in jail over 19 years
	jail_sentance_filter = which(is.na(days_in_jail) | (days_in_jail < -7000))
	statements = Inmates$last_statement[-jail_sentance_filter]
	days_in_jail = as.numeric(days_in_jail[-jail_sentance_filter])

	set.seed(3)
	s = get_nrc_sentiment(statements)
	positive_filter = which(s$negative < s$positive)
	sentiments <- rep(0, length(days_in_jail))
	sentiments[positive_filter] = 1

	mydata <- data.frame(days_in_jail, sentiments)
	mydata$sentiments <- factor(mydata$sentiments)
	
	ind <- sample(2, nrow(mydata), replace = T, prob = c(0.99, 0.01))
	train <- mydata[ind == 1,]
	test <- mydata[ind == 2,]
	m <- glm(sentiments~., data = train, family = 'binomial' )

	setEPS()
	postscript("plots/days-on-death-row-vs-negativity/last_stmtvs_negativity_corr.eps",width=5,height=4)
	plot_colors = c("#de536b", "black")
	plot(days_in_jail, pch = 19, col = plot_colors, ylim=c(-9000,0), xlab="Last Statement Id", ylab="Days on Death Row")
	legend("bottom",c("Negative Sentiment", "Positive Sentiment"),cex=.8,col= plot_colors,pch=19)
	dev.off()

	setEPS()
	postscript("plots/days-on-death-row-vs-negativity/roc_negativity.eps",width=5,height=4)
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

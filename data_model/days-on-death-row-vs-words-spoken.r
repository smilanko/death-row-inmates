library("dplyr")
library("rapport")
library("tm")
library("data.table") 
library("psych")
source("prepareInmateDocument.r")

# silence the warning, since this script is verified
options(warn=-1)

extractAnswer <- function(Inmates) {
	# set the seed for reproducability
	set.seed(3)

	# check if we have any empty statements
	empty_last_statement_filter = which(Inmates$last_statement == "")
	Inmates <- Inmates[-empty_last_statement_filter, ]

	# do some transformations on the text
	documents <- Corpus(VectorSource(Inmates$last_statement))
	documents = tm_map(documents, content_transformer(tolower))
	documents = tm_map(documents, removePunctuation)
	documents = tm_map(documents, removeWords, c(stopwords("english"),"spoken","verbal","written", "mumbled"))
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

	# we do not want data that has no days in jail over 19 years
	jail_sentance_filter = which(is.na(days_in_jail) | (days_in_jail < -7000))
	spokenWords = spokenWords[-jail_sentance_filter]
	days_in_jail = as.numeric(days_in_jail[-jail_sentance_filter])

	m1 = lm(spokenWords~days_in_jail)
	setEPS()
	postscript("plots/days-on-death-row-vs-words-spoken/linear_model.eps",width=12.5,height=4)
	par(mfrow=c(1,4))
	plot(m1)
	dev.off()
	print(summary(m1))

	# coorelation plot
	setEPS()
	postscript("plots/days-on-death-row-vs-words-spoken/variable_correlation.eps")	
    pairs.panels(data.frame(days_in_jail, spokenWords), labels =c('Days on Death Row', 'log(Spoken Words)'), smooth = TRUE, scale = FALSE, density = TRUE, ellipses = TRUE,  method = "pearson",  pch = 21, lm = FALSE, cor = TRUE, jiggle = FALSE, factor = 2,  hist.col = 4, stars = TRUE,  ci = TRUE)
    dev.off()
}

# load the inmates
Inmates = prepareInmateDocument()
extractAnswer(Inmates)
print("we've answered the question!! take a look at the model! 🤞")
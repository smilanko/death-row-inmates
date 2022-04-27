library("dplyr")
library("rapport")
library("tm")
library("data.table") 
library("psych")
library("stringr")

setwd('~/Documents/GitHub/death-row-inmates/data_model')
source("prepareInmateDocument.r")

# silence the warning, since this script is verified
options(warn=-1)

extractAnswer <- function(Inmates) {
	# set the seed for reproducability
	set.seed(3)

	# check if we have any empty statements
	empty_last_statement_filter = which(Inmates$last_statement == "")
	Inmates <- Inmates[-empty_last_statement_filter, ]
	
	Inmates$ed <- Inmates$education_level
	# remove ged in ed with 0
	Inmates$ed <- str_remove(Inmates$ed, "ged$") 

	# convert to numeric
	Inmates$ed <- as.numeric(Inmates$ed)
	
	# Replacing NA’s with median of the remaining values
	Inmates$ed[is.na(Inmates$ed)] <- median(Inmates$ed,na.rm=TRUE)

	# do some transformations on the text
	documents <- Corpus(VectorSource(Inmates$last_statement))
	documents = tm_map(documents, content_transformer(tolower))
	documents = tm_map(documents, removePunctuation)
	documents = tm_map(documents, removeNumbers)
	documents = tm_map(documents, removeWords, c(stopwords("english"),"spoken","verbal","written","mumble","recite","garble","unintelligible", "english", "spanish", "french", "vietnamese", "translate", "irish", "statement", "mouthed", "listed", "ahh"))
	documents <- tm_map(documents, stripWhitespace)

	# create a document term matrix
	dtm <- DocumentTermMatrix(documents)
	rowTotals <- apply(dtm , 1, sum) # Find the sum of words in each Document
	#document_count_filter = rowTotals> 2 # get documents with at least 2 words
	#dtm <- dtm[document_count_filter, ] 
	spokenWords = as.data.frame(log(rowSums(as.matrix(dtm))))[,1]
	print(paste("mean:", mean(spokenWords), "median:", median(spokenWords)))

	# we have the inmate's education level.
	ed_level = Inmates$ed
	# apply the same filter to remove inmates with little words
	#ed_level = ed_level[document_count_filter]

	m1 = lm(spokenWords ~ ed_level)
	setEPS()
	postscript("./plots/ed-level-vs-words-spoken/ed_linear_model.eps",width=12.5,height=4)
	par(mfrow=c(1,4))
	plot(m1)
	dev.off()
	print(summary(m1))

	# coorelation plot
	setEPS()
	postscript("plots/ed-level-vs-words-spoken/ed_variable_correlation.eps")	
    pairs.panels(data.frame(ed_level, spokenWords), labels =c('Education Level', 'log(Spoken Words)'), smooth = TRUE, scale = FALSE, density = TRUE, ellipses = TRUE,  method = "pearson",  pch = 21, lm = FALSE, cor = TRUE, jiggle = FALSE, factor = 2,  hist.col = 4, stars = TRUE,  ci = TRUE)
    dev.off()
}

# load the inmates
Inmates = prepareInmateDocument()
extractAnswer(Inmates)
print("we've answered the question!! take a look at the model! 🤞")

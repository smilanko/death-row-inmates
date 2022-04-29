library("dplyr")
library("rapport")
library("tm")
library("data.table") 
library("psych")
library("stringr")
library(tm)
library(qdap)
library(pbapply)
library(lda)
library(LDAvis)
library(treemap)

setwd('~/Documents/GitHub/death-row-inmates/data_model')
source("prepareInmateDocument.r")

# silence the warning, since this script is verified
options(warn=-1)

# Each term is assigned to a topic, so this will tally for a document & assign the most frequent as membership
docAssignment<-function(x){
  x <- table(x)
  x <- as.matrix(x)
  x <- t(x)
  x <-max.col(x)
}

# load the inmates
Inmates = prepareInmateDocument()

# set the seed for reproducability
set.seed(3)

# check if we have any empty statements
empty_last_statement_filter = which(Inmates$last_statement == "")
Inmates <- Inmates[-empty_last_statement_filter, ]
	
# do some transformations on the text
documents <- Corpus(VectorSource(Inmates$last_statement))
documents = tm_map(documents, content_transformer(tolower))
documents = tm_map(documents, removePunctuation)
documents = tm_map(documents, removeNumbers)
documents = tm_map(documents, removeWords, c(stopwords("english"),"spoken","verbal","written","mumble","recite","garble","unintelligible", "english", "spanish", "french", "vietnamese", "translate", "irish", "statement", "mouthed", "listed", "ahh"))
documents <- tm_map(documents, stripWhitespace)

# Instead of DTM/TDM, just clean the vector w/old functions
txt <- documents

# Extract the clean text
#txt <- unlist(pblapply(txt, content))

# Remove any blanks, happens sometimes w/tweets bc small length & stopwords
#txt <- pblapply(txt, blankRemoval)

# Lexicalize
txtLex <- lexicalize(txt)

# Examine the vocab or key and value pairing between key ()
head(txtLex$vocab) # remember #6
length(txtLex$vocab) #9k+ unique words among all articles, each 
head(txtLex$documents[[1]]) #look at [,22]
#head(txtLex$documents[[20]])

# Corpus stats
txtWordCount  <- word.counts(txtLex$documents, txtLex$vocab)
txtDocLength  <- document.lengths(txtLex$documents)

# LDA Topic Modeling
# suppose you have a bag of dice (documents)
# alpha - there is a distribution of the probabilities of how similar they are to each other, are dice similar in size/shape/weight?
# eta   - there is also a distribution of probabilities for the number of topics inside a single document, are dice 6 sided or other?
# 
k       <- 5 # number of topics
numIter <- 25 # number of reviews, it performs random word sampling each time
alpha   <- 0.02 #see above 
eta     <- 0.02 #see above
set.seed(1234) 
fit <- lda.collapsed.gibbs.sampler(documents      = txtLex$documents, 
                                   K              = k, 
                                   vocab          = txtLex$vocab, 
                                   num.iterations = numIter, 
                                   alpha          = alpha, 
                                   eta            = eta, 
                                   initial        = NULL, 
                                   burnin         = 0,
                                   compute.log.likelihood = TRUE)

# Prototypical Document
top.topic.documents(fit$document_sums,2) #top 2 docs (rows) * topics(cols)

# explore some of the results
fit$document_sums #topics by articles
head(t(fit$topics)) #words by topics

# LDAvis params
# normalize the article probabilites to each topic
theta <- t(pbapply(fit$document_sums + alpha, 2, function(x) x/sum(x))) # topic probabilities within a doc will sum to 1

# normalize each topic word's impact to the topic
phi  <- t(pbapply(fit$topics + eta, 1, function(x) x/sum(x)))

ldaJSON <- createJSON(phi = phi,
                      theta = theta, 
                      doc.length = txtDocLength, 
                      vocab = txtLex$vocab, 
                      term.frequency = as.vector(txtWordCount))
#install.packages('servr')
serVis(ldaJSON)
serVis(ldaJSON, out.dir = '~/Documents/GitHub/death-row-inmates/data_model/plots', open.browser = interactive(), as.gist = FALSE)

# End

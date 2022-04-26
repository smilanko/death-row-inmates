library("dplyr")
library("rapport")
library("tm")
library("wordcloud2")
library("extrafont")
library("webshot")
source("prepareInmateDocument.r")

createWordCloudForLastStatement <- function(Inmates) {
	empty_last_statement_filter = which(Inmates$last_statement == "")
	Inmates <- Inmates[-empty_last_statement_filter, ]

	# do some transformations on the text
	documents <- Corpus(VectorSource(Inmates$last_statement))
	documents = tm_map(documents, content_transformer(tolower))
	documents = tm_map(documents, removePunctuation)
	documents = tm_map(documents, removeWords, c(stopwords("english"),"spoken","verbal","written","mumble","recite","garble","unintelligible", "english", "spanish", "french", "vietnamese", "translate", "irish", "statement", "mouthed", "listed", "ahh"))
	documents <- tm_map(documents, stripWhitespace)

	# create a matrix for a word cloud chart
	tdm = TermDocumentMatrix(documents) %>%  as.matrix()
	words = sort(rowSums(tdm), decreasing = TRUE)
	df = data.frame(word = names(words), freq = words)

	webshot::install_phantomjs()
	set.seed(1223)
	uxc.colors = c("#e63946", "#f6bd60", "#0a9396", "#005f73", "#001219")
	fonts()
	hw <- wordcloud2(df,
	           color = rep_len(uxc.colors, nrow(df)),
	           fontFamily = "DM Sans",
	           size = 2.5,
	           minSize = 5,
	           rotateRatio = 0)
	htmlwidgets::saveWidget(hw,"1.html",selfcontained = F)
	webshot::webshot("1.html","plots/word_clouds/word_cloud_last_words_spoken.png",vwidth = 1992, vheight = 1744, delay =10)
}

# load the inmates
Inmates = prepareInmateDocument()
createWordCloudForLastStatement(Inmates)
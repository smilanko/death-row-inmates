library("rvest")
library("stringr")
library("tesseract")
library("imager")
library("rapport")
library("properties")

storeInmateLastStatement <- function(execution_number, stmt) {
	fileConn<-file(paste("inmate_last_statement/",execution_number, sep=""))
	writeLines(stmt, fileConn)
	close(fileConn)
}

downloadInmateLastStatement <- function() {
	# let's see what executions we already stored
	executions = list.files(path="inmate_executions/", pattern=NULL, all.files=FALSE, full.names=FALSE)
	print(paste("parsing through", length(executions), "executions to download the last statements 🥳"))

	# let's get the links for each one
	for (i in 1:length(executions)) {
		downloadedExecution = read.properties(paste("inmate_executions/", executions[i], sep=""), fields = NULL, encoding = "UTF-8")
		last_stmt_link = downloadedExecution$lastStatementLink
		execution_number = downloadedExecution$executionNumber

		# if we already fetched this file, we can move on
		if (file.exists(paste("inmate_last_statement/", execution_number, sep=""))) {
			next
		}

		inmatesComment = ''

		# some links that have no last statement take us to a default no statement page
		# if we encounter this, we can simply leave the insmates last statement blank
		if (grepl("no_last_statement.html", last_stmt_link, fixed=TRUE)) {
			storeInmateLastStatement(execution_number, inmatesComment)
			next
		}

		# get the page using the inmate_last_statement_link
		# the statement is in a uniquelly identifiable div, after the <p>Last Statement:</p>
		mainContent = read_html(last_stmt_link) %>% html_nodes(xpath = '//div[@id="content_right"]') %>% html_nodes("p")
		# R doesnt seem to like when there are parenthases in parsed strings
		# Let's use gsub to get rid of them, so that we can easily find location of the last statement
		lastStatementLocationIdx = match(1, str_detect('^LastStatement:', regex(gsub("[() ]", "", mainContent %>% html_text()), ignore_case = TRUE)))

		# it is possible that the inmate has no statement, but if they do
		# read it after <p>Last Statement:</p> ( hence the +1 )
		if (!is.na(lastStatementLocationIdx) && (lastStatementLocationIdx) < length(mainContent)) {
			# Let's check that their statement is not "No statement was made" or empty
			for (nodeIdx in (lastStatementLocationIdx + 1):length(mainContent)) {
				potentialStmt = mainContent[nodeIdx] %>% html_text()
				if (grepl("No statement was made", potentialStmt, fixed=TRUE) || gsub(" .,", "", potentialStmt, fixed = TRUE) == '') {
					break
				}
				inmatesComment = paste(inmatesComment, potentialStmt)
			}
		}

		# sometimes, the built comment is that the inmate has no comment
		# so we can remove this
		if (nchar(inmatesComment) < 10 && grepl("none", inmatesComment, ignore.case = TRUE)) { inmatesComment = '' }
		if (grepl("This inmate declined to make a last statement", inmatesComment, ignore.case = TRUE)) { inmatesComment = '' }
		storeInmateLastStatement(execution_number, inmatesComment)
	}

	print("hooray! 🎉 we downloaded last statements")
}
library("rvest")
library("stringr")
library("rapport")

fetchStoredStatement <- function(execution_number) {
	return(read.delim(paste("inmate_last_statement/",execution_number, sep=""), header = FALSE))
}

verifyLastStatement <- function() {
	# let's see what executions we already stored
	executions = list.files(path="inmate_executions/", pattern=NULL, all.files=FALSE, full.names=FALSE)
	print(paste("parsing through", length(executions), "executions to verify the last statement 🥳"))

	# let's get the links for each one
	for (i in 1:length(executions)) {
		downloadedExecution = read.properties(paste("inmate_executions/", executions[i], sep=""), fields = NULL, encoding = "UTF-8")
		lastStatementLink = downloadedExecution$lastStatementLink
		execution_number = downloadedExecution$executionNumber
		print(paste(lastStatementLink, execution_number))

		# if we already fetched this file, we can verify it
		if (!file.exists(paste("inmate_last_statement/", execution_number, sep=""))) {
			next
		}

		# it is a webpage, let's open a brower tab
		browseURL(lastStatementLink)
		print(str_replace(fetchStoredStatement(execution_number), '\n', ''))
		readline(prompt="Press [enter] to continue")
	}
	print("hooray! 🎉 we verified last statements")
}
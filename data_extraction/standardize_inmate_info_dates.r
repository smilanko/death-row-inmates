library("rvest")
library("stringr")
library("properties")
library("svDialogs")
source("fetch_inmate_info.r")

fetchStoredInmateInfo <- function(execution_number) {
	return(read.properties(paste("inmate_info/",execution_number,".properties", sep=""), fields = NULL, encoding = "UTF-8"))
}

fixDate <- function(enteredDate) {
	if (enteredDate == 'n/a') { return('n/a') }
	regex = "([^\\/]+$)"
	yearComponent = str_extract(enteredDate, regex)
	numOfcharsInYear = nchar(yearComponent)

	# no changes needed
	if (numOfcharsInYear == 4) { return(enteredDate) }
	if (numOfcharsInYear == 2) { return(paste(str_replace(enteredDate, regex, ""), "19", yearComponent, sep="")) }
	print(paste("what is this date? ", enteredDate))
	quit(-1)
	return(readDate)
}

standardizeDates <- function() {
	# let's see what executions we already stored
	executions = list.files(path="inmate_executions/", pattern=NULL, all.files=FALSE, full.names=FALSE)
	print(paste("parsing through", length(executions), "executions to verify dates 👀"))

	# let's get the links for each one
	for (i in 1:length(executions)) {
		downloadedExecution = read.properties(paste("inmate_executions/", executions[i], sep=""), fields = NULL, encoding = "UTF-8")
		execution_number = downloadedExecution$executionNumber
		print(paste("reading date for ", execution_number, "th execution"))

		# if we already fetched this file, we can verify it
		if (!file.exists(paste("inmate_info/", execution_number, ".properties", sep=""))) {
			next
		}

		# load the stored data
		currentProperties = fetchStoredInmateInfo(execution_number)
		storeInmateInfo(execution_number, fixDate(currentProperties$dob), fixDate(currentProperties$dateReceived), fixDate(currentProperties$dateOfOffsense), currentProperties$occupation, currentProperties$eyeColor, currentProperties$gender, currentProperties$hairColor, currentProperties$nativeCounty, currentProperties$nativeState, currentProperties$educationLevel)

	}
	print("hooray! 🎉 we fixed the dates")
}
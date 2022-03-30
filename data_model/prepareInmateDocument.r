library("properties")
library("rvest")
library("stringr")
library("rapport")

prepareInmateDocument <- function() {
	# let's see what executions we already stored
	executions = list.files(path="../data_extraction/inmate_executions/", pattern=NULL, all.files=FALSE, full.names=FALSE)
	print(paste("parsing through", length(executions), " inmate files 🥳"))

	for (i in 1:length(executions)) {

		downloadedExecution = read.properties(paste("../data_extraction/inmate_executions/", executions[i], sep=""), fields = NULL, encoding = "UTF-8")
		inmateInfo = read.properties(paste("../data_extraction/inmate_info/", executions[i], sep=""), fields = NULL, encoding = "UTF-8")
		lastStatement = readLines(paste("../data_extraction/inmate_last_statement/", downloadedExecution$executionNumber, sep=""))

		firstName = downloadedExecution$firstName
		lastName = downloadedExecution$lastName
		executionDate = downloadedExecution$executionDate
		race = downloadedExecution$race

		dob = inmateInfo$dob
		dateReceived = inmateInfo$dateReceived
		eyeColor = inmateInfo$eyeColor
		dateOfOffsense = inmateInfo$dateOfOffsense
		gender = inmateInfo$gender
		hairColor = inmateInfo$hairColor
		nativeCounty = inmateInfo$nativeCounty
		nativeState = inmateInfo$nativeState
		educationLevel = inmateInfo$educationLevel
		occupation = inmateInfo$occupation


		print(paste(firstName, lastName, executionDate, race, dob, dateReceived, eyeColor, dateOfOffsense, gender, hairColor, nativeCounty, nativeState, educationLevel, occupation, lastStatement))


	}
}
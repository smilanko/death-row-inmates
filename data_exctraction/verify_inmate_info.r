library("rvest")
library("stringr")
library("properties")
library("tesseract")
library("imager")
library("svDialogs")
source("fetch_inmate_info.r")

fetchStoredInmateInfo <- function(execution_number) {
	return(read.properties(paste("inmate_info/",execution_number,".properties", sep=""), fields = NULL, encoding = "UTF-8"))
}

verifyInmateInfo <- function() {
	# let's see what executions we already stored
	executions = list.files(path="inmate_executions/", pattern=NULL, all.files=FALSE, full.names=FALSE)
	print(paste("parsing through", length(executions), "executions to verify the inmate info 🥳"))

	# let's get the links for each one
	for (i in 1:length(executions)) {
		downloadedExecution = read.properties(paste("inmate_executions/", executions[i], sep=""), fields = NULL, encoding = "UTF-8")
		infoLink = downloadedExecution$infoLink
		execution_number = downloadedExecution$executionNumber
		print(paste(i, infoLink, execution_number))

		# if we already fetched this file, we can verify it
		if (!file.exists(paste("inmate_info/", execution_number, ".properties", sep=""))) {
			next
		}

		# load the stored data
		currentProperties = fetchStoredInmateInfo(execution_number)
		form <- list(
			"DateOfBirth:TXT" = currentProperties$dob,
			"DateReceived:TXT" = currentProperties$dateReceived,
			"DateOfOffense:TXT" = currentProperties$dateOfOffsense,
			"Occupation:TXT" = currentProperties$occupation,
			"EyeColor:TXT" = currentProperties$eyeColor,
			"Gender:TEXT" = currentProperties$gender,
			"HairColor:TXT" = currentProperties$hairColor,
			"NativeCounty:TXT" = currentProperties$nativeCounty,
			"NativeState:TXT" = currentProperties$nativeState,
			"EducationLevel:TXT" = currentProperties$educationLevel
		)

		# it is an image, let's show it
		if (!grepl(".html", infoLink, fixed=TRUE) ) {
			# display the image, and read the store property file
			plot(load.image(infoLink))
			Sys.sleep(1)
			correctedData = dlg_form(form, paste("Does the image data reflect details for execution", executions[i]))$res
			dev.off() # close the image
			next
		}

		# it is a webpage, let's open a brower tab
		browseURL(infoLink)
		correctedData = dlg_form(form, paste("Does the website data reflect details for execution", executions[i]))$res
		storeInmateInfo(execution_number, correctedData$DateOfBirth, correctedData$DateReceived, correctedData$DateOfOffense, correctedData$Occupation, correctedData$EyeColor, correctedData$Gender, correctedData$HairColor, correctedData$NativeCounty, correctedData$NativeState, correctedData$EducationLevel)

	}
	print("hooray! 🎉 we verified inmate infos")
}
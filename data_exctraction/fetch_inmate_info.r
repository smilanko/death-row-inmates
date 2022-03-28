library("rvest")
library("stringr")
library("properties")
library("tesseract")
library("imager")
library("rapport")

storeInmateInfo <- function(execution_number, date_of_birth, date_received, date_of_offense, inmate_occupation, eye_color, inmate_gender, hair_color, native_county, native_state, education_level) {
	write.properties(file = paste("inmate_info/",execution_number,".properties", sep=""), properties = list(dob = if(rapportools::is.empty(date_of_birth, trim = TRUE)) {'n/a'} else {date_of_birth}, dateReceived = if(rapportools::is.empty(date_received, trim = TRUE)) {'n/a'} else {date_received}, eyeColor = if(rapportools::is.empty(eye_color, trim = TRUE)) {'n/a'} else {eye_color}, dateOfOffsense = if(rapportools::is.empty(date_of_offense, trim = TRUE)) {'n/a'} else {date_of_offense}, gender = if(rapportools::is.empty(inmate_gender, trim = TRUE)) {'n/a'} else {inmate_gender}, hairColor = if(rapportools::is.empty(hair_color, trim = TRUE)) {'n/a'} else {hair_color}, nativeCounty = if(rapportools::is.empty(native_county, trim = TRUE)) {'n/a'} else {native_county}, nativeState = if(rapportools::is.empty(native_state, trim = TRUE)) {'n/a'} else {native_state}, educationLevel = if(rapportools::is.empty(education_level, trim = TRUE)) {'n/a'} else {education_level}, occupation = if(rapportools::is.empty(inmate_occupation, trim = TRUE)) {'n/a'} else {inmate_occupation}), fields = c("dob", "dateReceived", "eyeColor", "dateOfOffsense", "gender", "hairColor", "nativeCounty", "nativeState", "educationLevel", "occupation"))
}

getInmateInfoFromHtmlUsingKey <- function(mainContent, key) {
	return(mainContent[match(1, str_detect(paste('^',gsub("[() ]", "", key),':', sep=""), regex(gsub("[() ]", "", mainContent %>% html_text()), ignore_case = TRUE))) + 1] %>% html_text())
}

removeWeirdCharactersFromHumanText <- function(data) {
	return(gsub("[\"() ~*?°_]", "", data))
}

tryToPrefillValueForTessaractKey <- function(text, key, expectedLoc, isDate) {
	matchIdx = match(1, str_detect(regex(gsub("[\"() ~*\\?°_]", "", text$word), ignore_case = TRUE), paste("^", key, sep="")))
	if (is.na(matchIdx) ) ( return("n/a"))
	sanatizedString = removeWeirdCharactersFromHumanText(text$word[matchIdx + expectedLoc])
	if (length(sanatizedString) > 3 || isFALSE(isDate)) { return(sanatizedString) }
	# let's try to build the date, as it's chunked out
	startIdx = if (expectedLoc < 0) -2 else 1
	parsedDate = ''
	for (i in 1:2) {
		parsedDate = paste(parsedDate, removeWeirdCharactersFromHumanText(text$word[matchIdx + (expectedLoc + startIdx)]), sep="")
		startIdx = startIdx + 1
	}
	return(str_extract(if (expectedLoc < 0) paste(parsedDate, sanatizedString, sep="") else paste(sanatizedString, parsedDate, sep=""), '[0-9]{2}/[0-9]{2}/[0-9]{2}'))
}

downloadInmateInfo <- function() {
	# let's see what executions we already stored
	executions = list.files(path="inmate_executions/", pattern=NULL, all.files=FALSE, full.names=FALSE)
	print(paste("parsing through", length(executions), "executions to download the inmate info 🥳"))

	# let's get the links for each one
	for (i in 1:length(executions)) {
		downloadedExecution = read.properties(paste("inmate_executions/", executions[i], sep=""), fields = NULL, encoding = "UTF-8")
		infoLink = downloadedExecution$infoLink
		execution_number = downloadedExecution$executionNumber

		# when there is no info, sometimes, we get a no_info_avaiable html page.
		if (grepl("no_info_available.html", infoLink, fixed=TRUE)) {
			storeInmateInfo(execution_number, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL)
			next
		}

		# older info records are images, so we can use tesseract to help with data retrieval
		if (!grepl(".html", infoLink, fixed=TRUE) ) {
			
			# if we already fetched this image, we can move on
			if (file.exists(paste("inmate_info/", execution_number, ".properties", sep=""))) {
				next
			}

			# display the image, and run tessaract. give it some time to finish the task
			plot(load.image(infoLink))
			Sys.sleep(1)
			text <- tesseract::ocr_data(infoLink, engine = tesseract("eng"))

			form <- list(
				"DateOfBirth:TXT" = tryToPrefillValueForTessaractKey(text, "Received", -1, TRUE),
				"DateReceived:TXT" = tryToPrefillValueForTessaractKey(text, "Received", 1, TRUE),
				"DateOfOffense:TXT" = tryToPrefillValueForTessaractKey(text, "Date", 3, TRUE),
				"Occupation:TXT" = tryToPrefillValueForTessaractKey(text, "Occupation", 1, FALSE),
				"EyeColor:TXT" = tryToPrefillValueForTessaractKey(text, "Eyes", 1, FALSE),
				"Gender:TEXT" = "male",
				"HairColor:TXT" = tryToPrefillValueForTessaractKey(text, "Hair", 1, FALSE),
				"NativeCounty:TXT" = tryToPrefillValueForTessaractKey(text, "Native", 2, FALSE),
				"NativeState:TXT" = tryToPrefillValueForTessaractKey(text, "State", 1, FALSE),
				"EducationLevel:TXT" = paste(tryToPrefillValueForTessaractKey(text, "Education", 2, FALSE), "years", sep=" ")
			)

			correctedData = dlg_form(form, "Is this data correct?")$res
			storeInmateInfo(execution_number, correctedData$DateOfBirth, correctedData$DateReceived, correctedData$DateOfOffense, correctedData$Occupation, correctedData$EyeColor, correctedData$Gender, correctedData$HairColor, correctedData$NativeCounty, correctedData$NativeState, correctedData$EducationLevel)
			dev.off() # close the image
			next
		}

		# if we made it here, the website gave us parsable html
		# the info is wrapped in a uniquelly identifiable div
		inmateInfoDoc <- read_html(infoLink)
		mainContent = inmateInfoDoc %>% html_nodes(xpath = '//div[@id="content_right"]') %>% html_nodes("table") %>% html_nodes("tr") %>% html_nodes("td")
		storeInmateInfo(execution_number, getInmateInfoFromHtmlUsingKey(mainContent, 'Date of Birth'), getInmateInfoFromHtmlUsingKey(mainContent, 'Date Received'), getInmateInfoFromHtmlUsingKey(mainContent, 'Date of Offense'), str_replace(inmateInfoDoc %>% html_nodes(xpath = '//div[@id="content_right"]') %>% html_nodes("p") %>% html_nodes(xpath = '//p[span[text()="Prior Occupation"]]') %>% html_text(), "Prior Occupation\r\n", ""), getInmateInfoFromHtmlUsingKey(mainContent, 'Eye Color'), getInmateInfoFromHtmlUsingKey(mainContent, 'Gender'), getInmateInfoFromHtmlUsingKey(mainContent, 'Hair Color'), getInmateInfoFromHtmlUsingKey(mainContent, 'Native County'), getInmateInfoFromHtmlUsingKey(mainContent, 'Native State'), getInmateInfoFromHtmlUsingKey(mainContent, 'EducationLevel(HighestGradeCompleted)'))
	}
	print("hooray! 🎉 we downloaded inmate infos")
}
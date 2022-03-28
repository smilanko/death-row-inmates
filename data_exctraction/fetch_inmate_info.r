library("rvest")
library("stringr")
library("properties")
library("tesseract")
library("imager")
library("svDialogs")

getInmateInfoFromHtmlUsingKey <- function(mainContent, key) {
	return(mainContent[match(1, str_detect(paste('^',gsub("[() ]", "", key),':', sep=""), regex(gsub("[() ]", "", mainContent %>% html_text()), ignore_case = TRUE))) + 1] %>% html_text())
}

downloadInmateInfo <- function() {
	# let's see what executions we already stored
	executions = list.files(path="available_executions/", pattern=NULL, all.files=FALSE, full.names=FALSE)
	print(paste("we loaded", length(executions), "executions"))

	# let's get the links for each one
	for (i in 1:length(executions)) {
		downloadedExecution = read.properties(paste("available_executions/", executions[i], sep=""), fields = NULL, encoding = "UTF-8")
		infoLink = downloadedExecution$infoLink
		execution_number = downloadedExecution$executionNumber

		# if we already fetched this file, we can move on
		if (file.exists(paste("manual_inmate_info/", execution_number, ".properties", sep=""))) {
			next
		}

		# when there is no info, sometimes, we get a no_info_avaiable html page.
		if (grepl("no_info_available.html", infoLink, fixed=TRUE)) {
			manuallyStoreInmateInfo(execution_number, 'n/a', 'n/a', 'n/a', 'n/a', 'n/a', 'n/a', 'n/a', 'n/a', 'n/a', 'n/a')
			next
		}

		# older info records are images, so we can use tesseract to help with data retrieval
		if (!grepl(".html", infoLink, fixed=TRUE) ) {
			
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
			manuallyStoreInmateInfo(execution_number, correctedData$DateOfBirth, correctedData$DateReceived, correctedData$DateOfOffense, correctedData$Occupation, correctedData$EyeColor, correctedData$Gender, correctedData$HairColor, correctedData$NativeCounty, correctedData$NativeState, correctedData$EducationLevel)
			dev.off() # close the image
			next
		}

		# if we made it here, the website have us parsable html
		# get the page using the inmate_info_link
		# the info is in a table, wrapped in a uniquelly identifiable div
		inmateInfoDoc <- read_html(infoLink)
		mainContent = inmateInfoDoc %>% html_nodes(xpath = '//div[@id="content_right"]') %>% html_nodes("table") %>% html_nodes("tr") %>% html_nodes("td")
		manuallyStoreInmateInfo(execution_number, getInmateInfoFromHtmlUsingKey(mainContent, 'Date of Birth'), getInmateInfoFromHtmlUsingKey(mainContent, 'Date Received'), getInmateInfoFromHtmlUsingKey(mainContent, 'Date of Offense'), str_replace(inmateInfoDoc %>% html_nodes(xpath = '//div[@id="content_right"]') %>% html_nodes("p") %>% html_nodes(xpath = '//p[span[text()="Prior Occupation"]]') %>% html_text(), "Prior Occupation\r\n", ""), getInfoByKey(mainContent, 'Eye Color'), getInfoByKey(mainContent, 'Gender'), getInfoByKey(mainContent, 'Hair Color'), getInfoByKey(mainContent, 'Native County'), getInfoByKey(mainContent, 'Native State'), getInfoByKey(mainContent, 'EducationLevel(HighestGradeCompleted)'))
	}
}
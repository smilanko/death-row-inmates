library("rvest")
library("stringr")
library("properties")
library("tesseract")
library("imager")
library("svDialogs")

getInfoByKey <- function(mainContent, key) {
	return(mainContent[match(1, str_detect(paste('^',gsub("[() ]", "", key),':', sep=""), regex(gsub("[() ]", "", mainContent %>% html_text()), ignore_case = TRUE))) + 1] %>% html_text())
}

manuallyStoreInmateInfo <- function(execution_number, date_of_birth, date_received, date_of_offense, inmate_occupation, eye_color, inmate_gender, hair_color, native_county, native_state, education_level, age_at_offense) {
	print(paste("storing manual inmate info", execution_number));
	write.properties(file = paste("manual_inmate_info/",execution_number,".properties", sep=""), properties = list(dob = date_of_birth, dateReceived = date_received, eyeColor = eye_color, dateOfOffsense = date_of_offense, gender = inmate_gender, hairColor = hair_color, nativeCounty = native_county, nativeState = native_state, educationLevel = education_level, occupation = inmate_occupation), fields = c("dob", "dateReceived", "eyeColor", "dateOfOffsense", "gender", "hairColor", "nativeCounty", "nativeState", "educationLevel", "occupation"))
}

fetchManuallyStoredInmateInfo <- function(execution_number) {
	return(read.properties(paste("manual_inmate_info/",execution_number,".properties", sep=""), fields = NULL, encoding = "UTF-8"))
}

removeFuncCharactersFromString <- function(data) {
	return(gsub("[\"() ~*?°_]", "", data))
}

tryToPrefillValueForTessaractKey <- function(text, key, expectedLoc, isDate) {
	matchIdx = match(1, str_detect(regex(gsub("[\"() ~*\\?°_]", "", text$word), ignore_case = TRUE), paste("^", key, sep="")))
	if (is.na(matchIdx) ) ( return("n/a"))
	sanatizedString = removeFuncCharactersFromString(text$word[matchIdx + expectedLoc])
	if (length(sanatizedString) > 3 || isFALSE(isDate)) { return(sanatizedString) }
	# let's try to build the date, as it's chunked out
	startIdx = if (expectedLoc < 0) -2 else 1
	parsedDate = ''
	for (i in 1:2) {
		parsedDate = paste(parsedDate, removeFuncCharactersFromString(text$word[matchIdx + (expectedLoc + startIdx)]), sep="")
		startIdx = startIdx + 1
	}
	return(str_extract(if (expectedLoc < 0) paste(parsedDate, sanatizedString, sep="") else paste(sanatizedString, parsedDate, sep=""), '[0-9]{2}/[0-9]{2}/[0-9]{2}'))
}

downloadInmateData <- function() {
	# constants
	baseUrl = "https://www.tdcj.texas.gov/death_row/"

	# read the inmate table
	baseDoc <- read_html(paste(baseUrl, "dr_executed_offenders.html", sep=""))
	allInmatesTable = baseDoc %>% html_nodes("table") %>% html_nodes("tr")

	# get the general data from the death row table
	inmate_execution_numbers = c()
	inmate_info_links = c()
	inmate_last_statement_links = c()
	inmate_last_statements = c()
	inmate_last_names = c()
	inmate_first_names = c()
	inmate_tdjc_numbers = c()
	inmate_ages_at_execution = c()
	inmate_execution_dates = c()
	inmate_races = c()
	inmate_counties = c()
	inmate_dobs = c()
	inmate_received_dates = c()
	inmate_occupations = c()
	inmate_eye_colors = c()
	inmate_dates_of_offense = c()
	inmate_genders = c()
	inmate_hair_colors = c()
	inmate_native_counties = c()
	inmate_native_states = c()
	inmate_education_levels = c()

	insert_idx = 1
	for(i in 2:length(allInmatesTable)) {
		inmate = allInmatesTable[i] %>% html_nodes("td")
		inmate_execution_numbers[insert_idx] = strtoi(inmate[1] %>% html_text())
		inmate_info_links[insert_idx] = paste(baseUrl, str_remove(inmate[2] %>% html_nodes("a") %>% html_attr('href'), "/death_row/"), sep="")
		inmate_last_statement_links[insert_idx] = paste(baseUrl, str_remove(inmate[3] %>% html_nodes("a") %>% html_attr('href'), "/death_row/"), sep="")
		inmate_last_names[insert_idx] = inmate[4] %>% html_text()
		inmate_first_names[insert_idx] = inmate[5] %>% html_text()
		inmate_tdjc_numbers[insert_idx] = strtoi(inmate[6] %>% html_text())
		inmate_ages_at_execution[insert_idx] = strtoi(inmate[7] %>% html_text())
		inmate_execution_dates[insert_idx] = inmate[8] %>% html_text()
		inmate_races[insert_idx] = inmate[9] %>% html_text()
		inmate_counties[insert_idx] = inmate[10] %>% html_text()
		insert_idx = insert_idx + 1
	}

	print(paste("great news, we have found", insert_idx, "inmates."))

	# let's get more information on the inmate
	for (i in 1:length(inmate_info_links)) {
		print(paste("### fetching info:: ", inmate_info_links[i], "for inmate", inmate_execution_numbers[i]))

		# older info records are images, so we can use tesseract to get the data out
		# and then manually fix it so that it is sane
		if (!grepl(".html", inmate_info_links[i], fixed=TRUE) ) {

			# lets show what we have
			plot(load.image(inmate_info_links[i]))
			Sys.sleep(1)

			# did we already label this file?
			execution_number = inmate_execution_numbers[i]
			if (file.exists(paste("manual_inmate_info/", execution_number, ".properties", sep=""))) {
				currentProperties = fetchManuallyStoredInmateInfo(execution_number)
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
				correctedData = dlg_form(form, "Is this data correct?")$res
				
				manuallyStoreInmateInfo(execution_number, correctedData$DateOfBirth, correctedData$DateReceived, correctedData$DateOfOffense, correctedData$Occupation, correctedData$EyeColor, correctedData$Gender, correctedData$HairColor, correctedData$NativeCounty, correctedData$NativeState, correctedData$EducationLevel)

				inmate_dobs[i] = correctedData$DateOfBirth
				inmate_received_dates[i] = correctedData$DateReceived
				inmate_dates_of_offense[i] = correctedData$DateOfOffense
				inmate_occupations[i] = correctedData$Occupation
				inmate_eye_colors[i] = correctedData$EyeColor
				inmate_genders[i] = correctedData$Gender
				inmate_hair_colors[i] = correctedData$HairColor
				inmate_native_counties[i] = correctedData$NativeCounty
				inmate_native_states[i] = correctedData$NativeState
				inmate_education_levels[i] = correctedData$EducationLevel

				# close the image
				dev.off()
				next
			}

			

			# run tessaract
			text <- tesseract::ocr_data(inmate_info_links[i], engine = tesseract("eng"))

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
			
			inmate_dobs[i] = correctedData$DateOfBirth
			inmate_received_dates[i] = correctedData$DateReceived
			inmate_dates_of_offense[i] = correctedData$DateOfOffense
			inmate_occupations[i] = correctedData$Occupation
			inmate_eye_colors[i] = correctedData$EyeColor
			inmate_genders[i] = correctedData$Gender
			inmate_hair_colors[i] = correctedData$HairColor
			inmate_native_counties[i] = correctedData$NativeCounty
			inmate_native_states[i] = correctedData$NativeState
			inmate_education_levels[i] = correctedData$EducationLevel

			# close the image
			dev.off()
			next
		}

		if (grepl("no_info_available.html", inmate_info_links[i], fixed=TRUE)) {
			inmate_dobs[i] = 'n/a'
			inmate_received_dates[i] = 'n/a'
			inmate_dates_of_offense[i] = 'n/a'
			inmate_occupations[i] = 'n/a'
			inmate_eye_colors[i] = 'n/a'
			inmate_genders[i] = 'n/a'
			inmate_hair_colors[i] = 'n/a'
			inmate_native_counties[i] = 'n/a'
			inmate_native_states[i] = 'n/a'
			inmate_education_levels[i] = 'n/a'
			next
		}

		# get the page using the inmate_info_link
		# the info is in a table, wrapped in a uniquelly identifiable div
		inmateInfoDoc <- read_html(inmate_info_links[i])
		mainContent = inmateInfoDoc %>% html_nodes(xpath = '//div[@id="content_right"]') %>% html_nodes("table") %>% html_nodes("tr") %>% html_nodes("td")

		inmate_eye_colors[i] = getInfoByKey(mainContent, 'Eye Color')
		inmate_dates_of_offense[i] = getInfoByKey(mainContent, 'Date of Offense')
		inmate_genders[i] = getInfoByKey(mainContent, 'Gender')
		inmate_hair_colors[i] = getInfoByKey(mainContent, 'Hair Color')
		inmate_native_counties[i] = getInfoByKey(mainContent, 'Native County')
		inmate_native_states[i] = getInfoByKey(mainContent, 'Native State')
		inmate_education_levels[i] = getInfoByKey(mainContent, 'EducationLevel(HighestGradeCompleted)')
	}

	# let's get each last statement using the links stored above
	for (i in 1:length(inmate_last_statement_links)) {
		
		print(paste("### fetching latest statement:: ", inmate_last_statement_links[i], "for inmate", inmate_execution_numbers[i]))

		# some links that have no last statement take us to a default no statement page
		# if we encounter this, we can simply leave the insmates last statement blank
		if (grepl("no_last_statement.html", inmate_last_statement_links[i], fixed=TRUE)) {
			print("##### redirected to default no last statement page");
			inmate_last_statements[i] = ''
			next
		}

		inmatesComment = ''

		# get the page using the inmate_last_statement_link
		# the statement is in a uniquelly identifiable div, after the <p>Last Statement:</p>
		lastStatementDoc <- read_html(inmate_last_statement_links[i])
		mainContent = lastStatementDoc %>% html_nodes(xpath = '//div[@id="content_right"]') %>% html_nodes("p")
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
					print("##### no last statement explicitly stated");
					inmate_last_statements[i] = ''
					next
				}
				inmatesComment = paste(inmatesComment, potentialStmt)
			}
		} else {
			print("##### last statement not found on inmate's page");
		}

		inmate_last_statements[i] = inmatesComment
	}

	Inmates = data.frame(
		execution_number = inmate_execution_numbers,
		tdjc_number = inmate_tdjc_numbers,
		first_name = inmate_first_names,
		last_name = inmate_last_names,
		gender = inmate_genders,
		eye_color = inmate_eye_colors,
		hair_color = inmate_hair_colors,
		education_level = inmate_education_levels,
		date_of_offense = inmate_dates_of_offense,
		age_at_execution = inmate_ages_at_execution,
		execution_date = inmate_execution_dates,
		race = inmate_races,
		county = inmate_counties,
		native_county = inmate_native_counties,
		native_state = inmate_native_states,
		last_statement = inmate_last_statements,
		info_link = inmate_info_links,
		last_statement_link = inmate_last_statement_links)

	# sort by execution_number
	Inmates <- Inmates[order(-Inmates$execution_number),]

	comment(Inmates$execution_number)<-c("Execution number for the inmate. The first inmate to be executed is labeled as 1.")
	comment(Inmates$tdjc_number)<-c("Texas department of justice number")
	comment(Inmates$first_name)<-c("Inmate's first name.")
	comment(Inmates$last_name)<-c("Inmate's last name.")
	comment(Inmates$gender)<-c("Inmate's gender")
	comment(Inmates$eye_color)<-c("Inmate's eye color")
	comment(Inmates$hair_color)<-c("Inmate's hair color")
	comment(Inmates$education_level)<-c("Inmate's education level. n/a means unavailable")
	comment(Inmates$date_of_offense)<-c("The date of when the final crime was commited")
	comment(Inmates$age_at_execution)<-c("Inmate's age when executed")
	comment(Inmates$execution_date)<-c("Date of execution")	
	comment(Inmates$race)<-c("Inmate's race")
	comment(Inmates$county)<-c("is this the county where the crime was commited?")
	comment(Inmates$native_county)<-c("is this the county where the criminal grew up?")
	comment(Inmates$native_state)<-c("is this the state where the criminal grew up?")
	comment(Inmates$last_statement)<-c("Last statement text that was automatically extracted from the inmate's webpage.")
	comment(Inmates$info_link)<-c("This is the link you can use to get more details on the inmate.")
	comment(Inmates$last_statement_link)<-c("This is the link you can use to get the last statement for the inmate.")

	return(Inmates)
}

# source("downloader.r")
# Inmates = downloadInmateData()
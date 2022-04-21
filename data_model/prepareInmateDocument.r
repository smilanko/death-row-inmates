library("properties")
library("rvest")
library("stringr")
library("rapport")

cleanupLastStatement <- function(stmt) {
	# nothing was said
	if (length(stmt) == 0) {return("")}
	# remove multi space
	stmt = str_replace_all(stmt, " +", " ")
	# remove forward slashes
	stmt = str_replace_all(stmt, "///", "")
	# remove back slashes
	stmt = str_replace_all(stmt, "\\\\", "")
	# remove quotes
	stmt = str_replace_all(stmt, "\"", "")
	# remove new lines
	stmt = str_replace_all(stmt, "[\r\n]" , "")
	# remove beggining and ending whitespace
	return(rapportools::trim.space(stmt))
}

cleanupOccupation <- function(occupation) {
	# trim space and remove muli space
	occupation = rapportools::trim.space(str_replace_all(occupation, " +", " "))
	# remove all forward slashes, except in n/a
	if (occupation != 'n/a') { occupation = str_replace_all(occupation, "/", ",")}
	return(occupation)
}

prepareInmateDocument <- function() {
	# let's see what executions we already stored
	executions = list.files(path="../data_extraction/inmate_executions/", pattern=NULL, all.files=FALSE, full.names=FALSE)
	print(paste("parsing through", length(executions), " inmate files 🥳"))

	inmate_execution_number = c()
	inmate_first_names = c()
	inmate_last_names = c()
	inmate_execution_dates = c()
	inmate_races = c()
	inmate_dates_of_birth = c()
	inmate_dates_received = c()
	inmate_eye_colors = c()
	inmate_dates_of_offense = c()
	inmate_genders = c()
	inmate_hair_colors = c()
	inmate_native_counties = c()
	inmate_native_states = c()
	inmate_education_levels = c()
	inmate_occupations = c()
	inmate_last_statements = c()

	for (i in 1:length(executions)) {
		downloadedExecution = read.properties(paste("../data_extraction/inmate_executions/", executions[i], sep=""), fields = NULL, encoding = "UTF-8")
		inmateInfo = read.properties(paste("../data_extraction/inmate_info/", executions[i], sep=""), fields = NULL, encoding = "UTF-8")
		lastStatement = readLines(paste("../data_extraction/inmate_last_statement/", downloadedExecution$executionNumber, sep=""))



		inmate_execution_number[i] = downloadedExecution$executionNumber
		inmate_first_names[i] = downloadedExecution$firstName
		inmate_last_names[i] = downloadedExecution$lastName
		inmate_execution_dates[i] = downloadedExecution$executionDate
		inmate_races[i] = tolower(downloadedExecution$race)
		inmate_dates_of_birth[i] = inmateInfo$dob
		inmate_dates_received[i] = inmateInfo$dateReceived
		inmate_eye_colors[i] = tolower(inmateInfo$eyeColor)
		inmate_dates_of_offense[i] = inmateInfo$dateOfOffsense
		inmate_genders[i] = tolower(inmateInfo$gender)
		inmate_hair_colors[i] = tolower(inmateInfo$hairColor)
		inmate_native_counties[i] = tolower(inmateInfo$nativeCounty)
		inmate_native_states[i] = tolower(inmateInfo$nativeState)
		inmate_education_levels[i] = rapportools::trim.space(str_replace_all(tolower(inmateInfo$educationLevel), " +", " "))
		inmate_occupations[i] = cleanupOccupation(tolower(inmateInfo$occupation))
		inmate_last_statements[i] = cleanupLastStatement(lastStatement)
	}

	# prepare a dataframe
	Inmates = data.frame(
		execution_number = inmate_execution_number,
		first_name = inmate_first_names,
		last_name = inmate_last_names,
		execution_date = inmate_execution_dates,
		race = inmate_races,
		date_of_birth = inmate_dates_of_birth,
		date_received = inmate_dates_received,
		eye_color = inmate_eye_colors,
		date_of_offense = inmate_dates_of_offense,
		gender = inmate_genders,
		hair_color = inmate_hair_colors,
		native_county = inmate_native_counties,
		native_state = inmate_native_states,
		education_level = inmate_education_levels,
		occupation = inmate_occupations,
		last_statement = inmate_last_statements)

	comment(Inmates$execution_number)<-c("Execution number for the inmate. The first inmate to be executed is labeled as 1")
	comment(Inmates$first_name)<-c("Inmate's first name")
	comment(Inmates$last_name)<-c("Inmate's last name")
	comment(Inmates$execution_date)<-c("Date of execution")	
	comment(Inmates$race)<-c("Inmate's race")
	comment(Inmates$date_of_birth)<-c("Inmate's date of birth")
	comment(Inmates$date_received)<-c("Date when inmate went to deathrow prison, or whatever that place is called.")
	comment(Inmates$eye_color)<-c("Inmate's eye color")
	comment(Inmates$date_of_offense)<-c("The date of when the final crime was commited")
	comment(Inmates$gender)<-c("Inmate's gender")
	comment(Inmates$hair_color)<-c("Inmate's hair color")
	comment(Inmates$native_county)<-c("I think this is the county where the criminal grew up")
	comment(Inmates$native_state)<-c("I think this the state where the criminal grew up")
	comment(Inmates$education_level)<-c("Inmate's education level. It is measured in years, which is odd. e.g 16 years means 12 years of highschool and 4 years of college")
	comment(Inmates$occupation)<-c("Professional occupation of the inmate")
	comment(Inmates$last_statement)<-c("Last statement that was issued by the inmate")
	print(paste("inmate nodel built! 🥳 Run str(Inmates) to get started"))
	return(Inmates)
}
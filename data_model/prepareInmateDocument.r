library("properties")
library("rvest")
library("stringr")
library("rapport")

prepareInmateDocument <- function() {
	# let's see what executions we already stored
	executions = list.files(path="../data_extraction/inmate_executions/", pattern=NULL, all.files=FALSE, full.names=FALSE)
	print(paste("parsing through", length(executions), " inmate files 🥳"))

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

		# remove multi space
		lastStatement = str_replace_all(lastStatement, " +", " ")
		# remove forward slashes
		lastStatement = str_replace_all(lastStatement, "///", "")
		# remove back slashes
		lastStatement = str_replace_all(lastStatement, "\\\\", "")
		# remove quotes
		lastStatement = str_replace_all(lastStatement, "\"", "")
		# remove new lines
		lastStatement = str_replace_all(lastStatement, "[\r\n]" , "")
		# remove beggining and ending whitespace
		lastStatement = rapportools::trim.space(lastStatement)

		inmate_first_names[i] = downloadedExecution$firstName
		inmate_last_names[i] = downloadedExecution$lastName
		inmate_execution_dates[i] = downloadedExecution$executionDate
		inmate_races[i] = downloadedExecution$race
		inmate_dates_of_birth[i] = inmateInfo$dob
		inmate_dates_received[i] = inmateInfo$dateReceived
		inmate_eye_colors[i] = inmateInfo$eyeColor
		inmate_dates_of_offense[i] = inmateInfo$dateOfOffsense
		inmate_genders[i] = inmateInfo$gender
		inmate_hair_colors[i] = inmateInfo$hairColor
		inmate_native_counties[i] = inmateInfo$nativeCounty
		inmate_native_states[i] = inmateInfo$nativeState
		inmate_education_levels[i] = inmateInfo$educationLevel
		inmate_occupations[i] = inmateInfo$occupation
		inmate_last_statements[i] = lastStatement
	}

	# prepare a dataframe
	Inmates = data.frame(
		first_name = inmate_first_names,
		last_name = inmate_last_names,
		execution_date = inmate_execution_dates,
		race = inmate_races,
		dob = inmate_dates_of_birth,
		date_received = inmate_dates_received,
		eye_color = inmate_eye_colors,
		doo = inmate_dates_of_offense,
		gender = inmate_genders,
		hair_color = inmate_hair_colors,
		native_county = inmate_native_counties,
		native_state = inmate_native_states,
		education_level = inmate_education_levels,
		occupation = inmate_occupations,
		last_statement = inmate_last_statements)

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
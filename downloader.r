library(rvest)
library(stringr)

getInfoByKey <- function(mainContent, key) {
	return(mainContent[match(1, str_detect(paste('^',gsub("[() ]", "", key),':', sep=""), regex(gsub("[() ]", "", mainContent %>% html_text()), ignore_case = TRUE))) + 1] %>% html_text())
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
	inmate_eye_colors = c()
	inmate_dates_of_offense = c()
	inmate_genders = c()
	inmate_hair_colors = c()
	inmate_native_counties = c()
	inmate_native_states = c()
	inmate_education_levels = c()
	inmate_ages_at_offense = c()

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

		if (!grepl(".html", inmate_info_links[i], fixed=TRUE) || grepl("no_info_available.html", inmate_info_links[i], fixed=TRUE)) {
			print("##### need to investigate");
			inmate_eye_colors[i] = ''
			inmate_dates_of_offense[i] = ''
			inmate_genders[i] = ''
			inmate_hair_colors[i] = ''
			inmate_native_counties[i] = ''
			inmate_native_states[i] = ''
			inmate_education_levels[i] = ''
			inmate_ages_at_offense[i] = ''
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
		inmate_ages_at_offense[i] = getInfoByKey(mainContent, 'Age(atthetimeofOffense)')
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
		age_at_offsense = inmate_ages_at_offense,
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
	comment(Inmates$age_at_offsense)<-c("Inmate's age when crime commited")
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
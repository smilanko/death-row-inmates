library(rvest)
library(stringr)

#' Download death row inmate data from https://www.tdcj.texas.gov/death_row/dr_executed_offenders.html
#' 
#' @return Parsed data for each inmate as a dataframe
#' @examples
#' Inmates <- downloadInmateData()

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
	inmate_ages = c()
	inmate_execution_dates = c()
	inmate_races = c()
	inmate_countys = c()

	insert_idx = 1
	for(i in 2:length(allInmatesTable)) {
		inmate = allInmatesTable[i] %>% html_nodes("td")
		inmate_execution_numbers[insert_idx] = strtoi(inmate[1] %>% html_text())
		inmate_info_links[insert_idx] = paste(baseUrl, str_remove(inmate[2] %>% html_nodes("a") %>% html_attr('href'), "/death_row/"), sep="")
		inmate_last_statement_links[insert_idx] = paste(baseUrl, str_remove(inmate[3] %>% html_nodes("a") %>% html_attr('href'), "/death_row/"), sep="")
		inmate_last_names[insert_idx] = inmate[4] %>% html_text()
		inmate_first_names[insert_idx] = inmate[5] %>% html_text()
		inmate_tdjc_numbers[insert_idx] = strtoi(inmate[6] %>% html_text())
		inmate_ages[insert_idx] = strtoi(inmate[7] %>% html_text())
		inmate_execution_dates[insert_idx] = inmate[8] %>% html_text()
		inmate_races[insert_idx] = inmate[9] %>% html_text()
		inmate_countys[insert_idx] = inmate[10] %>% html_text()
		insert_idx = insert_idx + 1
	}

	print(paste("great news, we have found", insert_idx, "inmates."))

	# let's get each last statement using the links stored above
	for (i in 1:length(inmate_last_statement_links)) {
	# for (i in 1:2) {
		
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
			# Let's check that their statement is not "No statement was made"
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
		info_link = inmate_info_links,
		last_statement_link = inmate_last_statement_links,
		last_statement = inmate_last_statements,
		last_name = inmate_last_names,
		first_name = inmate_first_names,
		tdjc_number = inmate_tdjc_numbers,
		age = inmate_ages,
		execution_date = inmate_execution_dates,
		race = inmate_races,
		county = inmate_countys)
	# sort by execution_number
	Inmates <- Inmates[order(-Inmates$execution_number),]

	comment(Inmates$execution_number)<-c("Execution number for the inmate. The first inmate to be executed is labeled as 1.")
	comment(Inmates$info_link)<-c("This is the link you can use to get more details on the inmate.")
	comment(Inmates$last_statement_link)<-c("This is the link you can use to get the last statement for the inmate.")
	comment(Inmates$last_statement)<-c("Last statement text that was automatically extracted from the inmate's webpage.")
	comment(Inmates$last_name)<-c("Inmate's last name.")
	comment(Inmates$first_name)<-c("Inmate's first name.")
	comment(Inmates$tdjc_number)<-c("Texas department of justice number")
	comment(Inmates$age)<-c("Age of inmage on execution day")
	comment(Inmates$execution_date)<-c("Date of execution")
	comment(Inmates$race)<-c("Inmate's race")
	comment(Inmates$county)<-c("Inmate's county")
	return(Inmates)
}

# source("downloader.r")
# Inmates = downloadInmateData()
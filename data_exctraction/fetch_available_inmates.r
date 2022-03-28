library("rvest")
library("stringr")
library("properties")

storeExecution <- function(execution_number, info_link, last_statement_link, last_name, first_name, tdjc_number, age_at_execution, execution_date, race, offense_county) {
	write.properties(file = paste("available_executions/",execution_number,".properties", sep=""), properties = list(infoLink = info_link, lastStatementLink = last_statement_link, lastName = last_name, firstName = first_name, tdjcNumber = tdjc_number, ageAtExecution = age_at_execution, executionDate = execution_date, race = race, offenseCounty = offense_county), fields = c("infoLink", "lastStatementLink", "lastName", "firstName", "tdjcNumber", "ageAtExecution", "executionDate", "race", "offenseCounty"))
}

fetchAvailableInmates <- function() {
	# constants
	baseUrl = "https://www.tdcj.texas.gov/death_row/"
	print(paste("fetching available inmates from", baseUrl))
	# read the inmate table
	baseDoc <- read_html(paste(baseUrl, "dr_executed_offenders.html", sep=""))
	allInmatesTable = baseDoc %>% html_nodes("table") %>% html_nodes("tr")

	# get the general data from the death row table
	for(i in 2:length(allInmatesTable)) {
		inmate = allInmatesTable[i] %>% html_nodes("td")
		storeExecution(strtoi(inmate[1] %>% html_text()), paste(baseUrl, str_remove(inmate[2] %>% html_nodes("a") %>% html_attr('href'), "/death_row/"), sep=""), paste(baseUrl, str_remove(inmate[3] %>% html_nodes("a") %>% html_attr('href'), "/death_row/"), sep=""), inmate[4] %>% html_text(), inmate[5] %>% html_text(), strtoi(inmate[6] %>% html_text()), strtoi(inmate[7] %>% html_text()), inmate[8] %>% html_text(), inmate[9] %>% html_text(), inmate[10] %>% html_text())
	}
	print(paste("great news, we have found", length(allInmatesTable) - 1, "inmates."))

}
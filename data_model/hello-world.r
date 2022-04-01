library("dplyr")
library("rapport")
source("prepareInmateDocument.r")

showDataLevels <- function(Inmates) {
	print(levels(factor(Inmates$gender)))
	print(levels(factor(Inmates$eye_color)))
	print(levels(factor(Inmates$hair_color)))
	print(levels(factor(Inmates$race)))
	print(levels(factor(Inmates$native_county)))
	print(levels(factor(Inmates$native_state)))
	print(levels(factor(Inmates$education_level)))

	allOccupations = Inmates$occupation
	unique_occupations = c()
	for (i in 1:length(allOccupations)) {
		# split the occupation into parts, since individuals can have more than one profession
		currentOccupations = unlist(str_split(Inmates$occupation[i], ","))
		for (j in 1:length(currentOccupations)) { 
			unique_occupations = append(unique_occupations, rapportools::trim.space(currentOccupations[j])) 
		}
	}

	unique_occupations = unique_occupations[!duplicated(unique_occupations)]
	levels(factor(unique_occupations))
}

showDaysInJail <- function(Inmates) {
	for (i in 1:nrow(Inmates)) {
		days_in_jail = difftime(as.Date(Inmates$date_received[i], format = "%m/%d/%Y"), as.Date(Inmates$execution_date[i], format = "%m/%d/%Y"), units = "days")
		annoying_1 = as.Date(Inmates$date_received[i], format = "%m/%d/%Y")
		annoying_2 = as.Date(Inmates$execution_date[i], format = "%m/%d/%Y")
		print(paste(Inmates$execution_number[i], annoying_1, annoying_2, days_in_jail))

		if (is.na(days_in_jail)) { next }
		
		if (days_in_jail > 0) {
			print(paste("execution ", Inmates$execution_number[i], "might have the execution dates wrong"))
			quit(-1)
		}
	}
}

# load the inmates
Inmates = prepareInmateDocument()
showDaysInJail(Inmates)
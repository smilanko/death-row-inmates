library("properties")
library("rvest")
library("stringr")
library("rapport")
library("dplyr")
library("utils")
library("DescTools")
library("tm")
library("psych")
library("sjmisc")
library("grDevices")

setwd("~/Downloads/death-row-inmates-e9841ef48828d2ec8413b4b616cf766967686878/data_model")

source("prepareInmateDocument.r")

Inmates = prepareInmateDocument()

#silence the warning, since this script is verified
options(warn=-1)

#set seed 
set.seed(9)

#filter out empty statements
empty_last_statement_filter = which(Inmates$last_statement == "")
Inmates <- Inmates[-empty_last_statement_filter, ]
#450 observations left  

#create factor variables for forgiveness: 0 = does not say "forgive" 1 = does say "forgive"
Inmates$has_forgive <- ifelse(grepl("forgive", Inmates$last_statement, ignore.case = TRUE), "1","0")
View(Inmates)
forgive <- Inmates$has_forgive

#calculate days in jail
days_in_jail = difftime(as.Date(Inmates$date_received, format = "%m/%d/%Y"), as.Date(Inmates$execution_date, format = "%m/%d/%Y"), units = "days")

#we do not want data that has no days in jail over 19 years
jail_sentence_filter <- which(is.na(days_in_jail) | (days_in_jail < -7000))
Inmates <- Inmates[-jail_sentence_filter]
days_in_jail <- as.numeric(days_in_jail[-jail_sentence_filter])
forgive <- forgive[-jail_sentence_filter]
days_in_jail
forgive


m1 = lm(forgive~days_in_jail)
print(summary(m1))

pairs.panels(data.frame(days_in_jail, forgive), labels =c('Days on Death Row', 'Presence of Forgive in Last Statement'), smooth = TRUE, scale = FALSE, density = TRUE, ellipses = TRUE,  method = "pearson",  pch = 21, lm = FALSE, cor = TRUE, jiggle = FALSE, factor = 2,  hist.col = 4, stars = TRUE,  ci = TRUE)


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

setwd("~/Downloads/death-row-inmates-main 2/data_model")

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

#checking how many observations for each variable
yes_forgive <- str_subset(forgive, "1")
yes_forgive.data <- data.frame(yes_forgive)
#115 observations
no_forgive <- str_subset(forgive, "0")
no_forgive.data <- data.frame(no_forgive)
#302 observations


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

#jitterplot
setEPS()
postscript("~/Downloads/death-row-inmates-main 2/data_model/plots/forgiveness_plots/forgive_jitterplot.eps",width=5,height=4)
p_data <- as.data.frame(cbind(days_in_jail, forgive))
forgive <- as.numeric(forgive)
plot(jitter(p_data$forgive), p_data$days_in_jail, pch = 16, col = "#de536b", xaxt="n", ylab="Days in Jail", xlab="Forgive")
axis(1, xaxp=c(0, 1, 1), las=1)
dev.off()

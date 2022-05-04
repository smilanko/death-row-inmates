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
library("pROC")



setwd("~/Downloads/death-row-inmates-main 2/data_model")

source("prepareInmateDocument.r")

Inmates = prepareInmateDocument()

#silence the warning, since this script is verified
options(warn=-1)

#set seed 
set.seed(11)

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

#jitterplot
setEPS()
postscript("~/Downloads/death-row-inmates-main 2/data_model/plots/forgiveness_plots/forgive_jitterplot.eps",width=5,height=4)
p_data <- as.data.frame(cbind(days_in_jail, forgive))
forgive <- as.numeric(forgive)
plot(jitter(p_data$forgive), p_data$days_in_jail, pch = 16, col = "#de536b", xaxt="n", ylab="Days in Jail", xlab="Forgive")
axis(1, xaxp=c(0, 1, 1), las=1)
dev.off()

#roc curve
ind <- sample(2, nrow(p_data), replace = T, prob = c(0.7, 0.3))
train <- p_data[ind == 1,]
test <- p_data[ind == 2,]
m2 <- glm(forgive~., data = train, family = 'binomial' )
print(summary(m2))

p1 <- predict(m2, train, type = 'response')
p2 <- predict(m2, test, type = 'response')

postscript("~/Downloads/death-row-inmates-main 2/data_model/plots/forgiveness_plots/forgive_roc.eps",width=6,height=3)
par(mfrow=c(1,2))
r_one <- multiclass.roc(train$forgive, p1, percent = TRUE)
roc_one <- r_one[['rocs']]
r1 <- roc_one[[1]]
plot.roc(r1, main = 'ROC Curve for Train Data', col = 'red', lwd = 5, print.auc = T, auc.polygon = T, max.auc.polygon = T, auc.polygon.col = 'lightblue', print.thres = T)
auc(r1)
coords(r1, "best", ret="threshold", transpose = FALSE)
r_two <- multiclass.roc(test$forgive, p2, percent = TRUE)
roc_two <- r_two[['rocs']]
r2 <- roc_two[[1]]
plot.roc(r2, main = 'ROC Curve for Test Data', col = 'red', lwd = 5, print.auc = T, auc.polygon = T, max.auc.polygon = T, auc.polygon.col = 'lightblue', print.thres = T)
auc(r2)
coords(r2, "best", ret="threshold", transpose = FALSE)
dev.off()


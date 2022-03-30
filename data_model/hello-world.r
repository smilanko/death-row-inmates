library(dplyr)

# this file will generate the inmate documents
source("prepareInmateDocument.r")

# load the inmage
Inmates = prepareInmateDocument()

# do something fun
levels(factor(Inmates$gender))
levels(factor(Inmates$eye_color))
levels(factor(Inmates$hair_color))
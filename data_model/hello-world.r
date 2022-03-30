library(dplyr)

# this file will generate the inmate documents
source("prepareInmateDocument.r")

# load the inmage
Inmates = prepareInmateDocument()

# data checks
levels(factor(Inmates$gender))
levels(factor(Inmates$eye_color))
levels(factor(Inmates$hair_color))
levels(factor(Inmates$race))
levels(factor(Inmates$native_county))
levels(factor(Inmates$native_state))
levels(factor(Inmates$education_level))

# dan is here, making more checks

levels(factor(Inmates$occupation))

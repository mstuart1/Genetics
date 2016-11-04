# a script to pull the old pit tag data from the june spreadsheet into the august spreadsheet where the pit tag data has been concatenated incorrectly.

clownfishcoltypes <- c("numeric", "date", "text", "numeric", "numeric", "numeric", "text", "numeric", "numeric", "numeric", "numeric", "text", "numeric", "text", "text", "numeric", "numeric", "numeric", "text", "text", "numeric", "numeric", "numeric", "text", "text", "numeric", "numeric", "numeric", "text", "text", "numeric", "numeric", "numeric", "text", "text", "numeric", "numeric", "numeric", "text", "text", "numeric", "numeric", "text", "text", "text", "text", "text", "text", "numeric", "numeric", "numeric", "numeric", "text")

june <- readxl::read_excel("~/Documents/Philippines/Surveys_2016/GPSSurveys2016_20160610MRS.xlsx", sheet = "Clownfish", col_names=TRUE, col_types = clownfishcoltypes)



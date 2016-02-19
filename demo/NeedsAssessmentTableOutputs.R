source("formhub.R")
ed_na <- formhubDownload("education", "ossap_na", "sc4l3up!")
ed_na@data <- ed_na@data[-which(duplicated(ed_na@data$lga)),]
ed_na <- replaceHeaderNamesWithLabels(ed_na)

library(reshape2)
ed_na_for_print <- melt(ed_na@data, id=c("What is the name of your LGA?"))
kura_print <- subset(ed_na_for_print, `What is the name of your LGA?`=='KURA')

library(R2HTML)
tmpfic=HTMLInitFile("~/Desktop/kura")
setwd("~/Desktop/Tony/Tony")
library(readr)
library(memisc)
library(tidyverse)
oe <- read_csv("OE test.csv")
oe <- oe[-c(1,2),-c(1:18)]
oe <- oe[,-c(19,22,25,28,31,34,37,40,43,46,49,52,55,58,61,64)]
oe <- data.frame(sapply(oe, function(x) as.numeric(as.character(x))))
oe[,1:16][is.na(oe[,1:16])] = "NA"

# recode to 0 (similar options) and 1 (dissimilar option)
for (i in 1:nrow(oe)){
  for (j in 1:16){
    if(oe[i,j] > 1 & oe[i,j] != "NA") {oe[i,j] <- 0}
  }
}

######### Similarity Effect #########
prop.table(table(oe$lot50s, exclude = "NA"))
prop.table(table(oe$lot60s, exclude = "NA"))
prop.table(table(oe$res30s, exclude = "NA"))
prop.table(table(oe$res10s, exclude = "NA"))
prop.table(table(oe$can66s, exclude = "NA"))
prop.table(table(oe$can33s, exclude = "NA"))
prop.table(table(oe$mov33s, exclude = "NA"))
prop.table(table(oe$mov66s, exclude = "NA"))

######### Outlier Effect #########
prop.table(table(oe$lot50o, exclude = "NA"))
prop.table(table(oe$lot60o, exclude = "NA"))
prop.table(table(oe$res30o, exclude = "NA"))
prop.table(table(oe$res10o, exclude = "NA"))
prop.table(table(oe$can66o, exclude = "NA"))
prop.table(table(oe$can33o, exclude = "NA"))
prop.table(table(oe$mov33o, exclude = "NA"))
prop.table(table(oe$mov66o, exclude = "NA"))


######### Attributes #########
mean(oe$pcan33o_1, na.rm = T)
mean(oe$pcan33o_2, na.rm = T)
mean(oe$pcan33s_1, na.rm = T)
mean(oe$pcan33s_2, na.rm = T)
mean(oe$pcan66o_1, na.rm = T)
mean(oe$pcan66o_2, na.rm = T)
mean(oe$pcan66s_1, na.rm = T)
mean(oe$pcan66s_2, na.rm = T)


mean(oe$plot50s_1, na.rm = T)
mean(oe$plot50s_2, na.rm = T)
mean(oe$plot50o_1, na.rm = T)
mean(oe$plot50o_2, na.rm = T)
mean(oe$plot60s_1, na.rm = T)
mean(oe$plot60s_2, na.rm = T)
mean(oe$plot60o_1, na.rm = T)
mean(oe$plot60o_2, na.rm = T)

mean(oe$pres10o_1, na.rm = T)
mean(oe$pres10o_2, na.rm = T)
mean(oe$pres30o_1, na.rm = T)
mean(oe$pres30o_2, na.rm = T)
mean(oe$pres10s_1, na.rm = T)
mean(oe$pres10s_2, na.rm = T)
mean(oe$pres30s_1, na.rm = T)
mean(oe$pres30s_2, na.rm = T)

mean(oe$pmov66o_1, na.rm = T)
mean(oe$pmov66o_2, na.rm = T)
mean(oe$pmov33o_1, na.rm = T)
mean(oe$pmov33o_2, na.rm = T)
mean(oe$pmov66s_1, na.rm = T)
mean(oe$pmov66s_2, na.rm = T)
mean(oe$pmov33s_1, na.rm = T)
mean(oe$pmov33s_2, na.rm = T)

# demographic
prop.table(table(oe$gender))
prop.table(table(oe$age))

           
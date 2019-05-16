setwd("~/Desktop/Tony/Tony")
library(readr)
library(memisc)
library(tidyverse)
library(lme4)
oe <- read_csv("OE test.csv")
oe <- oe[-c(1,2),-c(1:18)]
oe <- oe[,-c(19,22,25,28,31,34,37,40,43,46,49,52,55,58,61,64)]
oe <- data.frame(sapply(oe, function(x) as.numeric(as.character(x))))

# remove participant 215 because didn't rate the attributes
oe <- oe[-c(215),]

#oe[,1:16][is.na(oe[,1:16])] = "NA"

# # recode to 0 (similar options) and 1 (dissimilar option)
# for (i in 1:nrow(oe)){
#   for (j in 1:16){
#     if(oe[i,j] > 1 & oe[i,j] != "NA") {oe[i,j] <- 0}
#   }
# }

######### Similarity Effect #########
prop.table(table(na.omit(oe$lot50s)))
prop.table(table(na.omit(oe$lot60s)))
prop.table(table(na.omit(oe$res30s)))
prop.table(table(na.omit(oe$res10s)))
prop.table(table(na.omit(oe$can66s)))
prop.table(table(na.omit(oe$can33s)))
prop.table(table(na.omit(oe$mov33s)))
prop.table(table(na.omit(oe$mov66s)))

######### Outlier Effect #########
prop.table(table(na.omit(oe$lot50o)))
prop.table(table(na.omit(oe$lot60o)))
prop.table(table(na.omit(oe$res30o)))
prop.table(table(na.omit(oe$res10o)))
prop.table(table(na.omit(oe$can66o)))
prop.table(table(na.omit(oe$can33o)))
prop.table(table(na.omit(oe$mov33o)))
prop.table(table(na.omit(oe$mov66o)))


######### Attributes #########
# 1 = IQ
# 2 = EQ
mean(oe$pcan33o_1, na.rm = T)
mean(oe$pcan33o_2, na.rm = T)
mean(oe$pcan33s_1, na.rm = T)
mean(oe$pcan33s_2, na.rm = T)
mean(oe$pcan66o_1, na.rm = T)
mean(oe$pcan66o_2, na.rm = T)
mean(oe$pcan66s_1, na.rm = T)
mean(oe$pcan66s_2, na.rm = T)

# 1 = prob of winning
# 2 = amount of winning
mean(oe$plot50s_1, na.rm = T)
mean(oe$plot50s_2, na.rm = T)
mean(oe$plot50o_1, na.rm = T)
mean(oe$plot50o_2, na.rm = T)
mean(oe$plot60s_1, na.rm = T)
mean(oe$plot60s_2, na.rm = T)
mean(oe$plot60o_1, na.rm = T)
mean(oe$plot60o_2, na.rm = T)

# 1 = food quality
# 2 = driving time
mean(oe$pres10o_1, na.rm = T)
mean(oe$pres10o_2, na.rm = T)
mean(oe$pres30o_1, na.rm = T)
mean(oe$pres30o_2, na.rm = T)
mean(oe$pres10s_1, na.rm = T)
mean(oe$pres10s_2, na.rm = T)
mean(oe$pres30s_1, na.rm = T)
mean(oe$pres30s_2, na.rm = T)


# 1 = critic I
# 2 = critic II
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


# create a new data frame
# dummy code for scenarios
# ef <- data.frame(resp = c(oe$lot50s, oe$lot60s, oe$lot50o, oe$lot60o,
#                           oe$res10s, oe$res30s, oe$res10o, oe$res30o,
#                           oe$can33s, oe$can66s, oe$can33o, oe$can66o,
#                           oe$mov33s, oe$mov66s, oe$mov33o, oe$mov66o),
#                  name = (c(rep(c("lot50s"), length(oe$lot50s)),
#                          rep(c("lot60s"), length(oe$lot60s)),
#                          rep(c("lot50o"), length(oe$lot50o)),
#                          rep(c("lot60o"), length(oe$lot60o)),
#                          rep(c("res10s"), length(oe$res10s)),
#                          rep(c("res30s"), length(oe$res30s)),
#                          rep(c("res10o"), length(oe$res10o)),
#                          rep(c("res30o"), length(oe$res30o)),
#                          rep(c("can33s"), length(oe$can33s)),
#                          rep(c("can66s"), length(oe$can66s)),
#                          rep(c("can33o"), length(oe$can33o)),
#                          rep(c("can66o"), length(oe$can66o)),
#                          rep(c("mov33s"), length(oe$mov33s)),
#                          rep(c("mov66s"), length(oe$mov66s)),
#                          rep(c("mov33o"), length(oe$mov33o)),
#                          rep(c("mov66o"), length(oe$mov66o)))))
# 
# ef$lot.mov <- factor(c(rep(1, nrow(oe)), rep(0, nrow(oe)), rep(0, nrow(oe)), rep(0, nrow(oe))))   
# ef$res.mov <- factor(c(rep(0, nrow(oe)), rep(1, nrow(oe)), rep(0, nrow(oe)), rep(0, nrow(oe)))) 
# ef$can.mov <- factor(c(rep(0, nrow(oe)), rep(0, nrow(oe)), rep(1, nrow(oe)), rep(0, nrow(oe)))) 

# dummy code for effects
# 0 = similarity 
# 1 = outlier
# ef$effect <- NA
# for (i in 1:nrow(ef)){
#   if (ef$name[i] == "lot50s" | ef$name[i] == "lot60s" | ef$name[i] == "res10s" | ef$name[i] == "res30s" |
#       ef$name[i] == "can33s" | ef$name[i] == "can66s" | ef$name[i] == "mov33s" | ef$name[i] == "mov66s")
#     {ef$effect[i] <- 0} else {ef$effect[i] <- 1}
# }


# dummy code for presentation: base on attribute weighting -- whether the option that has the highest value on that prominent attribute is a dissimilar option or a similar option
# For lot: probability of winning > amount of winning
# For res: food quality > driving time
# For can: EQ > IQ
# For mov: critic I > critic II 
# code = 0 if the "more attractive" option is a dissimilar option
# code = 1 if it is a similar option

# ef$pre <- NA
# for (i in 1:nrow(ef)){
#   if (ef$name[i] == "lot60s" | ef$name[i] == "lot60o" | ef$name[i] == "res30s" | ef$name[i] == "res30o" |
#       ef$name[i] == "can33s" | ef$name[i] == "can33o" | ef$name[i] == "mov66s" | ef$name[i] == "mov66o")
#   {ef$pre[i] <- 0} else {ef$pre[i] <- 1}
# }

# listwise deletion for NA responses
# dt <- na.omit(ef)

# recode to 0 (dissimilar options) and 1 (similar option)
# for (i in 1:nrow(dt)){
#     if(dt$resp[i] > 1) {dt$resp[i] <- 0}
# }
# dt$resp <- 1-dt$resp

# adding scenario names
# dt$scenario <- NA
# for (i in 1:nrow(dt)){
#   if (dt$name[i] == "lot50s" | dt$name[i] == "lot60s" | dt$name[i] == "lot50o" | dt$name[i] == "lot60o")
#   {dt$scenario[i] <- "gamble"}
#   if (dt$name[i] == "mov33s" | dt$name[i] == "mov33o" | dt$name[i] == "mov66s" | dt$name[i] == "mov66o")
#   {dt$scenario[i] <- "movie"}
#   if (dt$name[i] == "can33s" | dt$name[i] == "can33o" | dt$name[i] == "can66s" | dt$name[i] == "can66o")
#   {dt$scenario[i] <- "candidate"} 
#   if (dt$name[i] == "res10s" | dt$name[i] == "res10o" | dt$name[i] == "res30s" | dt$name[i] == "res30o")
#   {dt$scenario[i] <- "restaurant"}
# }

# logistic regression
# mod1 <- glm(resp ~ lot.mov + res.mov + can.mov + effect + lot.mov*effect + res.mov*effect + can.mov*effect, family = binomial(link="logit"), data = dt)
# summary(mod1) # doesn't work


############## create a data frame for each scenario ###############
################## gambling ##################
gb <- data.frame(resp = c(oe$lot50s, oe$lot60s, oe$lot50o, oe$lot60o),
                 name = (c(rep(c("lot50s"), length(oe$lot50s)),
                           rep(c("lot60s"), length(oe$lot60s)),
                           rep(c("lot50o"), length(oe$lot50o)),
                           rep(c("lot60o"), length(oe$lot60o)))),
                 att1 = c(oe$plot50s_1, oe$plot60s_1, oe$plot50o_1, oe$plot60o_1),
                 att2 = c(oe$plot50s_2, oe$plot60s_2, oe$plot50o_2, oe$plot60o_2))

# dummy code for presentation: base on attribute weighting -- whether the option that has the highest value on that prominent attribute is a dissimilar option or a similar option
# For lot: probability of winning > amount of winning
# For res: food quality > driving time
# For can: EQ > IQ
# For mov: critic I > critic II 
# code = 0 if the "more attractive" option is a dissimilar option
# code = 1 if it is a similar option
gb$pre <- NA
for (i in 1:nrow(gb)){
  if (gb$name[i] == "lot60s" | gb$name[i] == "lot60o")
  {gb$pre[i] <- 0} else {gb$pre[i] <- 1}
}

# dummy code for effects
# 0 = similarity 
# 1 = outlier
gb$effect <- NA
for (i in 1:nrow(gb)){
  if (gb$name[i] == "lot50s" | gb$name[i] == "lot60s")
  {gb$effect[i] <- 0} else {gb$effect[i] <- 1}
}



gb <- na.omit(gb)

# recode to 0 (dissimilar options) and 1 (similar option)
for (i in 1:nrow(gb)){
  if(gb$resp[i] > 1) {gb$resp[i] <- 0}
}
gb$resp <- 1-gb$resp

gambling <- glm(resp ~ effect + pre, family = binomial(link="logit"), data = gb)
summary(gambling)
coef(gambling)

# convert logit to prob
logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}
logit2prob(coef(gambling))



################# restaurant ##################
rt <- data.frame(resp = c(oe$res10s, oe$res30s, oe$res10o, oe$res30o),
                 name = (c(rep(c("res10s"), length(oe$res10s)),
                           rep(c("res30s"), length(oe$res30s)),
                           rep(c("res10o"), length(oe$res10o)),
                           rep(c("res30o"), length(oe$res30o)))),
                 att1 = c(oe$pres10s_1, oe$pres30s_1, oe$pres10o_1, oe$pres30o_1),
                 att2 = c(oe$pres10s_2, oe$pres30s_2, oe$pres10o_2, oe$pres30o_2))

rt$pre <- NA # presentation
for (i in 1:nrow(rt)){
  if (rt$name[i] == "res30s" | rt$name[i] == "res30o")
  {rt$pre[i] <- 0} else {rt$pre[i] <- 1}
}

rt$effect <- NA
for (i in 1:nrow(rt)){
  if (rt$name[i] == "res30s" | rt$name[i] == "res10s")
  {rt$effect[i] <- 0} else {rt$effect[i] <- 1}
}

rt <- na.omit(rt)

# recode to 0 (dissimilar options) and 1 (similar option)
for (i in 1:nrow(rt)){
  if(rt$resp[i] > 1) {rt$resp[i] <- 0}
}
rt$resp <- 1-rt$resp

restaurant <- glm(resp ~ effect + pre, family = binomial(link="logit"), data = rt)
summary(restaurant)
logit2prob(coef(restaurant))


############# candidate #####################
cd <- data.frame(resp = c(oe$can33s, oe$can66s, oe$can33o, oe$can66o),
                 name = (c(rep(c("can33s"), length(oe$can33s)),
                           rep(c("can66s"), length(oe$can66s)),
                           rep(c("can33o"), length(oe$can33o)),
                           rep(c("can66o"), length(oe$can66o)))),
                 att1 = c(oe$pcan33s_1, oe$pcan66s_1, oe$pcan33o_1, oe$pcan66o_1),
                 att2 = c(oe$pcan33s_2, oe$pcan66s_2, oe$pcan33o_2, oe$pcan66o_2))

cd$pre <- NA # presentation
for (i in 1:nrow(cd)){
  if (cd$name[i] == "can33s" | cd$name[i] == "can33o")
  {cd$pre[i] <- 0} else {cd$pre[i] <- 1}
}

cd$effect <- NA
for (i in 1:nrow(cd)){
  if (cd$name[i] == "can66s" | cd$name[i] == "can33s")
  {cd$effect[i] <- 0} else {cd$effect[i] <- 1}
}

cd <- na.omit(cd)

# recode to 0 (dissimilar options) and 1 (similar option)
for (i in 1:nrow(cd)){
  if(cd$resp[i] > 1) {cd$resp[i] <- 0}
}
cd$resp <- 1-cd$resp

candidate <- glm(resp ~ effect + pre, family = binomial(link="logit"), data = cd)
summary(candidate)
logit2prob(coef(candidate))




############# movie #####################
mv <- data.frame(resp = c(oe$mov33s, oe$mov66s, oe$mov33o, oe$mov66o),
                 name = (c(rep(c("mov33s"), length(oe$mov33s)),
                           rep(c("mov66s"), length(oe$mov66s)),
                           rep(c("mov33o"), length(oe$mov33o)),
                           rep(c("mov66o"), length(oe$mov66o)))),
                 att1 = c(oe$pmov33s_1, oe$pmov66s_1, oe$pmov33o_1, oe$pmov66o_1),
                 att2 = c(oe$pmov33s_2, oe$pmov66s_2, oe$pmov33o_2, oe$pmov66o_2))

mv$pre <- NA # presentation
for (i in 1:nrow(mv)){
  if (mv$name[i] == "mov66s" | mv$name[i] == "mov66o")
  {mv$pre[i] <- 0} else {mv$pre[i] <- 1}
}

mv$effect <- NA
for (i in 1:nrow(mv)){
  if (mv$name[i] == "mov66s" | mv$name[i] == "mov33s")
  {mv$effect[i] <- 0} else {mv$effect[i] <- 1}
}

mv <- na.omit(mv)

# recode to 0 (dissimilar options) and 1 (similar option)
for (i in 1:nrow(mv)){
  if(mv$resp[i] > 1) {mv$resp[i] <- 0}
}
mv$resp <- 1-mv$resp

movie <- glm(resp ~ effect + pre + effect*pre + att1 + att2, family = binomial(link="logit"), data = mv)
summary(movie)
logit2prob(coef(movie))


#######
# for the can33o, I have to remove first 17 participants due to survey errors 
can2 <- data.frame(can33o = na.omit(oe$can33o), pcan33o_1 = na.omit(oe$pcan33o_1), pcan33o_2 = na.omit(oe$pcan33o_2))
can2 <- can2[-c(1:17),]

# replace in the new data frame
cd1 <- data.frame(resp = c(oe$can33s, oe$can66s, can2$can33o, oe$can66o),
                 name = (c(rep(c("can33s"), length(oe$can33s)),
                           rep(c("can66s"), length(oe$can66s)),
                           rep(c("can33o"), length(can2$can33o)),
                           rep(c("can66o"), length(oe$can66o)))),
                 att1 = c(oe$pcan33s_1, oe$pcan66s_1, can2$pcan33o_1, oe$pcan66o_1),
                 att2 = c(oe$pcan33s_2, oe$pcan66s_2, can2$pcan33o_2, oe$pcan66o_2))

cd1$pre <- NA # presentation
for (i in 1:nrow(cd1)){
  if (cd1$name[i] == "can33s" | cd1$name[i] == "can33o")
  {cd1$pre[i] <- 0} else {cd1$pre[i] <- 1}
}

cd1$effect <- NA
for (i in 1:nrow(cd1)){
  if (cd1$name[i] == "can66s" | cd1$name[i] == "can33s")
  {cd1$effect[i] <- 0} else {cd1$effect[i] <- 1}
}

cd1 <- na.omit(cd1)

# recode to 0 (dissimilar options) and 1 (similar option)
for (i in 1:nrow(cd1)){
  if(cd1$resp[i] > 1) {cd1$resp[i] <- 0}
}
cd1$resp <- 1-cd1$resp

candidate1 <- glm(resp ~ effect + pre, family = binomial(link="logit"), data = cd1)
summary(candidate1)
logit2prob(coef(candidate1))



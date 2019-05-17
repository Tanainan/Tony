setwd("~/Desktop/Tony/Tony")
library(readr)
library(memisc)
library(tidyverse)
library(lme4)
library(stats)
library(car)
oe <- read_csv("OE test.csv")
oe <- oe[-c(1,2),-c(1:18)]
oe <- oe[,-c(19,22,25,28,31,34,37,40,43,46,49,52,55,58,61,64)]
oe <- data.frame(sapply(oe, function(x) as.numeric(as.character(x))))
# change column names
colnames(oe)[colnames(oe)=="res30o"] <- "res40o"
colnames(oe)[colnames(oe)=="res30s"] <- "res40s"
colnames(oe)[colnames(oe)=="pres30o_1"] <- "pres40o_1"
colnames(oe)[colnames(oe)=="pres30o_2"] <- "pres40o_2"
colnames(oe)[colnames(oe)=="pres30s_1"] <- "pres40s_1"
colnames(oe)[colnames(oe)=="pres30s_2"] <- "pres40s_2"

# remove part0icipant 215 because didn't rate the attributes for all scenarios
oe <- oe[-c(215),]

#oe[,1:16][is.na(oe[,1:16])] = "NA"

# # recode to 0 (similar options) and 1 (dissimilar option)
# for (i in 1:nrow(oe)){
#   for (j in 1:16){
#     if(oe[i,j] > 1 & oe[i,j] != "NA") {oe[i,j] <- 0}
#   }
# }

######### Similarity Effect #########
# prop.table(table(na.omit(oe$lot50s)))
# prop.table(table(na.omit(oe$lot60s)))
# prop.table(table(na.omit(oe$res40s)))
# prop.table(table(na.omit(oe$res10s)))
# prop.table(table(na.omit(oe$can66s)))
# prop.table(table(na.omit(oe$can33s)))
# prop.table(table(na.omit(oe$mov33s)))
# prop.table(table(na.omit(oe$mov66s)))

######### Outlier Effect #########
# prop.table(table(na.omit(oe$lot50o)))
# prop.table(table(na.omit(oe$lot60o)))
# prop.table(table(na.omit(oe$res40o)))
# prop.table(table(na.omit(oe$res10o)))
# prop.table(table(na.omit(oe$can66o)))
# prop.table(table(na.omit(oe$can33o)))
# prop.table(table(na.omit(oe$mov33o)))
# prop.table(table(na.omit(oe$mov66o)))


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

# 1 = driving time
# 2 = food quality
mean(oe$pres10o_1, na.rm = T)
mean(oe$pres10o_2, na.rm = T)
mean(oe$pres40o_1, na.rm = T)
mean(oe$pres40o_2, na.rm = T)
mean(oe$pres10s_1, na.rm = T)
mean(oe$pres10s_2, na.rm = T)
mean(oe$pres40s_1, na.rm = T)
mean(oe$pres40s_2, na.rm = T)


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




############## create a data frame for each scenario ###############
################## gambling ##################
gb0 <- data.frame(resp = c(oe$lot50s, oe$lot60s, oe$lot50o, oe$lot60o),
                 name = (c(rep(c("lot50s"), length(oe$lot50s)),
                           rep(c("lot60s"), length(oe$lot60s)),
                           rep(c("lot50o"), length(oe$lot50o)),
                           rep(c("lot60o"), length(oe$lot60o)))),
                 att1 = c(oe$plot50s_1, oe$plot60s_1, oe$plot50o_1, oe$plot60o_1),
                 att2 = c(oe$plot50s_2, oe$plot60s_2, oe$plot50o_2, oe$plot60o_2))

gb0 <- na.omit(gb0)
# dummy code for presentation: base on attribute weighting -- whether the option that has the highest value on that prominent attribute is a dissimilar option or a similar option
# For lot: probability of winning > amount of winning
# For res: food quality > driving time
# For can: EQ > IQ
# For mov: critic I > critic II 
########## Use Helmert0 coding ##########
# code = -0.5 if the "more attractive" option is a dissimilar option
# code = 0.5 if it is a similar option
gb0$pre <- NA
for (i in 1:nrow(gb0)){
  if (gb0$name[i] == "lot60s" | gb0$name[i] == "lot60o")
  {gb0$pre[i] <- -0.5} else {gb0$pre[i] <- 0.5}
}

# dummy code for effects
# 0 = similarity 
# 1 = outlier
gb0$effect <- NA
for (i in 1:nrow(gb0)){
  if (gb0$name[i] == "lot50s" | gb0$name[i] == "lot60s")
  {gb0$effect[i] <- 0} else {gb0$effect[i] <- 1}
}



# recode to 0 (dissimilar options) and 1 (similar option)
for (i in 1:nrow(gb0)){
  if(gb0$resp[i] > 1) {gb0$resp[i] <- 0}
}
gb0$resp <- 1-gb0$resp

nrow(gb0[which(gb0$effect == 0),])
nrow(gb0[which(gb0$effect == 1),])

gambling <- glm(resp ~ effect + pre, family = binomial(link="logit"), data = gb0)
summary(gambling)


# convert0 logit to prob
logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}
logit2prob(coef(gambling))


# test for correlation between presentation and att1-att2
gb0$attdiff <- gb0$att1 - gb0$att2
cor.test(gb0$attdiff, gb0$pre)
mean(gb0$attdiff)

################# restaurant ##################
rt0 <- data.frame(resp = c(oe$res10s, oe$res40s, oe$res10o, oe$res40o),
                 name = (c(rep(c("res10s"), length(oe$res10s)),
                           rep(c("res40s"), length(oe$res40s)),
                           rep(c("res10o"), length(oe$res10o)),
                           rep(c("res40o"), length(oe$res40o)))),
                 att1 = c(oe$pres10s_1, oe$pres40s_1, oe$pres10o_1, oe$pres40o_1),
                 att2 = c(oe$pres10s_2, oe$pres40s_2, oe$pres10o_2, oe$pres40o_2))

rt0 <- na.omit(rt0)

rt0$pre <- NA # presentation
for (i in 1:nrow(rt0)){
  if (rt0$name[i] == "res40s" | rt0$name[i] == "res40o")
  {rt0$pre[i] <- -0.5} else {rt0$pre[i] <- 0.5}
}

rt0$effect <- NA
for (i in 1:nrow(rt0)){
  if (rt0$name[i] == "res40s" | rt0$name[i] == "res10s")
  {rt0$effect[i] <- 0} else {rt0$effect[i] <- 1}
}

# recode to 0 (dissimilar options) and 1 (similar option)
for (i in 1:nrow(rt0)){
  if(rt0$resp[i] > 1) {rt0$resp[i] <- 0}
}
rt0$resp <- 1-rt0$resp

nrow(rt0[which(rt0$effect == 0),])
nrow(rt0[which(rt0$effect == 1),])

restaurant <- glm(resp ~ effect + pre, family = binomial(link="logit"), data = rt0)
summary(restaurant)
logit2prob(coef(restaurant))


rt0$attdiff <- rt0$att1 - rt0$att2
cor.test(rt0$attdiff, rt0$pre)
mean(rt0$attdiff)

############# candidate #####################
cd0 <- data.frame(resp = c(oe$can33s, oe$can66s, oe$can33o, oe$can66o),
                 name = (c(rep(c("can33s"), length(oe$can33s)),
                           rep(c("can66s"), length(oe$can66s)),
                           rep(c("can33o"), length(oe$can33o)),
                           rep(c("can66o"), length(oe$can66o)))),
                 att1 = c(oe$pcan33s_1, oe$pcan66s_1, oe$pcan33o_1, oe$pcan66o_1),
                 att2 = c(oe$pcan33s_2, oe$pcan66s_2, oe$pcan33o_2, oe$pcan66o_2))

cd0 <- na.omit(cd0)

cd0$pre <- NA # presentation
for (i in 1:nrow(cd0)){
  if (cd0$name[i] == "can33s" | cd0$name[i] == "can33o")
  {cd0$pre[i] <- -0.5} else {cd0$pre[i] <- 0.5}
}

cd0$effect <- NA
for (i in 1:nrow(cd0)){
  if (cd0$name[i] == "can66s" | cd0$name[i] == "can33s")
  {cd0$effect[i] <- 0} else {cd0$effect[i] <- 1}
}


# recode to 0 (dissimilar options) and 1 (similar option)
for (i in 1:nrow(cd0)){
  if(cd0$resp[i] > 1) {cd0$resp[i] <- 0}
}
cd0$resp <- 1-cd0$resp

nrow(cd0[which(cd0$effect == 0),])
nrow(cd0[which(cd0$effect == 1),])

candidate <- glm(resp ~ effect + pre, family = binomial(link="logit"), data = cd0)
summary(candidate)
logit2prob(coef(candidate))

cd0$attdiff <- cd0$att1 - cd0$att2
cor.test(cd0$attdiff, cd0$pre)
mean(cd0$attdiff)

############# movie #####################
mv0 <- data.frame(resp = c(oe$mov33s, oe$mov66s, oe$mov33o, oe$mov66o),
                 name = (c(rep(c("mov33s"), length(oe$mov33s)),
                           rep(c("mov66s"), length(oe$mov66s)),
                           rep(c("mov33o"), length(oe$mov33o)),
                           rep(c("mov66o"), length(oe$mov66o)))),
                 att1 = c(oe$pmov33s_1, oe$pmov66s_1, oe$pmov33o_1, oe$pmov66o_1),
                 att2 = c(oe$pmov33s_2, oe$pmov66s_2, oe$pmov33o_2, oe$pmov66o_2))
mv0 <- na.omit(mv0)

mv0$pre <- NA # presentation
for (i in 1:nrow(mv0)){
  if (mv0$name[i] == "mov66s" | mv0$name[i] == "mov66o")
  {mv0$pre[i] <- -0.5} else {mv0$pre[i] <- 0.5}
}

mv0$effect <- NA
for (i in 1:nrow(mv0)){
  if (mv0$name[i] == "mov66s" | mv0$name[i] == "mov33s")
  {mv0$effect[i] <- 0} else {mv0$effect[i] <- 1}
}

mv0 <- na.omit(mv0)


# recode to 0 (dissimilar options) and 1 (similar option)
for (i in 1:nrow(mv0)){
  if(mv0$resp[i] > 1) {mv0$resp[i] <- 0}
}
mv0$resp <- 1-mv0$resp

nrow(mv0[which(mv0$effect == 0),])
nrow(mv0[which(mv0$effect == 1),])

movie <- glm(resp ~ effect + pre + effect*pre + att1 + att2, family = binomial(link="logit"), data = mv0)
movie1 <- glm(resp ~ effect + pre, family = binomial(link="logit"), data = mv0)
summary(movie)
summary(movie1)
logit2prob(coef(movie))

mv0$attdiff <- mv0$att1 - mv0$att2
cor.test(mv0$attdiff, mv0$pre)
mean(mv0$attdiff)

#######
# for the can33o, I have to remove first 17 part0icipants due to survey errors 
can2 <- data.frame(can33o = na.omit(oe$can33o), pcan33o_1 = na.omit(oe$pcan33o_1), pcan33o_2 = na.omit(oe$pcan33o_2))
can2 <- can2[-c(1:17),]

# replace in the new data frame
cd01 <- data.frame(resp = c(oe$can33s, oe$can66s, can2$can33o, oe$can66o),
                 name = (c(rep(c("can33s"), length(oe$can33s)),
                           rep(c("can66s"), length(oe$can66s)),
                           rep(c("can33o"), length(can2$can33o)),
                           rep(c("can66o"), length(oe$can66o)))),
                 att1 = c(oe$pcan33s_1, oe$pcan66s_1, can2$pcan33o_1, oe$pcan66o_1),
                 att2 = c(oe$pcan33s_2, oe$pcan66s_2, can2$pcan33o_2, oe$pcan66o_2))

cd01$pre <- NA # presentation
for (i in 1:nrow(cd01)){
  if (cd01$name[i] == "can33s" | cd01$name[i] == "can33o")
  {cd01$pre[i] <- -0.5} else {cd01$pre[i] <- 0.5}
}

cd01$effect <- NA
for (i in 1:nrow(cd01)){
  if (cd01$name[i] == "can66s" | cd01$name[i] == "can33s")
  {cd01$effect[i] <- 0} else {cd01$effect[i] <- 1}
}

cd01 <- na.omit(cd01)

# recode to 0 (dissimilar options) and 1 (similar option)
for (i in 1:nrow(cd01)){
  if(cd01$resp[i] > 1) {cd01$resp[i] <- 0}
}
cd01$resp <- 1-cd01$resp

nrow(cd01[which(cd01$effect == 0),])
nrow(cd01[which(cd01$effect == 1),])

candidate1 <- glm(resp ~ effect + pre, family = binomial(link="logit"), data = cd01)
summary(candidate1)
logit2prob(coef(candidate1))

cd01$attdiff <- cd01$att1 - cd01$att2
cor.test(cd01$attdiff, cd01$pre)
mean(cd01$attdiff)


####### find the raw probability of each scenario ########
# histogram(gb0[which(gb0$effect == 0), c("resp")])
# histogram(gb0[which(gb0$effect == 1), c("resp")])
# 
# histogram(rt0[which(rt0$effect == 0), c("resp")])
# histogram(rt0[which(rt0$effect == 1), c("resp")])
# 
# histogram(cd0[which(cd0$effect == 0), c("resp")])
# histogram(cd0[which(cd0$effect == 1), c("resp")])
# 
# histogram(mv0[which(mv0$effect == 0), c("resp")])
# histogram(mv0[which(mv0$effect == 1), c("resp")])

#### 0 as % dissimilar option being chosen ####
# gambling
prop.table(table(gb0[which(gb0$effect == 0), c("resp")]))[1] # similarity
prop.table(table(gb0[which(gb0$effect == 1), c("resp")]))[1] # outlier

# restaurant
prop.table(table(rt0[which(rt0$effect == 0), c("resp")]))[1] # similarity
prop.table(table(rt0[which(rt0$effect == 1), c("resp")]))[1] # outlier

# candidate
prop.table(table(cd0[which(cd0$effect == 0), c("resp")]))[1] # similarity
prop.table(table(cd0[which(cd0$effect == 1), c("resp")]))[1] # outlier

# movie
prop.table(table(mv0[which(mv0$effect == 0), c("resp")]))[1] # similarity
prop.table(table(mv0[which(mv0$effect == 1), c("resp")]))[1] # outlier

##### test if raw proport0ion of dissimilar option is different from 0.33 and 0.125
# gambling
binom.test(nrow(gb0[which(gb0$effect == 0 & gb0$resp == 0),]), nrow(gb0[which(gb0$effect == 0),]), p = 0.33, alternative = "greater")
binom.test(nrow(gb0[which(gb0$effect == 1 & gb0$resp == 0),]), nrow(gb0[which(gb0$effect == 1),]), p = 0.125, alternative = "less")

# restaurant
binom.test(nrow(rt0[which(rt0$effect == 0 & rt0$resp == 0),]), nrow(rt0[which(rt0$effect == 0),]), p = 0.33, alternative = "greater")
binom.test(nrow(rt0[which(rt0$effect == 1 & rt0$resp == 0),]), nrow(rt0[which(rt0$effect == 1),]), p = 0.125, alternative = "less")

# candidate
binom.test(nrow(cd0[which(cd0$effect == 0 & cd0$resp == 0),]), nrow(cd0[which(cd0$effect == 0),]), p = 0.33, alternative = "greater")
binom.test(nrow(cd0[which(cd0$effect == 1 & cd0$resp == 0),]), nrow(cd0[which(cd0$effect == 1),]), p = 0.125, alternative = "less")

# movie
binom.test(nrow(mv0[which(mv0$effect == 0 & mv0$resp == 0),]), nrow(mv0[which(mv0$effect == 0),]), p = 0.33, alternative = "greater")
binom.test(nrow(mv0[which(mv0$effect == 1 & mv0$resp == 0),]), nrow(mv0[which(mv0$effect == 1),]), p = 0.125, alternative = "less")


####### only test for either similarity effect or outlier effect, controlling for presentation ######
# gambling similarity
gb0s <- gb0[which(gb0$effect == 0),]
gb0s1 <- glm(resp ~ pre, data = gb0s, family = binomial(link="logit"))
summary(gb0s1)
logit2prob(coef(gb0s1)[1]) # dissimilar = 0 and similar = 1
linearHypothesis(gb0s1, "(Intercept) = 0.70818505792") # the coefficients for intercept and when presentation is 0 and 1 (average), comparing with a logit of 0.67
gb0sc <- (exp(coef(gb0s1)[1])/(1+exp(coef(gb0s1)[1])))
zgb0s <- (gb0sc - 0.67)/sqrt(gb0sc*(1-gb0sc)/nrow(gb0s)) # the formula is from http://www.sthda.com/english/wiki/one-proport0ion-z-test-in-r
2*pnorm(-abs(zgb0s))

gb0s2 <- glm(resp ~ attdiff*pre, data = gbs, family = binomial(link="logit"))
summary(gb0s2)
gbsc2 <- (exp(coef(gb0s2)[1] + coef(gb0s2)[2]/2)/(1+exp(coef(gb0s2)[1] + coef(gb0s2)[2]/2)))
zgb0s2 <- (gbsc2 - 0.67)/sqrt(gbsc2*(1-gbsc2)/nrow(gbs)) # the formula is from http://www.sthda.com/english/wiki/one-proportion-z-test-in-r
2*pnorm(-abs(zgb0s2))

# gambling outlier
gb0o <- gb0[which(gb0$effect == 1),]
gb0o1 <- glm(resp ~ pre, data = gb0o, family = binomial(link="logit"))
summary(gb0o1)
logit2prob(coef(gb0o1)[1]) # dissimilar = 0 and similar = 1
linearHypothesis(gb0o1, "(Intercept) = 1.94591014906") # comparing with a logit of 0.875
gb0oc <- (exp(coef(gb0o1)[1])/(1+exp(coef(gb0o1)[1])))
zgb0o <- (gb0oc - 0.875)/sqrt(gb0oc*(1-gb0oc)/nrow(gb0o))
2*pnorm(-abs(zgb0o))

gb0o2 <- glm(resp ~ pre*attdiff, data = gbo, family = binomial(link="logit"))
summary(gb0o2)
gboc2 <- (exp(coef(gb0o2)[1] + coef(gb0o2)[2]/2)/(1+exp(coef(gb0o2)[1] + coef(gb0o2)[2]/2)))
zgb0o2 <- (gboc2 - 0.875)/sqrt(gboc2*(1-gboc2)/nrow(gbo))
2*pnorm(-abs(zgb0o2))

# restaurant similarity
rt0s <- rt0[which(rt0$effect == 0),]
rt0s1 <- glm(resp ~ pre, data = rt0s, family = binomial(link="logit"))
summary(rt0s1)
logit2prob(coef(rt0s1)[1]) # dissimilar = 0 and similar = 1
linearHypothesis(rt0s1, "(Intercept) = 0.70818505792")
rt0sc <- (exp(coef(rt0s1)[1])/(1+exp(coef(rt0s1)[1])))
zrt0s <- (rt0sc - 0.67)/sqrt(rt0sc*(1-rt0sc)/nrow(rt0s))
2*pnorm(-abs(zrt0s))

rt0s2 <- glm(resp ~ pre*attdiff, data = rts, family = binomial(link="logit"))
summary(rt0s2)
rtsc2 <- (exp(coef(rt0s2)[1] + coef(rt0s2)[2]/2)/(1+exp(coef(rt0s2)[1] + coef(rt0s2)[2]/2)))
zrt0s2 <- (rtsc2 - 0.67)/sqrt(rtsc2*(1-rtsc2)/nrow(rts))
2*pnorm(-abs(zrt0s2))

# restaurant outlier
rt0o <- rt0[which(rt0$effect == 1),]
rt0o1 <- glm(resp ~ pre, data = rt0o, family = binomial(link="logit"))
summary(rt0o1)
logit2prob(coef(rt0o1)[1]) # dissimilar = 0 and similar = 1
linearHypothesis(rt0o1, "(Intercept) = 1.94591014906")
rt0oc <- (exp(coef(rt0o1)[1])/(1+exp(coef(rt0o1)[1])))
zrt0o <- (rt0oc - 0.875)/sqrt(rt0oc*(1-rt0oc)/nrow(rt0o))
2*pnorm(-abs(zrt0o))

rt0o2 <- glm(resp ~ pre*attdiff, data = rto, family = binomial(link="logit"))
summary(rt0o2)
rtoc2 <- (exp(coef(rt0o2)[1] + coef(rt0o2)[2]/2)/(1+exp(coef(rt0o2)[1] + coef(rt0o2)[2]/2)))
zrt0o2 <- (rtoc2 - 0.875)/sqrt(rtoc2*(1-rtoc2)/nrow(rto))
2*pnorm(-abs(zrt0o2))

# candidate similarity
cd0s <- cd0[which(cd0$effect == 0),]
cd0s1 <- glm(resp ~ pre, data = cd0s, family = binomial(link="logit"))
summary(cd0s1)
logit2prob(coef(cd0s1)[1]) # dissimilar = 0 and similar = 1
linearHypothesis(cd0s1, "(Intercept) = 0.70818505792")
cd0sc <- (exp(coef(cd0s1)[1])/(1+exp(coef(cd0s1)[1])))
zcd0s <- (cd0sc - 0.67)/sqrt(cd0sc*(1-cd0sc)/nrow(cd0s))
2*pnorm(-abs(zcd0s))

cd0s2 <- glm(resp ~ pre*attdiff, data = cds, family = binomial(link="logit"))
summary(cd0s2)
cdsc2 <- (exp(coef(cd0s2)[1] + coef(cd0s2)[2]/2)/(1+exp(coef(cd0s2)[1] + coef(cd0s2)[2]/2)))
zcd0s2 <- (cdsc2 - 0.67)/sqrt(cdsc2*(1-cdsc2)/nrow(cds))
2*pnorm(-abs(zcd0s2))

# candidate outlier
cd0o <- cd0[which(cd0$effect == 1),]
cd0o1 <- glm(resp ~ pre, data = cd0o, family = binomial(link="logit"))
summary(cd0o1)
logit2prob(coef(cd0o1)[1]) # dissimilar = 0 and similar = 1
linearHypothesis(cd0o1, "(Intercept)= 1.94591014906")
cd0oc <- (exp(coef(cd0o1)[1])/(1+exp(coef(cd0o1)[1])))
zcd0o <- (cd0oc - 0.875)/sqrt(cd0oc*(1-cd0oc)/nrow(cd0o))
2*pnorm(-abs(zcd0o))

cd0o2 <- glm(resp ~ pre*attdiff, data = cdo, family = binomial(link="logit"))
summary(cd0o2)
cdoc2 <- (exp(coef(cd0o2)[1] + coef(cd0o2)[2]/2)/(1+exp(coef(cd0o2)[1] + coef(cd0o2)[2]/2)))
zcd0o2 <- (cdoc2 - 0.875)/sqrt(cdoc2*(1-cdoc2)/nrow(cdo))
2*pnorm(-abs(zcd0o2))

# movie similarity
mv0s <- mv0[which(mv0$effect == 0),]
mv0s1 <- glm(resp ~ pre, data = mv0s, family = binomial(link="logit"))
mv0s2 <- glm(resp ~ pre + att1 + att2, data = mv0s, family = binomial(link="logit"))
summary(mv0s1)
logit2prob(coef(mv0s1)[1]) # dissimilar = 0 and similar = 1
linearHypothesis(mv0s1, "(Intercept) = 0.70818505792")
mv0sc <- (exp(coef(mv0s1)[1])/(1+exp(coef(mv0s1)[1])))
zmv0s <- (mv0sc - 0.67)/sqrt(mv0sc*(1-mv0sc)/nrow(mv0s))
2*pnorm(-abs(zmv0s))

mv0s2 <- glm(resp ~ pre*attdiff, data = mvs, family = binomial(link="logit"))
summary(mv0s2)
mvsc2 <- (exp(coef(mv0s2)[1] + coef(mv0s2)[2]/2)/(1+exp(coef(mv0s2)[1] + coef(mv0s2)[2]/2)))
zmv0s2 <- (mvsc2 - 0.67)/sqrt(mvsc2*(1-mvsc2)/nrow(mvs))
2*pnorm(-abs(zmv0s2))

# movie outlier
mv0o <- mv0[which(mv0$effect == 1),]
nrow(mv0o)
mv0o1 <- glm(resp ~ pre, data = mv0o, family = binomial(link="logit"))
summary(mv0o1)
logit2prob(coef(mv0o1)[1]) # dissimilar = 0 and similar = 1
linearHypothesis(mv0o1, "(Intercept) + 0.5*pre = 1.94591014906")
mv0oc <- (exp(coef(mv0o1)[1])/(1+exp(coef(mv0o1)[1])))
zmv0o <- (mv0oc - 0.875)/sqrt(mv0oc*(1-mv0oc)/nrow(mv0o))
2*pnorm(-abs(zmv0o))


mv0o2 <- glm(resp ~ pre*attdiff, data = mvo, family = binomial(link="logit"))
summary(mv0o2)
mvoc2 <- (exp(coef(mv0o2)[1] + coef(mv0o2)[2]/2)/(1+exp(coef(mv0o2)[1] + coef(mv0o2)[2]/2)))
zmv0o2 <- (mvoc2 - 0.875)/sqrt(mvoc2*(1-mvoc2)/nrow(mvo))
2*pnorm(-abs(zmv0o2))

##### all scenarios #####
# create a new data frame
# dummy code for scenarios
ef <- data.frame(resp = c(oe$lot50s, oe$lot60s, oe$lot50o, oe$lot60o,
                          oe$res10s, oe$res40s, oe$res10o, oe$res40o,
                          oe$can33s, oe$can66s, oe$can33o, oe$can66o,
                          oe$mov33s, oe$mov66s, oe$mov33o, oe$mov66o),
                 name = (c(rep(c("lot50s"), length(oe$lot50s)),
                         rep(c("lot60s"), length(oe$lot60s)),
                         rep(c("lot50o"), length(oe$lot50o)),
                         rep(c("lot60o"), length(oe$lot60o)),
                         rep(c("res10s"), length(oe$res10s)),
                         rep(c("res40s"), length(oe$res40s)),
                         rep(c("res10o"), length(oe$res10o)),
                         rep(c("res40o"), length(oe$res40o)),
                         rep(c("can33s"), length(oe$can33s)),
                         rep(c("can66s"), length(oe$can66s)),
                         rep(c("can33o"), length(oe$can33o)),
                         rep(c("can66o"), length(oe$can66o)),
                         rep(c("mov33s"), length(oe$mov33s)),
                         rep(c("mov66s"), length(oe$mov66s)),
                         rep(c("mov33o"), length(oe$mov33o)),
                         rep(c("mov66o"), length(oe$mov66o)))),
                 att1 = c(oe$plot50s_1, oe$plot60s_1, oe$plot50o_1, oe$plot60o_1,
                          oe$pres10s_1, oe$pres40s_1, oe$pres10o_1, oe$pres40o_1,
                          oe$pcan33s_1, oe$pcan66s_1, oe$pcan33o_1, oe$pcan66o_1,
                          oe$pmov33s_1, oe$pmov66s_1, oe$pmov33o_1, oe$pmov66o_1),
                 att2 = c(oe$plot50s_2, oe$plot60s_2, oe$plot50o_2, oe$plot60o_2,
                          oe$pres10s_2, oe$pres40s_2, oe$pres10o_2, oe$pres40o_2,
                          oe$pcan33s_2, oe$pcan66s_2, oe$pcan33o_2, oe$pcan66o_2,
                          oe$pmov33s_2, oe$pmov66s_2, oe$pmov33o_2, oe$pmov66o_2))

ef$lot.mov <- factor(c(rep(1, nrow(oe)), rep(0, nrow(oe)), rep(0, nrow(oe)), rep(0, nrow(oe))))
ef$res.mov <- factor(c(rep(0, nrow(oe)), rep(1, nrow(oe)), rep(0, nrow(oe)), rep(0, nrow(oe))))
ef$can.mov <- factor(c(rep(0, nrow(oe)), rep(0, nrow(oe)), rep(1, nrow(oe)), rep(0, nrow(oe))))

# dummy code for effects
# 0 = similarity 
# 1 = outlier
ef$effect <- NA
for (i in 1:nrow(ef)){
  if (ef$name[i] == "lot50s" | ef$name[i] == "lot60s" | ef$name[i] == "res10s" | ef$name[i] == "res40s" |
      ef$name[i] == "can33s" | ef$name[i] == "can66s" | ef$name[i] == "mov33s" | ef$name[i] == "mov66s")
    {ef$effect[i] <- 0} else {ef$effect[i] <- 1}
}


# dummy code for presentation: base on attribute weighting -- whether the option that has the highest value on that prominent attribute is a dissimilar option or a similar option
# For lot: probability of winning > amount of winning
# For res: food quality > driving time
# For can: EQ > IQ
# For mov: critic I > critic II 
# code = 0 if the "more attractive" option is a dissimilar option
# code = 1 if it is a similar option

ef$pre <- NA
for (i in 1:nrow(ef)){
  if (ef$name[i] == "lot60s" | ef$name[i] == "lot60o" | ef$name[i] == "res40s" | ef$name[i] == "res40o" |
      ef$name[i] == "can33s" | ef$name[i] == "can33o" | ef$name[i] == "mov66s" | ef$name[i] == "mov66o")
  {ef$pre[i] <- -0.5} else {ef$pre[i] <- 0.5}
}

ef$attdiff <- ef$att1 - ef$att2


# listwise deletion for NA responses
dt <- na.omit(ef)

# recode to 0 (dissimilar options) and 1 (similar option)
for (i in 1:nrow(dt)){
    if(dt$resp[i] > 1) {dt$resp[i] <- 0}
}
dt$resp <- 1-dt$resp

# adding scenario names
dt$scenario <- NA
for (i in 1:nrow(dt)){
  if (dt$name[i] == "lot50s" | dt$name[i] == "lot60s" | dt$name[i] == "lot50o" | dt$name[i] == "lot60o")
  {dt$scenario[i] <- "gamble"}
  if (dt$name[i] == "mov33s" | dt$name[i] == "mov33o" | dt$name[i] == "mov66s" | dt$name[i] == "mov66o")
  {dt$scenario[i] <- "movie"}
  if (dt$name[i] == "can33s" | dt$name[i] == "can33o" | dt$name[i] == "can66s" | dt$name[i] == "can66o")
  {dt$scenario[i] <- "candidate"}
  if (dt$name[i] == "res10s" | dt$name[i] == "res10o" | dt$name[i] == "res40s" | dt$name[i] == "res40o")
  {dt$scenario[i] <- "restaurant"}
}

# logistic regression
# mod1 <- glm(resp ~ lot.mov + res.mov + can.mov + effect + lot.mov*effect + res.mov*effect + can.mov*effect, family = binomial(link="logit"), data = dt)
# summary(mod1) # doesn't work

# similarity condition for all scenarios
si <- dt[which(dt$effect == 0),]
si1 <- glm(resp ~ pre*attdiff + lot.mov + res.mov + can.mov, data = dt)
summary(si1)
logit2prob(coef(si1)[1]) # dissimilar = 0 and similar = 1
linearHypothesis(si1, "(Intercept) = 0.70818505792")



# outlier condition for all scenarios
ou <- dt[which(dt$effect == 1),]
ou1 <- glm(resp ~ pre*attdiff + lot.mov + res.mov + can.mov, data = dt)
summary(ou1)
logit2prob(coef(ou1)[1]) # dissimilar = 0 and similar = 1
linearHypothesis(ou1, "(Intercept) = 1.94591014906")

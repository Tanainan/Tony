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

# remove participant 215 because didn't rate the attributes for all scenarios
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
gb <- data.frame(resp = c(oe$lot50s, oe$lot60s, oe$lot50o, oe$lot60o),
                 name = (c(rep(c("lot50s"), length(oe$lot50s)),
                           rep(c("lot60s"), length(oe$lot60s)),
                           rep(c("lot50o"), length(oe$lot50o)),
                           rep(c("lot60o"), length(oe$lot60o)))),
                 att1 = c(oe$plot50s_1, oe$plot60s_1, oe$plot50o_1, oe$plot60o_1),
                 att2 = c(oe$plot50s_2, oe$plot60s_2, oe$plot50o_2, oe$plot60o_2))

gb <- na.omit(gb)
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



# recode to 0 (dissimilar options) and 1 (similar option)
for (i in 1:nrow(gb)){
  if(gb$resp[i] > 1) {gb$resp[i] <- 0}
}
gb$resp <- 1-gb$resp

nrow(gb[which(gb$effect == 0),])
nrow(gb[which(gb$effect == 1),])

gambling <- glm(resp ~ effect + pre, family = binomial(link="logit"), data = gb)
summary(gambling)


# convert logit to prob
logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}
logit2prob(coef(gambling))


# test for correlation between presentation and att1-att2
gb$attdiff <- gb$att1 - gb$att2
cor.test(gb$attdiff, gb$pre)
mean(gb$attdiff)

################# restaurant ##################
rt <- data.frame(resp = c(oe$res10s, oe$res40s, oe$res10o, oe$res40o),
                 name = (c(rep(c("res10s"), length(oe$res10s)),
                           rep(c("res40s"), length(oe$res40s)),
                           rep(c("res10o"), length(oe$res10o)),
                           rep(c("res40o"), length(oe$res40o)))),
                 att1 = c(oe$pres10s_1, oe$pres40s_1, oe$pres10o_1, oe$pres40o_1),
                 att2 = c(oe$pres10s_2, oe$pres40s_2, oe$pres10o_2, oe$pres40o_2))

rt <- na.omit(rt)

rt$pre <- NA # presentation
for (i in 1:nrow(rt)){
  if (rt$name[i] == "res40s" | rt$name[i] == "res40o")
  {rt$pre[i] <- 0} else {rt$pre[i] <- 1}
}

rt$effect <- NA
for (i in 1:nrow(rt)){
  if (rt$name[i] == "res40s" | rt$name[i] == "res10s")
  {rt$effect[i] <- 0} else {rt$effect[i] <- 1}
}

# recode to 0 (dissimilar options) and 1 (similar option)
for (i in 1:nrow(rt)){
  if(rt$resp[i] > 1) {rt$resp[i] <- 0}
}
rt$resp <- 1-rt$resp

nrow(rt[which(rt$effect == 0),])
nrow(rt[which(rt$effect == 1),])

restaurant <- glm(resp ~ effect + pre, family = binomial(link="logit"), data = rt)
summary(restaurant)
logit2prob(coef(restaurant))


rt$attdiff <- rt$att1 - rt$att2
cor.test(rt$attdiff, rt$pre)
mean(rt$attdiff)

############# candidate #####################
cd <- data.frame(resp = c(oe$can33s, oe$can66s, oe$can33o, oe$can66o),
                 name = (c(rep(c("can33s"), length(oe$can33s)),
                           rep(c("can66s"), length(oe$can66s)),
                           rep(c("can33o"), length(oe$can33o)),
                           rep(c("can66o"), length(oe$can66o)))),
                 att1 = c(oe$pcan33s_1, oe$pcan66s_1, oe$pcan33o_1, oe$pcan66o_1),
                 att2 = c(oe$pcan33s_2, oe$pcan66s_2, oe$pcan33o_2, oe$pcan66o_2))

cd <- na.omit(cd)

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


# recode to 0 (dissimilar options) and 1 (similar option)
for (i in 1:nrow(cd)){
  if(cd$resp[i] > 1) {cd$resp[i] <- 0}
}
cd$resp <- 1-cd$resp

nrow(cd[which(cd$effect == 0),])
nrow(cd[which(cd$effect == 1),])

candidate <- glm(resp ~ effect + pre, family = binomial(link="logit"), data = cd)
summary(candidate)
logit2prob(coef(candidate))

cd$attdiff <- cd$att1 - cd$att2
cor.test(cd$attdiff, cd$pre)
mean(cd$attdiff)

############# movie #####################
mv <- data.frame(resp = c(oe$mov33s, oe$mov66s, oe$mov33o, oe$mov66o),
                 name = (c(rep(c("mov33s"), length(oe$mov33s)),
                           rep(c("mov66s"), length(oe$mov66s)),
                           rep(c("mov33o"), length(oe$mov33o)),
                           rep(c("mov66o"), length(oe$mov66o)))),
                 att1 = c(oe$pmov33s_1, oe$pmov66s_1, oe$pmov33o_1, oe$pmov66o_1),
                 att2 = c(oe$pmov33s_2, oe$pmov66s_2, oe$pmov33o_2, oe$pmov66o_2))
mv <- na.omit(mv)

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

nrow(mv[which(mv$effect == 0),])
nrow(mv[which(mv$effect == 1),])

movie <- glm(resp ~ effect + pre + effect*pre + att1 + att2, family = binomial(link="logit"), data = mv)
movie1 <- glm(resp ~ effect + pre, family = binomial(link="logit"), data = mv)
summary(movie)
summary(movie1)
logit2prob(coef(movie))

mv$attdiff <- mv$att1 - mv$att2
cor.test(mv$attdiff, mv$pre)
mean(mv$attdiff)

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

nrow(cd1[which(cd1$effect == 0),])
nrow(cd1[which(cd1$effect == 1),])

candidate1 <- glm(resp ~ effect + pre, family = binomial(link="logit"), data = cd1)
summary(candidate1)
logit2prob(coef(candidate1))

cd1$attdiff <- cd1$att1 - cd1$att2
cor.test(cd1$attdiff, cd1$pre)
mean(cd1$attdiff)


####### find the raw probability of each scenario ########
# histogram(gb[which(gb$effect == 0), c("resp")])
# histogram(gb[which(gb$effect == 1), c("resp")])
# 
# histogram(rt[which(rt$effect == 0), c("resp")])
# histogram(rt[which(rt$effect == 1), c("resp")])
# 
# histogram(cd[which(cd$effect == 0), c("resp")])
# histogram(cd[which(cd$effect == 1), c("resp")])
# 
# histogram(mv[which(mv$effect == 0), c("resp")])
# histogram(mv[which(mv$effect == 1), c("resp")])

#### 0 as % dissimilar option being chosen ####
# gambling
prop.table(table(gb[which(gb$effect == 0), c("resp")]))[1] # similarity
prop.table(table(gb[which(gb$effect == 1), c("resp")]))[1] # outlier

# restaurant
prop.table(table(rt[which(rt$effect == 0), c("resp")]))[1] # similarity
prop.table(table(rt[which(rt$effect == 1), c("resp")]))[1] # outlier

# candidate
prop.table(table(cd[which(cd$effect == 0), c("resp")]))[1] # similarity
prop.table(table(cd[which(cd$effect == 1), c("resp")]))[1] # outlier

# movie
prop.table(table(mv[which(mv$effect == 0), c("resp")]))[1] # similarity
prop.table(table(mv[which(mv$effect == 1), c("resp")]))[1] # outlier

##### test if raw proportion of dissimilar option is different from 0.33 and 0.125
# gambling
binom.test(nrow(gb[which(gb$effect == 0 & gb$resp == 0),]), nrow(gb[which(gb$effect == 0),]), p = 0.33, alternative = "greater")
binom.test(nrow(gb[which(gb$effect == 1 & gb$resp == 0),]), nrow(gb[which(gb$effect == 1),]), p = 0.125, alternative = "less")

# restaurant
binom.test(nrow(rt[which(rt$effect == 0 & rt$resp == 0),]), nrow(rt[which(rt$effect == 0),]), p = 0.33, alternative = "greater")
binom.test(nrow(rt[which(rt$effect == 1 & rt$resp == 0),]), nrow(rt[which(rt$effect == 1),]), p = 0.125, alternative = "less")

# candidate
binom.test(nrow(cd[which(cd$effect == 0 & cd$resp == 0),]), nrow(cd[which(cd$effect == 0),]), p = 0.33, alternative = "greater")
binom.test(nrow(cd[which(cd$effect == 1 & cd$resp == 0),]), nrow(cd[which(cd$effect == 1),]), p = 0.125, alternative = "less")

# movie
binom.test(nrow(mv[which(mv$effect == 0 & mv$resp == 0),]), nrow(mv[which(mv$effect == 0),]), p = 0.33, alternative = "greater")
binom.test(nrow(mv[which(mv$effect == 1 & mv$resp == 0),]), nrow(mv[which(mv$effect == 1),]), p = 0.125, alternative = "less")


####### only test for either similarity effect or outlier effect, controlling for presentation ######
# gambling similarity
gbs <- gb[which(gb$effect == 0),]
gbs1 <- glm(resp ~ pre, data = gbs, family = binomial(link="logit"))
summary(gbs1)
logit2prob(coef(gbs1)[1]+coef(gbs1)[2]/2) # dissimilar = 0 and similar = 1
#linearHypothesis(mvs1, "(Intercept) = 0.70818505792")
linearHypothesis(gbs1, "(Intercept) + 0.5*pre = 0.70818505792") # the coefficients for intercept and when presentation is 0 and 1 (average), comparing with a logit of 0.67
gbsc <- (exp(coef(gbs1)[1] + coef(gbs1)[2]/2)/(1+exp(coef(gbs1)[1] + coef(gbs1)[2]/2)))
zgbs <- (gbsc - 0.67)/sqrt(gbsc*(1-gbsc)/nrow(gbs)) # the formula is from http://www.sthda.com/english/wiki/one-proportion-z-test-in-r
2*pnorm(-abs(zgbs))

gbs2 <- glm(resp ~ attdiff*pre, data = gbs, family = binomial(link="logit"))
summary(gbs2)
gbsc2 <- (exp(coef(gbs2)[1] + coef(gbs2)[2]/2)/(1+exp(coef(gbs2)[1] + coef(gbs2)[2]/2)))
zgbs2 <- (gbsc2 - 0.67)/sqrt(gbsc2*(1-gbsc2)/nrow(gbs)) # the formula is from http://www.sthda.com/english/wiki/one-proportion-z-test-in-r
2*pnorm(-abs(zgbs2))


# gambling outlier
gbo <- gb[which(gb$effect == 1),]
gbo1 <- glm(resp ~ pre, data = gbo, family = binomial(link="logit"))
summary(gbo1)
logit2prob(coef(gbo1)[1]+coef(gbo1)[2]/2) # dissimilar = 0 and similar = 1
#linearHypothesis(gbo1, "(Intercept) = 1.94591014906")
linearHypothesis(gbo1, "(Intercept) + 0.5*pre = 1.94591014906") # comparing with a logit of 0.875
gboc <- (exp(coef(gbo1)[1] + coef(gbo1)[2]/2)/(1+exp(coef(gbo1)[1] + coef(gbo1)[2]/2)))
zgbo <- (gboc - 0.875)/sqrt(gboc*(1-gboc)/nrow(gbo))
2*pnorm(-abs(zgbo))

gbo2 <- glm(resp ~ pre*attdiff, data = gbo, family = binomial(link="logit"))
summary(gbo2)
gboc2 <- (exp(coef(gbo2)[1] + coef(gbo2)[2]/2)/(1+exp(coef(gbo2)[1] + coef(gbo2)[2]/2)))
zgbo2 <- (gboc2 - 0.875)/sqrt(gboc2*(1-gboc2)/nrow(gbo))
2*pnorm(-abs(zgbo2))


# restaurant similarity
rts <- rt[which(rt$effect == 0),]
rts1 <- glm(resp ~ pre, data = rts, family = binomial(link="logit"))
summary(rts1)
logit2prob(coef(rts1)[1]+coef(rts1)[2]/2) # dissimilar = 0 and similar = 1
#linearHypothesis(rts1, "(Intercept) = 0.70818505792")
linearHypothesis(rts1, "(Intercept) + 0.5*pre = 0.70818505792")
rtsc <- (exp(coef(rts1)[1] + coef(rts1)[2]/2)/(1+exp(coef(rts1)[1] + coef(rts1)[2]/2)))
zrts <- (rtsc - 0.67)/sqrt(rtsc*(1-rtsc)/nrow(rts))
2*pnorm(-abs(zrts))

rts2 <- glm(resp ~ pre*attdiff, data = rts, family = binomial(link="logit"))
summary(rts2)
rtsc2 <- (exp(coef(rts2)[1] + coef(rts2)[2]/2)/(1+exp(coef(rts2)[1] + coef(rts2)[2]/2)))
zrts2 <- (rtsc2 - 0.67)/sqrt(rtsc2*(1-rtsc2)/nrow(rts))
2*pnorm(-abs(zrts2))


# restaurant outlier
rto <- rt[which(rt$effect == 1),]
rto1 <- glm(resp ~ pre, data = rto, family = binomial(link="logit"))
summary(rto1)
logit2prob(coef(rto1)[1]+coef(rto1)[2]/2) # dissimilar = 0 and similar = 1
#linearHypothesis(rto1, "(Intercept) = 1.94591014906")
linearHypothesis(rto1, "(Intercept) + 0.5*pre = 1.94591014906")
rtoc <- (exp(coef(rto1)[1] + coef(rto1)[2]/2)/(1+exp(coef(rto1)[1] + coef(rto1)[2]/2)))
zrto <- (rtoc - 0.875)/sqrt(rtoc*(1-rtoc)/nrow(rto))
2*pnorm(-abs(zrto))

rto2 <- glm(resp ~ pre*attdiff, data = rto, family = binomial(link="logit"))
summary(rto2)
rtoc2 <- (exp(coef(rto2)[1] + coef(rto2)[2]/2)/(1+exp(coef(rto2)[1] + coef(rto2)[2]/2)))
zrto2 <- (rtoc2 - 0.875)/sqrt(rtoc2*(1-rtoc2)/nrow(rto))
2*pnorm(-abs(zrto2))


# candidate similarity
cds <- cd[which(cd$effect == 0),]
cds1 <- glm(resp ~ pre, data = cds, family = binomial(link="logit"))
summary(cds1)
logit2prob(coef(cds1)[1]+coef(cds1)[2]/2) # dissimilar = 0 and similar = 1
#linearHypothesis(cds1, "(Intercept) = 0.70818505792")
linearHypothesis(cds1, "(Intercept) + 0.5*pre = 0.70818505792")
cdsc <- (exp(coef(cds1)[1] + coef(cds1)[2]/2)/(1+exp(coef(cds1)[1] + coef(cds1)[2]/2)))
zcds <- (cdsc - 0.67)/sqrt(cdsc*(1-cdsc)/nrow(cds))
2*pnorm(-abs(zcds))


cds2 <- glm(resp ~ pre*attdiff, data = cds, family = binomial(link="logit"))
summary(cds2)
cdsc2 <- (exp(coef(cds2)[1] + coef(cds2)[2]/2)/(1+exp(coef(cds2)[1] + coef(cds2)[2]/2)))
zcds2 <- (cdsc2 - 0.67)/sqrt(cdsc2*(1-cdsc2)/nrow(cds))
2*pnorm(-abs(zcds2))

# candidate outlier
cdo <- cd[which(cd$effect == 1),]
cdo1 <- glm(resp ~ pre, data = cdo, family = binomial(link="logit"))
summary(cdo1)
logit2prob(coef(cdo1)[1]+coef(cdo1)[2]/2) # dissimilar = 0 and similar = 1
#linearHypothesis(cdo1, "(Intercept) = 1.94591014906")
linearHypothesis(cdo1, "(Intercept) + 0.5*pre = 1.94591014906")
cdoc <- (exp(coef(cdo1)[1] + coef(cdo1)[2]/2)/(1+exp(coef(cdo1)[1] + coef(cdo1)[2]/2)))
zcdo <- (cdoc - 0.875)/sqrt(cdoc*(1-cdoc)/nrow(cdo))
2*pnorm(-abs(zcdo))


cdo2 <- glm(resp ~ pre*attdiff, data = cdo, family = binomial(link="logit"))
summary(cdo2)
cdoc2 <- (exp(coef(cdo2)[1] + coef(cdo2)[2]/2)/(1+exp(coef(cdo2)[1] + coef(cdo2)[2]/2)))
zcdo2 <- (cdoc2 - 0.875)/sqrt(cdoc2*(1-cdoc2)/nrow(cdo))
2*pnorm(-abs(zcdo2))

# movie similarity
mvs <- mv[which(mv$effect == 0),]
mvs1 <- glm(resp ~ pre, data = mvs, family = binomial(link="logit"))
summary(mvs1)
logit2prob(coef(mvs1)[1]+coef(mvs1)[2]/2) # dissimilar = 0 and similar = 1
#linearHypothesis(mvs1, "(Intercept) = 0.70818505792")
linearHypothesis(mvs1, "(Intercept) + 0.5*pre = 0.70818505792")
mvsc <- (exp(coef(mvs1)[1] + coef(mvs1)[2]/2)/(1+exp(coef(mvs1)[1] + coef(mvs1)[2]/2)))
zmvs <- (mvsc - 0.67)/sqrt(mvsc*(1-mvsc)/nrow(mvs))
2*pnorm(-abs(zmvs))

mvs2 <- glm(resp ~ pre*attdiff, data = mvs, family = binomial(link="logit"))
summary(mvs2)
mvsc2 <- (exp(coef(mvs2)[1] + coef(mvs2)[2]/2)/(1+exp(coef(mvs2)[1] + coef(mvs2)[2]/2)))
zmvs2 <- (mvsc2 - 0.67)/sqrt(mvsc2*(1-mvsc2)/nrow(mvs))
2*pnorm(-abs(zmvs2))

# movie outlier
mvo <- mv[which(mv$effect == 1),]
nrow(mvo)
mvo1 <- glm(resp ~ pre, data = mvo, family = binomial(link="logit"))
summary(mvo1)
logit2prob(coef(mvo1)[1]+coef(mvo1)[2]/2) # dissimilar = 0 and similar = 1
#linearHypothesis(mvo1, "(Intercept) = 1.94591014906")
linearHypothesis(mvo1, "(Intercept) + 0.5*pre = 1.94591014906")
mvoc <- (exp(coef(mvo1)[1] + coef(mvo1)[2]/2)/(1+exp(coef(mvo1)[1] + coef(mvo1)[2]/2)))
zmvo <- (mvoc - 0.875)/sqrt(mvoc*(1-mvoc)/nrow(mvo))
2*pnorm(-abs(zmvo))

mvo2 <- glm(resp ~ pre*attdiff, data = mvo, family = binomial(link="logit"))
summary(mvo2)
mvoc2 <- (exp(coef(mvo2)[1] + coef(mvo2)[2]/2)/(1+exp(coef(mvo2)[1] + coef(mvo2)[2]/2)))
zmvo2 <- (mvoc2 - 0.875)/sqrt(mvoc2*(1-mvoc2)/nrow(mvo))
2*pnorm(-abs(zmvo2))



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
  {ef$pre[i] <- 0} else {ef$pre[i] <- 1}
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
#linearHypothesis(si1, "(Intercept) = 0.70818505792")
linearHypothesis(si1, "(Intercept) + 0.5*pre = 0.70818505792")



# outlier condition for all scenarios
ou <- dt[which(dt$effect == 1),]
ou1 <- glm(resp ~ pre*attdiff + lot.mov + res.mov + can.mov, data = dt)
summary(ou1)
logit2prob(coef(ou1)[1]) # dissimilar = 0 and similar = 1
#linearHypothesis(ou1, "(Intercept) = 1.94591014906")
linearHypothesis(ou1, "(Intercept) + 0.5*pre = 1.94591014906")

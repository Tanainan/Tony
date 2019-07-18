setwd("~/Desktop/Tony/Tony")
library(readr)
library(memisc)
library(tidyverse)
library(lme4)
library(stats)
library(car)
library(nnet) # multinom
oe <- read_csv("OE test.csv")
oe <- oe[-c(1,2),-c(1:18)] # remove headers and other columns
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

logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

logit2odds <- function(logit){
  odds <- exp(logit)
  return(odds)
}





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
# candidate
# 1 = IQ
# 2 = EQ
# mean(oe$pcan33o_1, na.rm = T)
# mean(oe$pcan33o_2, na.rm = T)
# mean(oe$pcan33s_1, na.rm = T)
# mean(oe$pcan33s_2, na.rm = T)
# mean(oe$pcan66o_1, na.rm = T)
# mean(oe$pcan66o_2, na.rm = T)
# mean(oe$pcan66s_1, na.rm = T)
# mean(oe$pcan66s_2, na.rm = T)
# 
# sd(oe$pcan33o_1, na.rm = T)
# sd(oe$pcan33o_2, na.rm = T)
# sd(oe$pcan33s_1, na.rm = T)
# sd(oe$pcan33s_2, na.rm = T)
# sd(oe$pcan66o_1, na.rm = T)
# sd(oe$pcan66o_2, na.rm = T)
# sd(oe$pcan66s_1, na.rm = T)
# sd(oe$pcan66s_2, na.rm = T)

mean(cd$att1)
sd(cd$att1)
mean(cd$att2)
sd(cd$att2)

mean(cd1$att1)
sd(cd1$att1)
mean(cd1$att2)
sd(cd1$att2)


# (need to exclude 17)


# gambling
# 1 = prob of winning
# 2 = amount of winning
# mean(oe$plot50s_1, na.rm = T)
# mean(oe$plot50s_2, na.rm = T)
# mean(oe$plot50o_1, na.rm = T)
# mean(oe$plot50o_2, na.rm = T)
# mean(oe$plot60s_1, na.rm = T)
# mean(oe$plot60s_2, na.rm = T)
# mean(oe$plot60o_1, na.rm = T)
# mean(oe$plot60o_2, na.rm = T)
# 
# sd(oe$plot50s_1, na.rm = T)
# sd(oe$plot50s_2, na.rm = T)
# sd(oe$plot50o_1, na.rm = T)
# sd(oe$plot50o_2, na.rm = T)
# sd(oe$plot60s_1, na.rm = T)
# sd(oe$plot60s_2, na.rm = T)
# sd(oe$plot60o_1, na.rm = T)
# sd(oe$plot60o_2, na.rm = T)

mean(gb$att1)
sd(gb$att1)
mean(gb$att2)
sd(gb$att2)

# restaurant
# 1 = driving time
# 2 = food quality
# mean(oe$pres10o_1, na.rm = T)
# mean(oe$pres10o_2, na.rm = T)
# mean(oe$pres40o_1, na.rm = T)
# mean(oe$pres40o_2, na.rm = T)
# mean(oe$pres10s_1, na.rm = T)
# mean(oe$pres10s_2, na.rm = T)
# mean(oe$pres40s_1, na.rm = T)
# mean(oe$pres40s_2, na.rm = T)
# 
# sd(oe$pres10o_1, na.rm = T)
# sd(oe$pres10o_2, na.rm = T)
# sd(oe$pres40o_1, na.rm = T)
# sd(oe$pres40o_2, na.rm = T)
# sd(oe$pres10s_1, na.rm = T)
# sd(oe$pres10s_2, na.rm = T)
# sd(oe$pres40s_1, na.rm = T)
# sd(oe$pres40s_2, na.rm = T)

mean(rt$att1)
sd(rt$att1)
mean(rt$att2)
sd(rt$att2)

# movie
# 1 = critic I
# 2 = critic II

# mean(oe$pmov66o_1, na.rm = T)
# mean(oe$pmov66o_2, na.rm = T)
# mean(oe$pmov33o_1, na.rm = T)
# mean(oe$pmov33o_2, na.rm = T)
# mean(oe$pmov66s_1, na.rm = T)
# mean(oe$pmov66s_2, na.rm = T)
# mean(oe$pmov33s_1, na.rm = T)
# mean(oe$pmov33s_2, na.rm = T)

# sd(oe$pmov66o_1, na.rm = T)
# sd(oe$pmov66o_2, na.rm = T)
# sd(oe$pmov33o_1, na.rm = T)
# sd(oe$pmov33o_2, na.rm = T)
# sd(oe$pmov66s_1, na.rm = T)
# sd(oe$pmov66s_2, na.rm = T)
# sd(oe$pmov33s_1, na.rm = T)
# sd(oe$pmov33s_2, na.rm = T)

mean(mv$att1)
sd(mv$att1)
mean(mv$att2)
sd(mv$att2)

# demographic
prop.table(table(oe$gender))

oe[which(oe$age == 1996),c("age")] <- "25"
oe[which(oe$age == 1995),c("age")] <- "24"
oe[which(oe$age == 1989),c("age")] <- "30"
oe[which(oe$age == 1999),c("age")] <- "20"

mean(as.numeric(oe$age), na.rm = T)
sd(as.numeric(oe$age), na.rm = T)
which(is.na(oe$age))
which(is.na(oe$gender))

nrow(dt[which(dt$scenario == "gamble" & dt$effect == 0),])
nrow(dt[which(dt$scenario == "gamble" & dt$effect == 1),])
nrow(dt[which(dt$scenario == "gamble" & dt$effect == 0 & dt$pre == -0.5),])
nrow(dt[which(dt$scenario == "gamble" & dt$effect == 0 & dt$pre == 0.5),])
nrow(dt[which(dt$scenario == "gamble" & dt$effect == 1 & dt$pre == -0.5),])
nrow(dt[which(dt$scenario == "gamble" & dt$effect == 1 & dt$pre == 0.5),])

nrow(dt[which(dt$scenario == "restaurant" & dt$effect == 0),])
nrow(dt[which(dt$scenario == "restaurant" & dt$effect == 1),])
nrow(dt[which(dt$scenario == "restaurant" & dt$effect == 0 & dt$pre == -0.5),])
nrow(dt[which(dt$scenario == "restaurant" & dt$effect == 0 & dt$pre == 0.5),])
nrow(dt[which(dt$scenario == "restaurant" & dt$effect == 1 & dt$pre == -0.5),])
nrow(dt[which(dt$scenario == "restaurant" & dt$effect == 1 & dt$pre == 0.5),])


nrow(dt[which(dt$scenario == "candidate" & dt$effect == 0),])
nrow(dt[which(dt$scenario == "candidate" & dt$effect == 1),])
nrow(dt[which(dt$scenario == "candidate" & dt$effect == 0 & dt$pre == -0.5),])
nrow(dt[which(dt$scenario == "candidate" & dt$effect == 0 & dt$pre == 0.5),])
nrow(dt[which(dt$scenario == "candidate" & dt$effect == 1 & dt$pre == -0.5),])
nrow(dt[which(dt$scenario == "candidate" & dt$effect == 1 & dt$pre == 0.5),])

nrow(dt[which(dt$scenario == "movie" & dt$effect == 0),])
nrow(dt[which(dt$scenario == "movie" & dt$effect == 1),])
nrow(dt[which(dt$scenario == "movie" & dt$effect == 0 & dt$pre == -0.5),])
nrow(dt[which(dt$scenario == "movie" & dt$effect == 0 & dt$pre == 0.5),])
nrow(dt[which(dt$scenario == "movie" & dt$effect == 1 & dt$pre == -0.5),])
nrow(dt[which(dt$scenario == "movie" & dt$effect == 1 & dt$pre == 0.5),])

############## test each scenario separetely but with both similarity and outlier conditions ###############
# create a data frame for each scenario
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
# code = -0.5 if the "more attractive" option is a dissimilar option
# code = 0.5 if it is a similar option
gb$pre <- NA
for (i in 1:nrow(gb)){
  if (gb$name[i] == "lot60s" | gb$name[i] == "lot60o")
  {gb$pre[i] <- -0.5} else {gb$pre[i] <- 0.5}
}

# dummy code for effects
# 0 = similarity 
# 1 = outlier
gb$effect <- NA
for (i in 1:nrow(gb)){
  if (gb$name[i] == "lot50s" | gb$name[i] == "lot60s")
  {gb$effect[i] <- 0} else {gb$effect[i] <- 1}
}



# recode to 1 (dissimilar options) and 0 (similar option)
for (i in 1:nrow(gb)){
  if(gb$resp[i] > 1) {gb$resp[i] <- 0}
}

# test for correlation between presentation and att1-att2 (prevent IVs correlating each other)
gb$attdiff <- gb$att1 - gb$att2
#cor.test(gb$attdiff, gb$pre)


#mean centering attdiff
gb$attdiffc <- gb$attdiff - mean(gb$attdiff)

sd(gb$attdiff)

nrow(gb[which(gb$effect == 0),])
nrow(gb[which(gb$effect == 1),])

gambling <- glm(resp ~ effect + pre*attdiffc, family = binomial(link="logit"), data = gb)
summary(gambling)


# convert logit to prob

logit2prob(coef(gambling))



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
  {rt$pre[i] <- -0.5} else {rt$pre[i] <- 0.5}
}

rt$effect <- NA
for (i in 1:nrow(rt)){
  if (rt$name[i] == "res40s" | rt$name[i] == "res10s")
  {rt$effect[i] <- 0} else {rt$effect[i] <- 1}
}

# recode to 1 (dissimilar options) and 0 (similar option)
for (i in 1:nrow(rt)){
  if(rt$resp[i] > 1) {rt$resp[i] <- 0}
}


rt$attdiff <- rt$att1 - rt$att2
#cor.test(rt$attdiff, rt$pre)

#mean centering attdiff
rt$attdiffc <- rt$attdiff - mean(rt$attdiff)
sd(rt$attdiff)

nrow(rt[which(rt$effect == 0),])
nrow(rt[which(rt$effect == 1),])

restaurant <- glm(resp ~ effect + pre*attdiffc, family = binomial(link="logit"), data = rt)
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

cd <- na.omit(cd)

cd$pre <- NA # presentation
for (i in 1:nrow(cd)){
  if (cd$name[i] == "can33s" | cd$name[i] == "can33o")
  {cd$pre[i] <- -0.5} else {cd$pre[i] <- 0.5}
}

cd$effect <- NA
for (i in 1:nrow(cd)){
  if (cd$name[i] == "can66s" | cd$name[i] == "can33s")
  {cd$effect[i] <- 0} else {cd$effect[i] <- 1}
}


# recode to 1 (dissimilar options) and 0 (similar option)
for (i in 1:nrow(cd)){
  if(cd$resp[i] > 1) {cd$resp[i] <- 0}
}


cd$attdiff <- cd$att1 - cd$att2
#cor.test(cd$attdiff, cd$pre)
cd$attdiffc <- cd$attdiff - mean(cd$attdiff)

sd(cd$attdiff)


nrow(cd[which(cd$effect == 0),])
nrow(cd[which(cd$effect == 1),])

candidate <- glm(resp ~ effect + pre*attdiffc, family = binomial(link="logit"), data = cd)
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
mv <- na.omit(mv)

mv$pre <- NA # presentation
for (i in 1:nrow(mv)){
  if (mv$name[i] == "mov66s" | mv$name[i] == "mov66o")
  {mv$pre[i] <- -0.5} else {mv$pre[i] <- 0.5}
}

mv$effect <- NA
for (i in 1:nrow(mv)){
  if (mv$name[i] == "mov66s" | mv$name[i] == "mov33s")
  {mv$effect[i] <- 0} else {mv$effect[i] <- 1}
}

mv <- na.omit(mv)


# recode to 1 (dissimilar options) and 0 (similar option)
for (i in 1:nrow(mv)){
  if(mv$resp[i] > 1) {mv$resp[i] <- 0}
}

mv$attdiff <- mv$att1 - mv$att2
#cor.test(mv$attdiff, mv$pre)
mv$attdiffc <- mv$attdiff - mean(mv$attdiff)

sd(mv$attdiff)

nrow(mv[which(mv$effect == 0),])
nrow(mv[which(mv$effect == 1),])

movie <- glm(resp ~ effect + pre*attdiffc, family = binomial(link="logit"), data = mv)
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
  {cd1$pre[i] <- -0.5} else {cd1$pre[i] <- 0.5}
}

cd1$effect <- NA
for (i in 1:nrow(cd1)){
  if (cd1$name[i] == "can66s" | cd1$name[i] == "can33s")
  {cd1$effect[i] <- 0} else {cd1$effect[i] <- 1}
}

cd1 <- na.omit(cd1)

# recode to 1 (dissimilar options) and 0 (similar option)
for (i in 1:nrow(cd1)){
  if(cd1$resp[i] > 1) {cd1$resp[i] <- 0}
}

cd1$attdiff <- cd1$att1 - cd1$att2
#cor.test(cd1$attdiff, cd1$pre)
cd1$attdiffc <- cd1$attdiff - mean(cd1$attdiff)

sd(cd1$attdiff)

nrow(cd1[which(cd1$effect == 0),])
nrow(cd1[which(cd1$effect == 1),])

candidate1 <- glm(resp ~ effect + pre*attdiffc, family = binomial(link="logit"), data = cd1)
summary(candidate1)
logit2prob(coef(candidate1))




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

######## frequency: 0 as % dissimilar option being chosen ####
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
binom.test(nrow(gb[which(gb$effect == 0 & gb$resp == 1),]), nrow(gb[which(gb$effect == 0),]), p = 0.33, alternative = "two.sided")
binom.test(nrow(gb[which(gb$effect == 1 & gb$resp == 1),]), nrow(gb[which(gb$effect == 1),]), p = 0.125, alternative = "two.sided")

# restaurant
binom.test(nrow(rt[which(rt$effect == 0 & rt$resp == 1),]), nrow(rt[which(rt$effect == 0),]), p = 0.33, alternative = "two.sided")
binom.test(nrow(rt[which(rt$effect == 1 & rt$resp == 1),]), nrow(rt[which(rt$effect == 1),]), p = 0.125, alternative = "two.sided")

# candidate
binom.test(nrow(cd[which(cd$effect == 0 & cd$resp == 1),]), nrow(cd[which(cd$effect == 0),]), p = 0.33, alternative = "two.sided")
binom.test(nrow(cd[which(cd$effect == 1 & cd$resp == 1),]), nrow(cd[which(cd$effect == 1),]), p = 0.125, alternative = "two.sided")
binom.test(nrow(cd1[which(cd1$effect == 1 & cd1$resp == 1),]), nrow(cd1[which(cd1$effect == 1),]), p = 0.125, alternative = "two.sided") # candidate - 17 

# movie
binom.test(nrow(mv[which(mv$effect == 0 & mv$resp == 1),]), nrow(mv[which(mv$effect == 0),]), p = 0.33, alternative = "two.sided")
binom.test(nrow(mv[which(mv$effect == 1 & mv$resp == 1),]), nrow(mv[which(mv$effect == 1),]), p = 0.125, alternative = "two.sided")






################## only test for either similarity effect or outlier effect, controlling for presentation #################

############# gambling similarity #################
gbs <- gb[which(gb$effect == 0),]
gbs1 <- glm(resp ~ pre*attdiffc, data = gbs, family = binomial(link="logit"))
summary(gbs1)
# gbsc <- (exp(coef(gbs1)[1])/(1+exp(coef(gbs1)[1])))
# zgbs <- (gbsc - 0.67)/sqrt(gbsc*(1-gbsc)/nrow(gbs)) # the formula is from http://www.sthda.com/english/wiki/one-proport0ion-z-test-in-r
# 2*pnorm(-abs(zgbs))

# logit to prob for intercept, controlling for other IVs 
gbs1a <- logit2prob(coef(gbs1)[1]) # dissimilar = 1 and similar = 0
binom.test(round(gbs1a*nrow(gbs),0), nrow(gbs), p = 0.33, alternative = "two.sided")

# test if logit of intercept is significantly equal to 0.33 or not
#linearHypothesis(gbs1, "(Intercept) = 0.70818505792") # the coefficients for intercept and when presentation is 0 and 1 (average), comparing with a logit of 0.67


############# gambling outlier #################
gbo <- gb[which(gb$effect == 1),]
gbo1 <- glm(resp ~ pre*attdiffc, data = gbo, family = binomial(link="logit"))
summary(gbo1)

# gboc <- (exp(coef(gbo1)[1])/(1+exp(coef(gbo1)[1])))
# zgbo <- (gboc - 0.875)/sqrt(gboc*(1-gboc)/nrow(gbo))
# 2*pnorm(-abs(zgbo))

gbo1a <- logit2prob(coef(gbo1)[1]) # dissimilar = 1 and similar = 0
binom.test(round(gbo1a*nrow(gbo),0), nrow(gbo), p = 0.125, alternative = "two.sided")

#linearHypothesis(gbo1, "(Intercept) = 1.94591014906") # comparing with a logit of 0.875

############# restaurant similarity #################
rts <- rt[which(rt$effect == 0),]
rts1 <- glm(resp ~ pre*attdiffc, data = rts, family = binomial(link="logit"))
summary(rts1)
# rtsc <- (exp(coef(rts1)[1])/(1+exp(coef(rts1)[1])))
# zrts <- (rtsc - 0.67)/sqrt(rtsc*(1-rtsc)/nrow(rts))
# 2*pnorm(-abs(zrts))

rts1a <- logit2prob(coef(rts1)[1]) # dissimilar = 1 and similar = 0
binom.test(round(rts1a*nrow(rts),0), nrow(rts), p = 0.33, alternative = "two.sided")

#linearHypothesis(rts1, "(Intercept) = 0.70818505792")

############# restaurant outlier #################
rto <- rt[which(rt$effect == 1),]
rto1 <- glm(resp ~ pre*attdiffc, data = rto, family = binomial(link="logit"))
summary(rto1)
# rtoc <- (exp(coef(rto1)[1])/(1+exp(coef(rto1)[1])))
# zrto <- (rtoc - 0.875)/sqrt(rtoc*(1-rtoc)/nrow(rto))
# 2*pnorm(-abs(zrto))

rto1a <- logit2prob(coef(rto1)[1]) # dissimilar = 1 and similar = 0
binom.test(round(rto1a*nrow(rto),0), nrow(rto), p = 0.125, alternative = "two.sided")


#linearHypothesis(rto1, "(Intercept) = 1.94591014906")

############# candidate similarity #################
cds <- cd[which(cd$effect == 0),]
cds1 <- glm(resp ~ pre*attdiffc, data = cds, family = binomial(link="logit"))
summary(cds1)
# cdsc <- (exp(coef(cds1)[1])/(1+exp(coef(cds1)[1])))
# zcds <- (cdsc - 0.67)/sqrt(cdsc*(1-cdsc)/nrow(cds))
# 2*pnorm(-abs(zcds))

cds1a <- logit2prob(coef(cds1)[1]) # dissimilar = 1 and similar = 0
binom.test(round(cds1a*nrow(cds),0), nrow(cds), p = 0.33, alternative = "two.sided")

#linearHypothesis(cds1, "(Intercept) = 0.70818505792")

############# candidate outlier #################
cdo <- cd[which(cd$effect == 1),]
cdo1 <- glm(resp ~ pre*attdiffc, data = cdo, family = binomial(link="logit"))
summary(cdo1)
# cdoc <- (exp(coef(cdo1)[1])/(1+exp(coef(cdo1)[1])))
# zcdo <- (cdoc - 0.875)/sqrt(cdoc*(1-cdoc)/nrow(cdo))
# 2*pnorm(-abs(zcdo))

cdo1a <- logit2prob(coef(cdo1)[1]) # dissimilar = 1 and similar = 0
binom.test(round(cdo1a*nrow(cdo),0), nrow(cdo), p = 0.125, alternative = "two.sided")


#linearHypothesis(cdo1, "(Intercept)= 1.94591014906")

############# candidate - 17 outlier #################
cd1o <- cd1[which(cd1$effect == 1),]
cd1o1 <- glm(resp ~ pre*attdiffc, data = cd1o, family = binomial(link="logit"))
summary(cd1o1)
# cd1oc <- (exp(coef(cd1o1)[1])/(1+exp(coef(cd1o1)[1])))
# zcd1o <- (cd1oc - 0.875)/sqrt(cd1oc*(1-cd1oc)/nrow(cd1o))
# 2*pnorm(-abs(zcd1o))

cd1o1a <- logit2prob(coef(cd1o1)[1]) # dissimilar = 1 and similar = 0
binom.test(round(cd1o1a*nrow(cd1o),0), nrow(cd1o), p = 0.125, alternative = "two.sided")


#linearHypothesis(cd1o1, "(Intercept) = 1.94591014906")

############# movie similarity #################
mvs <- mv[which(mv$effect == 0),]
mvs1 <- glm(resp ~ pre*attdiffc, data = mvs, family = binomial(link="logit"))
summary(mvs1)
# mvsc <- (exp(coef(mvs1)[1])/(1+exp(coef(mvs1)[1])))
# zmvs <- (mvsc - 0.67)/sqrt(mvsc*(1-mvsc)/nrow(mvs))
# 2*pnorm(-abs(zmvs))

mvs1a <- logit2prob(coef(mvs1)[1]) # dissimilar = 1 and similar = 0
binom.test(round(mvs1a*nrow(mvs),0), nrow(mvs), p = 0.33, alternative = "two.sided")


#linearHypothesis(mvs1, "(Intercept) = 0.70818505792")

############# movie outlier #################
mvo <- mv[which(mv$effect == 1),]
mvo1 <- glm(resp ~ pre*attdiffc, data = mvo, family = binomial(link="logit"))
summary(mvo1)
# mvoc <- (exp(coef(mvo1)[1])/(1+exp(coef(mvo1)[1])))
# zmvo <- (mvoc - 0.875)/sqrt(mvoc*(1-mvoc)/nrow(mvo))
# 2*pnorm(-abs(zmvo))

mvo1a <- logit2prob(coef(mvo1)[1]) # dissimilar = 1 and similar = 0
binom.test(round(mvo1a*nrow(mvo),0), nrow(mvo), p = 0.125, alternative = "two.sided")

#linearHypothesis(mvo1, "(Intercept) = 1.94591014906")


###################### all scenarios ######################
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

# ef$lot.mov <- factor(c(rep(1, nrow(oe)), rep(0, nrow(oe)), rep(0, nrow(oe)), rep(0, nrow(oe))))
# ef$res.mov <- factor(c(rep(0, nrow(oe)), rep(1, nrow(oe)), rep(0, nrow(oe)), rep(0, nrow(oe))))
# ef$can.mov <- factor(c(rep(0, nrow(oe)), rep(0, nrow(oe)), rep(1, nrow(oe)), rep(0, nrow(oe))))

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
# code = -0.5 if the "more attractive" option is a dissimilar option
# code = 0.5 if it is a similar option

ef$pre <- NA
for (i in 1:nrow(ef)){
  if (ef$name[i] == "lot60s" | ef$name[i] == "lot60o" | ef$name[i] == "res40s" | ef$name[i] == "res40o" |
      ef$name[i] == "can33s" | ef$name[i] == "can33o" | ef$name[i] == "mov66s" | ef$name[i] == "mov66o")
  {ef$pre[i] <- -0.5} else {ef$pre[i] <- 0.5}
}

ef$attdiff <- ef$att1 - ef$att2

# mean centering
ef$attdiffc <- ef$attdiff - mean(ef$attdiff, na.rm = T)

# listwise deletion for NA responses
dt <- na.omit(ef)

# recode to 1 (dissimilar options) and 0 (similar option)
for (i in 1:nrow(dt)){
    if(dt$resp[i] > 1) {dt$resp[i] <- 0}
}

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


######### binomial test for all scenarios and each condition #############
binom.test(nrow(dt[which(dt$effect == 0 & dt$resp == 1),]),nrow(dt[which(dt$effect == 0),]), p = 0.33, alternative = "two.sided")
binom.test(nrow(dt[which(dt$effect == 1 & dt$resp == 1),]),nrow(dt[which(dt$effect == 1),]), p = 0.125, alternative = "two.sided")
# when removing 17 samples from candidate condition (outlier effect)
dt2 <- dt
dt2<-dt2[-c(763:779),]
binom.test(nrow(dt2[which(dt2$effect == 1 & dt2$resp == 1),]),nrow(dt2[which(dt2$effect == 1),]), p = 0.125, alternative = "two.sided")

############### similarity logistic #############
simsim <- dt[which(dt$effect == 0),]
sims <- glm(resp ~ pre*attdiffc, data = simsim, family = binomial(link="logit"))
summary(sims)


ssiim <- logit2prob(coef(sims)[1]) # dissimilar = 1 and similar = 0
binom.test(round(ssiim*nrow(simsim),0), nrow(simsim), p = 0.33, alternative = "two.sided")

############### outlier logistic #############
outout <- dt[which(dt$effect == 1),]
outo <- glm(resp ~ pre*attdiffc, data = outout, family = binomial(link="logit"))
summary(outo)


oouut <- logit2prob(coef(outo)[1]) # dissimilar = 1 and similar = 0
binom.test(round(oouut*nrow(outout),0), nrow(outout), p = 0.125, alternative = "two.sided")


# logistic regression
# mod1 <- glm(resp ~ lot.mov + res.mov + can.mov + effect + lot.mov*effect + res.mov*effect + can.mov*effect, family = binomial(link="logit"), data = dt)
# summary(mod1) # doesn't work due to redundancy of independent variables

# # similarity condition for all scenarios
# si <- dt[which(dt$effect == 0),]
# si1 <- glm(resp ~ pre*attdiffc + lot.mov + res.mov + can.mov, data = si)
# summary(si1)
# logit2prob(coef(si1)[1]) # dissimilar = 0 and similar = 1
# linearHypothesis(si1, "(Intercept) = 0.70818505792")
# 
# 
# 
# # outlier condition for all scenarios
# ou <- dt[which(dt$effect == 1),]
# ou1 <- glm(resp ~ pre*attdiffc + lot.mov + res.mov + can.mov, data = ou)
# summary(ou1)
# logit2prob(coef(ou1)[1]) # dissimilar = 0 and similar = 1
# linearHypothesis(ou1, "(Intercept) = 1.94591014906")



############################ multinomial model ##############################
# do not recode the responses to 0 and 1, but keep 1,2,3,4,...,8
# use the data frame "ef"
mul <- na.omit(ef)
# reference group for dv is dissimilar option
# use Helmert coding for presentation variable
for (i in 1:nrow(mul)){
  if (mul$name[i] == "lot60s" | mul$name[i] == "lot60o" | mul$name[i] == "res40s" | mul$name[i] == "res40o" |
      mul$name[i] == "can33s" | mul$name[i] == "can33o" | mul$name[i] == "mov66s" | mul$name[i] == "mov66o")
  {mul$pre[i] <- -0.5} else {mul$pre[i] <- 0.5}
}

# # similarity condition
# # gambling as reference
# simm <- mul[which(mul$effect == 0),]
# mulsim <- multinom(resp ~ pre*attdiffc + lot.mov + res.mov + can.mov, data = simm)
# summary(mulsim)
# zsim <- summary(mulsim)$coefficients/summary(mulsim)$standard.errors; zsim # z statistic
# # 2-tailed z test
# psim <- (1 - pnorm(abs(zsim), 0, 1)) * 2
# psim
# # get coefficients for intercepts and transform them to prob >>> The results show prob of sim relative to dissim. Must reverse it to dissim
# probsim <- 1-(logit2prob(coef(mulsim)[,1]))
# # compute total prob of dissim being chosen 
# simdis <- probsim[1:2] %>% sum()/2
# # test if different from 0.33, using z test ????




####################### Testing each scenario separately with multinomial ########################
################## gambling #################
gbmul <- data.frame(resp = c(oe$lot50s, oe$lot60s, oe$lot50o, oe$lot60o),
                 name = (c(rep(c("lot50s"), length(oe$lot50s)),
                           rep(c("lot60s"), length(oe$lot60s)),
                           rep(c("lot50o"), length(oe$lot50o)),
                           rep(c("lot60o"), length(oe$lot60o)))),
                 att1 = c(oe$plot50s_1, oe$plot60s_1, oe$plot50o_1, oe$plot60o_1),
                 att2 = c(oe$plot50s_2, oe$plot60s_2, oe$plot50o_2, oe$plot60o_2))

gbmul <- na.omit(gbmul)
gbmul$pre <- NA
for (i in 1:nrow(gbmul)){
  if (gbmul$name[i] == "lot60s" | gbmul$name[i] == "lot60o")
  {gbmul$pre[i] <- -0.5} else {gbmul$pre[i] <- 0.5} # Helmert coding
}

gbmul$effect <- NA
for (i in 1:nrow(gbmul)){
  if (gbmul$name[i] == "lot50s" | gbmul$name[i] == "lot60s")
  {gbmul$effect[i] <- 0} else {gbmul$effect[i] <- 1}
}

gbmul$attdiff <- gbmul$att1 - gbmul$att2
gbmul$attdiffc <- gbmul$attdiff - mean(gbmul$attdiff)

################## gambling similarity #################
gbsmul <- gbmul[which(gbmul$effect == 0),]
mulsimgb <- multinom(resp ~ pre*attdiffc, data = gbsmul)
summary(mulsimgb)
zsimgb <- summary(mulsimgb)$coefficients/summary(mulsimgb)$standard.errors; zsimgb # z statistic
# 2-tailed z test
psimgb <- (1 - pnorm(abs(zsimgb), 0, 1)) * 2
psimgb

# get prob from the intercepts
disgb <- 1/(1+sum(logit2odds(coef(mulsimgb)[,1])))

binom.test(round(disgb*nrow(gbsmul),0), nrow(gbsmul), p = 0.33, alternative = "two.sided")

round(1/(1+sum(logit2odds(coef(mulsimgb)[,1]))),2)


disgb2 <- round(logit2odds(coef(mulsimgb)[1,1])/(1+sum(logit2odds(coef(mulsimgb)[,1]))),2) 
binom.test(round(disgb2*nrow(gbsmul),0), nrow(gbsmul), p = 0.33, alternative = "two.sided")

disgb3 <- round(logit2odds(coef(mulsimgb)[2,1])/(1+sum(logit2odds(coef(mulsimgb)[,1]))),2) 
binom.test(round(disgb3*nrow(gbsmul),0), nrow(gbsmul), p = 0.33, alternative = "two.sided")
# no one chose the 3th 

################## gambling outlier #################
gbomul <- gbmul[which(gbmul$effect == 1),]
muloutgb <- multinom(resp ~ pre*attdiffc, data = gbomul)
summary(muloutgb)
zoutgb <- summary(muloutgb)$coefficients/summary(muloutgb)$standard.errors; zoutgb # z statistic
# 2-tailed z test
poutgb <- (1 - pnorm(abs(zoutgb), 0, 1)) * 2
poutgb

# get prob from the intercepts
disgbo <- 1/(1+sum(logit2odds(coef(muloutgb)[,1]))) 
binom.test(round(disgbo*nrow(gbomul),0), nrow(gbomul), p = 0.125, alternative = "two.sided")
round(1/(1+sum(logit2odds(coef(muloutgb)[,1]))),2)

disgbo2 <- round(logit2odds(coef(muloutgb)[1,1])/(1+sum(logit2odds(coef(muloutgb)[,1]))),2) 
binom.test(round(disgbo2*nrow(gbomul),0), nrow(gbomul), p = 0.125, alternative = "two.sided")

disgbo3 <- round(logit2odds(coef(muloutgb)[2,1])/(1+sum(logit2odds(coef(muloutgb)[,1]))),2) 
binom.test(round(disgbo3*nrow(gbomul),0), nrow(gbomul), p = 0.125, alternative = "two.sided")

disgbo4 <- round(logit2odds(coef(muloutgb)[3,1])/(1+sum(logit2odds(coef(muloutgb)[,1]))),2)
binom.test(round(disgbo4*nrow(gbomul),0), nrow(gbomul), p = 0.125, alternative = "two.sided")

disgbo5 <- round(logit2odds(coef(muloutgb)[4,1])/(1+sum(logit2odds(coef(muloutgb)[,1]))),2) 
binom.test(round(disgbo5*nrow(gbomul),0), nrow(gbomul), p = 0.125, alternative = "two.sided")

disgbo6 <- round(logit2odds(coef(muloutgb)[5,1])/(1+sum(logit2odds(coef(muloutgb)[,1]))),2) 
binom.test(round(disgbo6*nrow(gbomul),0), nrow(gbomul), p = 0.125, alternative = "two.sided")

disgbo7 <- round(logit2odds(coef(muloutgb)[6,1])/(1+sum(logit2odds(coef(muloutgb)[,1]))),2) 
binom.test(round(disgbo7*nrow(gbomul),0), nrow(gbomul), p = 0.125, alternative = "two.sided")

disgbo8 <- round(logit2odds(coef(muloutgb)[7,1])/(1+sum(logit2odds(coef(muloutgb)[,1]))),2)
binom.test(round(disgbo8*nrow(gbomul),0), nrow(gbomul), p = 0.125, alternative = "two.sided")



################## restaurant #################
rtmul <- data.frame(resp = c(oe$res10s, oe$res40s, oe$res10o, oe$res40o),
                 name = (c(rep(c("res10s"), length(oe$res10s)),
                           rep(c("res40s"), length(oe$res40s)),
                           rep(c("res10o"), length(oe$res10o)),
                           rep(c("res40o"), length(oe$res40o)))),
                 att1 = c(oe$pres10s_1, oe$pres40s_1, oe$pres10o_1, oe$pres40o_1),
                 att2 = c(oe$pres10s_2, oe$pres40s_2, oe$pres10o_2, oe$pres40o_2))

rtmul <- na.omit(rtmul)

rtmul$pre <- NA # presentation
for (i in 1:nrow(rtmul)){
  if (rtmul$name[i] == "res40s" | rtmul$name[i] == "res40o")
  {rtmul$pre[i] <- -0.5} else {rtmul$pre[i] <- 0.5}
}

rtmul$effect <- NA
for (i in 1:nrow(rtmul)){
  if (rtmul$name[i] == "res40s" | rtmul$name[i] == "res10s")
  {rtmul$effect[i] <- 0} else {rtmul$effect[i] <- 1}
}

rtmul$attdiff <- rtmul$att1 - rtmul$att2
rtmul$attdiffc <- rtmul$attdiff - mean(rtmul$attdiff)


################## restaurant similarity #################
rtsmul <- rtmul[which(rtmul$effect == 0),]
mulsimrt <- multinom(resp ~ pre*attdiffc, data = rtsmul)
summary(mulsimrt)
zsimrt <- summary(mulsimrt)$coefficients/summary(mulsimrt)$standard.errors; zsimrt # z statistic
# 2-tailed z test
psimrt <- (1 - pnorm(abs(zsimrt), 0, 1)) * 2
psimrt

# get prob from the intercepts
disrt <- 1/(1+sum(logit2odds(coef(mulsimrt)[,1])))
binom.test(round(disrt*nrow(rtsmul),0), nrow(rtsmul), p = 0.33, alternative = "two.sided")
round(1/(1+sum(logit2odds(coef(mulsimrt)[,1]))),2)

disrt2 <- round(logit2odds(coef(mulsimrt)[1,1])/(1+sum(logit2odds(coef(mulsimrt)[,1]))),2) 
binom.test(round(disrt2*nrow(rtsmul),0), nrow(rtsmul), p = 0.33, alternative = "two.sided")

disrt3 <- round(logit2odds(coef(mulsimrt)[2,1])/(1+sum(logit2odds(coef(mulsimrt)[,1]))),2) 
binom.test(round(disrt3*nrow(rtsmul),0), nrow(rtsmul), p = 0.33, alternative = "two.sided")



################## restaurant outlier #################
rtomul <- rtmul[which(rtmul$effect == 1),]
muloutrt <- multinom(resp ~ pre*attdiffc, data = rtomul)
summary(muloutrt)
zoutrt <- summary(muloutrt)$coefficients/summary(muloutrt)$standard.errors; zoutrt # z statistic
# 2-tailed z test
poutrt <- (1 - pnorm(abs(zoutrt), 0, 1)) * 2
poutrt

# get prob from the intercepts
disrto <- 1/(1+sum(logit2odds(coef(muloutrt)[,1])))
binom.test(round(disrto*nrow(rtomul),0), nrow(rtomul), p = 0.125, alternative = "two.sided")
round(1/(1+sum(logit2odds(coef(muloutrt)[,1]))),2)

disrto2 <- round(logit2odds(coef(muloutrt)[1,1])/(1+sum(logit2odds(coef(muloutrt)[,1]))),2) 
binom.test(round(disrto2*nrow(rtomul),0), nrow(rtomul), p = 0.125, alternative = "two.sided")

disrto3 <- round(logit2odds(coef(muloutrt)[2,1])/(1+sum(logit2odds(coef(muloutrt)[,1]))),2) 
binom.test(round(disrto3*nrow(rtomul),0), nrow(rtomul), p = 0.125, alternative = "two.sided")

disrto4 <- round(logit2odds(coef(muloutrt)[3,1])/(1+sum(logit2odds(coef(muloutrt)[,1]))),2)
binom.test(round(disrto4*nrow(rtomul),0), nrow(rtomul), p = 0.125, alternative = "two.sided")

disrto5 <- round(logit2odds(coef(muloutrt)[4,1])/(1+sum(logit2odds(coef(muloutrt)[,1]))),2) 
binom.test(round(disrto5*nrow(rtomul),0), nrow(rtomul), p = 0.125, alternative = "two.sided")

disrto6 <- round(logit2odds(coef(muloutrt)[5,1])/(1+sum(logit2odds(coef(muloutrt)[,1]))),2) 
binom.test(round(disrto6*nrow(rtomul),0), nrow(rtomul), p = 0.125, alternative = "two.sided")

disrto7 <- round(logit2odds(coef(muloutrt)[6,1])/(1+sum(logit2odds(coef(muloutrt)[,1]))),2) 
binom.test(round(disrto7*nrow(rtomul),0), nrow(rtomul), p = 0.125, alternative = "two.sided")

disrto8 <- round(logit2odds(coef(muloutrt)[7,1])/(1+sum(logit2odds(coef(muloutrt)[,1]))),2)
binom.test(round(disrto8*nrow(rtomul),0), nrow(rtomul), p = 0.125, alternative = "two.sided")
# no one chose the 7th


################## candidate #################
cdmul <- data.frame(resp = c(oe$can33s, oe$can66s, oe$can33o, oe$can66o),
                 name = (c(rep(c("can33s"), length(oe$can33s)),
                           rep(c("can66s"), length(oe$can66s)),
                           rep(c("can33o"), length(oe$can33o)),
                           rep(c("can66o"), length(oe$can66o)))),
                 att1 = c(oe$pcan33s_1, oe$pcan66s_1, oe$pcan33o_1, oe$pcan66o_1),
                 att2 = c(oe$pcan33s_2, oe$pcan66s_2, oe$pcan33o_2, oe$pcan66o_2))

cdmul <- na.omit(cdmul)

cdmul$pre <- NA # presentation
for (i in 1:nrow(cdmul)){
  if (cdmul$name[i] == "can33s" | cdmul$name[i] == "can33o")
  {cdmul$pre[i] <- -0.5} else {cdmul$pre[i] <- 0.5}
}

cdmul$effect <- NA
for (i in 1:nrow(cdmul)){
  if (cdmul$name[i] == "can66s" | cdmul$name[i] == "can33s")
  {cdmul$effect[i] <- 0} else {cdmul$effect[i] <- 1}
}

cdmul$attdiff <- cdmul$att1 - cdmul$att2
cdmul$attdiffc <- cdmul$attdiff - mean(cdmul$attdiff)


################## candidate similarity #################
cdsmul <- cdmul[which(cdmul$effect == 0),]
mulsimcd <- multinom(resp ~ pre*attdiffc, data = cdsmul)
summary(mulsimcd)
zsimcd <- summary(mulsimcd)$coefficients/summary(mulsimcd)$standard.errors; zsimcd # z statistic
# 2-tailed z test
psimcd <- (1 - pnorm(abs(zsimcd), 0, 1)) * 2
psimcd

# get prob from the intercepts
discd <- 1/(1+sum(logit2odds(coef(mulsimcd)[,1])))
binom.test(round(discd*nrow(cdsmul),0), nrow(cdsmul), p = 0.33, alternative = "two.sided")
round(1/(1+sum(logit2odds(coef(mulsimcd)[,1]))),2)

discd2 <- round(logit2odds(coef(mulsimcd)[1,1])/(1+sum(logit2odds(coef(mulsimcd)[,1]))),2) 
binom.test(round(discd2*nrow(cdsmul),0), nrow(cdsmul), p = 0.33, alternative = "two.sided")

discd3 <- round(logit2odds(coef(mulsimcd)[2,1])/(1+sum(logit2odds(coef(mulsimcd)[,1]))),2) 
binom.test(round(discd3*nrow(cdsmul),0), nrow(cdsmul), p = 0.33, alternative = "two.sided")


################## candidate outlier #################
cdomul <- cdmul[which(cdmul$effect == 1),]
muloutcd <- multinom(resp ~ pre*attdiffc, data = cdomul)
summary(muloutcd)
zoutcd <- summary(muloutcd)$coefficients/summary(muloutcd)$standard.errors; zoutcd # z statistic
# 2-tailed z test
poutcd <- (1 - pnorm(abs(zoutcd), 0, 1)) * 2
poutcd


# get prob from the intercepts
discdo <- 1/(1+sum(logit2odds(coef(muloutcd)[,1])))
binom.test(round(discdo*nrow(cdomul),0), nrow(cdomul), p = 0.125, alternative = "two.sided")

round(1/(1+sum(logit2odds(coef(muloutcd)[,1]))),2)

discdo2 <- round(logit2odds(coef(muloutcd)[1,1])/(1+sum(logit2odds(coef(muloutcd)[,1]))),2) 
binom.test(round(discdo2*nrow(cdomul),0), nrow(cdomul), p = 0.125, alternative = "two.sided")

discdo3 <- round(logit2odds(coef(muloutcd)[2,1])/(1+sum(logit2odds(coef(muloutcd)[,1]))),2) 
binom.test(round(discdo3*nrow(cdomul),0), nrow(cdomul), p = 0.125, alternative = "two.sided")

discdo4 <- round(logit2odds(coef(muloutcd)[3,1])/(1+sum(logit2odds(coef(muloutcd)[,1]))),2)
binom.test(round(discdo4*nrow(cdomul),0), nrow(cdomul), p = 0.125, alternative = "two.sided")

discdo5 <- round(logit2odds(coef(muloutcd)[4,1])/(1+sum(logit2odds(coef(muloutcd)[,1]))),2) 
binom.test(round(discdo5*nrow(cdomul),0), nrow(cdomul), p = 0.125, alternative = "two.sided")

discdo6 <- round(logit2odds(coef(muloutcd)[5,1])/(1+sum(logit2odds(coef(muloutcd)[,1]))),2) 
binom.test(round(discdo6*nrow(cdomul),0), nrow(cdomul), p = 0.125, alternative = "two.sided")

discdo7 <- round(logit2odds(coef(muloutcd)[6,1])/(1+sum(logit2odds(coef(muloutcd)[,1]))),2) 
binom.test(round(discdo7*nrow(cdomul),0), nrow(cdomul), p = 0.125, alternative = "two.sided")

discdo8 <- round(logit2odds(coef(muloutcd)[7,1])/(1+sum(logit2odds(coef(muloutcd)[,1]))),2)
binom.test(round(discdo8*nrow(cdomul),0), nrow(cdomul), p = 0.125, alternative = "two.sided")



################## candidate - 17 #################
cd1mul <- data.frame(resp = c(oe$can33s, oe$can66s, can2$can33o, oe$can66o),
                     name = (c(rep(c("can33s"), length(oe$can33s)),
                               rep(c("can66s"), length(oe$can66s)),
                               rep(c("can33o"), length(can2$can33o)),
                               rep(c("can66o"), length(oe$can66o)))),
                     att1 = c(oe$pcan33s_1, oe$pcan66s_1, can2$pcan33o_1, oe$pcan66o_1),
                     att2 = c(oe$pcan33s_2, oe$pcan66s_2, can2$pcan33o_2, oe$pcan66o_2))

cd1mul$pre <- NA # presentation
for (i in 1:nrow(cd1mul)){
  if (cd1mul$name[i] == "can33s" | cd1mul$name[i] == "can33o")
  {cd1mul$pre[i] <- -0.5} else {cd1mul$pre[i] <- 0.5}
}

cd1mul$effect <- NA
for (i in 1:nrow(cd1mul)){
  if (cd1mul$name[i] == "can66s" | cd1mul$name[i] == "can33s")
  {cd1mul$effect[i] <- 0} else {cd1mul$effect[i] <- 1}
}

cd1mul <- na.omit(cd1mul)

cd1mul$attdiff <- cd1mul$att1 - cd1mul$att2
cd1mul$attdiffc <- cd1mul$attdiff - mean(cd1mul$attdiff)



################## candidate -17 outlier #################
cd1omul <- cd1mul[which(cd1mul$effect == 1),]
muloutcd1 <- multinom(resp ~ pre*attdiffc, data = cd1omul)
summary(muloutcd1)
zoutcd1 <- summary(muloutcd1)$coefficients/summary(muloutcd1)$standard.errors; zoutcd1 # z statistic
# 2-tailed z test
poutcd1 <- (1 - pnorm(abs(zoutcd1), 0, 1)) * 2
poutcd1


# get prob from the intercepts
discd1o <- 1/(1+sum(logit2odds(coef(muloutcd1)[,1])))
binom.test(round(discd1o*nrow(cd1omul),0), nrow(cd1omul), p = 0.125, alternative = "two.sided")

round(1/(1+sum(logit2odds(coef(muloutcd1)[,1]))),2)

discd1o2 <- round(logit2odds(coef(muloutcd1)[1,1])/(1+sum(logit2odds(coef(muloutcd1)[,1]))),2) 
binom.test(round(discd1o2*nrow(cd1omul),0), nrow(cd1omul), p = 0.125, alternative = "two.sided")

discd1o3 <- round(logit2odds(coef(muloutcd1)[2,1])/(1+sum(logit2odds(coef(muloutcd1)[,1]))),2) 
binom.test(round(discd1o3*nrow(cd1omul),0), nrow(cd1omul), p = 0.125, alternative = "two.sided")

discd1o4 <- round(logit2odds(coef(muloutcd1)[3,1])/(1+sum(logit2odds(coef(muloutcd1)[,1]))),2)
binom.test(round(discd1o4*nrow(cd1omul),0), nrow(cd1omul), p = 0.125, alternative = "two.sided")

discd1o5 <- round(logit2odds(coef(muloutcd1)[4,1])/(1+sum(logit2odds(coef(muloutcd1)[,1]))),2) 
binom.test(round(discd1o5*nrow(cd1omul),0), nrow(cd1omul), p = 0.125, alternative = "two.sided")

discd1o6 <- round(logit2odds(coef(muloutcd1)[5,1])/(1+sum(logit2odds(coef(muloutcd1)[,1]))),2) 
binom.test(round(discd1o6*nrow(cd1omul),0), nrow(cd1omul), p = 0.125, alternative = "two.sided")

discd1o7 <- round(logit2odds(coef(muloutcd1)[6,1])/(1+sum(logit2odds(coef(muloutcd1)[,1]))),2) 
binom.test(round(discd1o7*nrow(cd1omul),0), nrow(cd1omul), p = 0.125, alternative = "two.sided")

discd1o8 <- round(logit2odds(coef(muloutcd1)[7,1])/(1+sum(logit2odds(coef(muloutcd1)[,1]))),2)
binom.test(round(discd1o8*nrow(cd1omul),0), nrow(cd1omul), p = 0.125, alternative = "two.sided")


################## movie #################
mvmul <- data.frame(resp = c(oe$mov33s, oe$mov66s, oe$mov33o, oe$mov66o),
                 name = (c(rep(c("mov33s"), length(oe$mov33s)),
                           rep(c("mov66s"), length(oe$mov66s)),
                           rep(c("mov33o"), length(oe$mov33o)),
                           rep(c("mov66o"), length(oe$mov66o)))),
                 att1 = c(oe$pmov33s_1, oe$pmov66s_1, oe$pmov33o_1, oe$pmov66o_1),
                 att2 = c(oe$pmov33s_2, oe$pmov66s_2, oe$pmov33o_2, oe$pmov66o_2))


mvmul$pre <- NA # presentation
for (i in 1:nrow(mvmul)){
  if (mvmul$name[i] == "mov66s" | mvmul$name[i] == "mov66o")
  {mvmul$pre[i] <- 0} else {mvmul$pre[i] <- 1}
}

mvmul$effect <- NA
for (i in 1:nrow(mvmul)){
  if (mvmul$name[i] == "mov66s" | mvmul$name[i] == "mov33s")
  {mvmul$effect[i] <- 0} else {mvmul$effect[i] <- 1}
}

mvmul <- na.omit(mvmul)

mvmul$attdiff <- mvmul$att1 - mvmul$att2
mvmul$attdiffc <- mvmul$attdiff - mean(mvmul$attdiff)


################## movie similarity #################
mvsmul <- mvmul[which(mvmul$effect == 0),]
mulsimmv <- multinom(resp ~ pre*attdiffc, data = mvsmul)
summary(mulsimmv)
zsimmv <- summary(mulsimmv)$coefficients/summary(mulsimmv)$standard.errors; zsimmv # z statistic
# 2-tailed z test
psimmv <- (1 - pnorm(abs(zsimmv), 0, 1)) * 2
psimmv


# get prob from the intercepts
dismv <- 1/(1+sum(logit2odds(coef(mulsimmv)[,1]))) 
binom.test(round(dismv*nrow(mvsmul),0), nrow(mvsmul), p = 0.33, alternative = "two.sided")
round(1/(1+sum(logit2odds(coef(mulsimmv)[,1]))),2)

dismv2 <- round(logit2odds(coef(mulsimmv)[1,1])/(1+sum(logit2odds(coef(mulsimmv)[,1]))),2) 
binom.test(round(dismv2*nrow(mvsmul),0), nrow(mvsmul), p = 0.33, alternative = "two.sided")

dismv3 <- round(logit2odds(coef(mulsimmv)[2,1])/(1+sum(logit2odds(coef(mulsimmv)[,1]))),2) 
binom.test(round(dismv3*nrow(mvsmul),0), nrow(mvsmul), p = 0.33, alternative = "two.sided")



################## movie outlier #################
mvomul <- mvmul[which(mvmul$effect == 1),]
muloutmv <- multinom(resp ~ pre*attdiffc, data = mvomul)
summary(muloutmv)
zoutmv <- summary(muloutmv)$coefficients/summary(muloutmv)$standard.errors; zoutmv # z statistic
# 2-tailed z test
poutmv <- (1 - pnorm(abs(zoutmv), 0, 1)) * 2
poutmv

# get prob from the intercepts
dismvo <- 1/(1+sum(logit2odds(coef(muloutmv)[,1]))) 
binom.test(round(dismvo*nrow(mvomul),0), nrow(mvomul), p = 0.125, alternative = "two.sided")
round(1/(1+sum(logit2odds(coef(muloutmv)[,1]))),2)

dismvo2 <- round(logit2odds(coef(muloutmv)[1,1])/(1+sum(logit2odds(coef(muloutmv)[,1]))),2) 
binom.test(round(dismvo2*nrow(mvomul),0), nrow(mvomul), p = 0.125, alternative = "two.sided")

dismvo3 <- round(logit2odds(coef(muloutmv)[2,1])/(1+sum(logit2odds(coef(muloutmv)[,1]))),2) 
binom.test(round(dismvo3*nrow(mvomul),0), nrow(mvomul), p = 0.125, alternative = "two.sided")

dismvo4 <- round(logit2odds(coef(muloutmv)[3,1])/(1+sum(logit2odds(coef(muloutmv)[,1]))),2)
binom.test(round(dismvo4*nrow(mvomul),0), nrow(mvomul), p = 0.125, alternative = "two.sided")

dismvo5 <- round(logit2odds(coef(muloutmv)[4,1])/(1+sum(logit2odds(coef(muloutmv)[,1]))),2) 
binom.test(round(dismvo5*nrow(mvomul),0), nrow(mvomul), p = 0.125, alternative = "two.sided")

dismvo6 <- round(logit2odds(coef(muloutmv)[5,1])/(1+sum(logit2odds(coef(muloutmv)[,1]))),2) 
binom.test(round(dismvo6*nrow(mvomul),0), nrow(mvomul), p = 0.125, alternative = "two.sided")

dismvo7 <- round(logit2odds(coef(muloutmv)[6,1])/(1+sum(logit2odds(coef(muloutmv)[,1]))),2) 
binom.test(round(dismvo7*nrow(mvomul),0), nrow(mvomul), p = 0.125, alternative = "two.sided")

dismvo8 <- round(logit2odds(coef(muloutmv)[7,1])/(1+sum(logit2odds(coef(muloutmv)[,1]))),2)
binom.test(round(dismvo8*nrow(mvomul),0), nrow(mvomul), p = 0.125, alternative = "two.sided")


####### Multinomial with all scenarios #########

# similarity effect
muls <- mul[which(mul$effect == 0),]
mulsim <- multinom(resp ~ pre*attdiffc, data = muls)
summary(mulsim)
zs <- summary(mulsim)$coefficients/summary(mulsim)$standard.errors; zs # z statistic
# 2-tailed z test
ps <- (1 - pnorm(abs(zs), 0, 1)) * 2
ps


# get prob from the intercepts
dissim <- 1/(1+sum(logit2odds(coef(mulsim)[,1]))) 
binom.test(round(dissim*nrow(muls),0), nrow(muls), p = 0.33, alternative = "two.sided")
round(1/(1+sum(logit2odds(coef(mulsim)[,1]))),2)

dissim2 <- round(logit2odds(coef(mulsim)[1,1])/(1+sum(logit2odds(coef(mulsim)[,1]))),2) 
binom.test(round(dissim2*nrow(muls),0), nrow(muls), p = 0.33, alternative = "two.sided")

dissim3 <- round(logit2odds(coef(mulsim)[2,1])/(1+sum(logit2odds(coef(mulsim)[,1]))),2)
binom.test(round(dissim3*nrow(muls),0), nrow(muls), p = 0.33, alternative = "two.sided")




# outlier effect
mulo <- mul[which(mul$effect == 1),]
mulout <- multinom(resp ~ pre*attdiffc, data = mulo)
summary(mulout)
zo <- summary(mulout)$coefficients/summary(mulout)$standard.errors; zo # z statistic
# 2-tailed z test
po <- (1 - pnorm(abs(zo), 0, 1)) * 2
po
#mulout <- multinom(resp ~ pre + attdiffc, data = mulo) # interaction was not signi


# get prob from the intercepts
disout <- 1/(1+sum(logit2odds(coef(mulout)[,1])))
round(1/(1+sum(logit2odds(coef(mulout)[,1]))),2)
binom.test(round(disout*nrow(mulo),0), nrow(mulo), p = 0.125, alternative = "two.sided") # dissim

disout2 <- round(logit2odds(coef(mulout)[1,1])/(1+sum(logit2odds(coef(mulout)[,1]))),2) 
binom.test(round(disout2*nrow(mulo),0), nrow(mulo), p = 0.125, alternative = "two.sided")

disout3 <- round(logit2odds(coef(mulout)[2,1])/(1+sum(logit2odds(coef(mulout)[,1]))),2) 
binom.test(round(disout3*nrow(mulo),0), nrow(mulo), p = 0.125, alternative = "two.sided")

disout4 <- round(logit2odds(coef(mulout)[3,1])/(1+sum(logit2odds(coef(mulout)[,1]))),2)
binom.test(round(disout4*nrow(mulo),0), nrow(mulo), p = 0.125, alternative = "two.sided")

disout5 <- round(logit2odds(coef(mulout)[4,1])/(1+sum(logit2odds(coef(mulout)[,1]))),2) 
binom.test(round(disout5*nrow(mulo),0), nrow(mulo), p = 0.125, alternative = "two.sided")

disout6 <- round(logit2odds(coef(mulout)[5,1])/(1+sum(logit2odds(coef(mulout)[,1]))),2) 
binom.test(round(disout6*nrow(mulo),0), nrow(mulo), p = 0.125, alternative = "two.sided")

disout7 <- round(logit2odds(coef(mulout)[6,1])/(1+sum(logit2odds(coef(mulout)[,1]))),2) 
binom.test(round(disout7*nrow(mulo),0), nrow(mulo), p = 0.125, alternative = "two.sided")

disout8 <- round(logit2odds(coef(mulout)[7,1])/(1+sum(logit2odds(coef(mulout)[,1]))),2) 
binom.test(round(disout8*nrow(mulo),0), nrow(mulo), p = 0.125, alternative = "two.sided")





ef <- na.omit(ef)
ef$scenario <- NA
for (i in 1:nrow(ef)){
  if (ef$name[i] == "lot50s" | ef$name[i] == "lot60s" | ef$name[i] == "lot50o" | ef$name[i] == "lot60o")
  {ef$scenario[i] <- "gb"}
  if (ef$name[i] == "mov33s" | ef$name[i] == "mov33o" | ef$name[i] == "mov66s" | ef$name[i] == "mov66o")
  {ef$scenario[i] <- "mv"}
  if (ef$name[i] == "can33s" | ef$name[i] == "can33o" | ef$name[i] == "can66s" | ef$name[i] == "can66o")
  {ef$scenario[i] <- "cd"}
  if (ef$name[i] == "res10s" | ef$name[i] == "res10o" | ef$name[i] == "res40s" | ef$name[i] == "res40o")
  {ef$scenario[i] <- "rt"}
}






# binomial test for 17 resp omitted. Outlier only
cd1all <- rbind(cd1,mv,rt,gb)
binom.test(nrow(dt[which(cd1all$effect == 1 & cd1all$resp == 1),]),nrow(cd1all[which(cd1all$effect == 1),]), p = 0.125, alternative = "two.sided")





# multinomial logistic for 17 resp omitted
cd1mul <- rbind(mvomul, gbomul, rtomul, cd1omul)

cd1mulout <- multinom(resp ~ pre*attdiffc, data = cd1mul)
summary(cd1mulout)
zo1 <- summary(cd1mulout)$coefficients/summary(cd1mulout)$standard.errors; zo # z statistic
# 2-tailed z test
po1 <- (1 - pnorm(abs(zo1), 0, 1)) * 2
po1
#mulout <- multinom(resp ~ pre + attdiffc, data = mulo) # interaction was not signi


# get prob from the intercepts
cd1disout <- 1/(1+sum(logit2odds(coef(cd1mulout)[,1])))
round(1/(1+sum(logit2odds(coef(cd1mulout)[,1]))),2)
binom.test(round(cd1disout*nrow(cd1mul),0), nrow(cd1mul), p = 0.125, alternative = "two.sided") # dissim

cd1disout2 <- round(logit2odds(coef(cd1mulout)[1,1])/(1+sum(logit2odds(coef(cd1mulout)[,1]))),2) 
binom.test(round(cd1disout2*nrow(cd1mul),0), nrow(cd1mul), p = 0.125, alternative = "two.sided")

cd1disout3 <- round(logit2odds(coef(cd1mulout)[2,1])/(1+sum(logit2odds(coef(cd1mulout)[,1]))),2) 
binom.test(round(cd1disout3*nrow(cd1mul),0), nrow(cd1mul), p = 0.125, alternative = "two.sided")

cd1disout4 <- round(logit2odds(coef(cd1mulout)[3,1])/(1+sum(logit2odds(coef(cd1mulout)[,1]))),2)
binom.test(round(cd1disout4*nrow(cd1mul),0), nrow(cd1mul), p = 0.125, alternative = "two.sided")

cd1disout5 <- round(logit2odds(coef(cd1mulout)[4,1])/(1+sum(logit2odds(coef(cd1mulout)[,1]))),2) 
binom.test(round(cd1disout5*nrow(cd1mul),0), nrow(cd1mul), p = 0.125, alternative = "two.sided")

cd1disout6 <- round(logit2odds(coef(cd1mulout)[5,1])/(1+sum(logit2odds(coef(cd1mulout)[,1]))),2) 
binom.test(round(cd1disout6*nrow(cd1mul),0), nrow(cd1mul), p = 0.125, alternative = "two.sided")

cd1disout7 <- round(logit2odds(coef(cd1mulout)[6,1])/(1+sum(logit2odds(coef(cd1mulout)[,1]))),2) 
binom.test(round(cd1disout7*nrow(cd1mul),0), nrow(cd1mul), p = 0.125, alternative = "two.sided")

cd1disout8 <- round(logit2odds(coef(cd1mulout)[7,1])/(1+sum(logit2odds(coef(cd1mulout)[,1]))),2) 
binom.test(round(cd1disout8*nrow(cd1mul),0), nrow(cd1mul), p = 0.125, alternative = "two.sided")



# prop.table(table(ef[which(ef$effect == 0), c("resp")]))
# prop.table(table(ef[which(ef$effect == 1), c("resp")]))
# prop.table(table(muls[, c("resp")]))
# prop.table(table(mulo[, c("resp")]))
# 
# prop.table(table(ef[which(ef$effect == 0 & ef$scenario == "gb"), c("resp")]))
# prop.table(table(ef[which(ef$effect == 1 & ef$scenario == "gb"), c("resp")]))
# prop.table(table(gbmul[which(gbmul$effect == 0), c("resp")]))
# prop.table(table(gbmul[which(gbmul$effect == 1), c("resp")]))
# 
# 
# 
# prop.table(table(ef[which(ef$effect == 0 & ef$scenario == "rt"), c("resp")]))
# prop.table(table(ef[which(ef$effect == 1 & ef$scenario == "rt"), c("resp")]))
# prop.table(table(rtmul[which(rtmul$effect == 0), c("resp")]))
# prop.table(table(rtmul[which(rtmul$effect == 1), c("resp")]))
# 
# 
# prop.table(table(cdmul[which(cdmul$effect == 0), c("resp")]))
# prop.table(table(cdmul[which(cdmul$effect == 1), c("resp")]))
# prop.table(table(cd1mul[which(cd1mul$effect == 1), c("resp")]))
# 
# 
# prop.table(table(mvmul[which(mvmul$effect == 0), c("resp")]))
# prop.table(table(mvmul[which(mvmul$effect == 1), c("resp")]))
# 
# 
# 
# # weighting each presentation equally
# prop.table(table(muls[which(muls$pre == -0.5), c("resp")]))
# prop.table(table(muls[which(muls$pre == 0.5), c("resp")]))
# prop.table(table(mulo[which(mulo$pre == -0.5), c("resp")]))
# prop.table(table(mulo[which(mulo$pre == 0.5), c("resp")]))
# 
# 
# prop.table(table(gbmul[which(gbmul$effect == 0 & gbmul$pre == -0.5), c("resp")]))
# prop.table(table(gbmul[which(gbmul$effect == 0 & gbmul$pre == 0.5), c("resp")]))
# prop.table(table(gbmul[which(gbmul$effect == 1 & gbmul$pre == -0.5), c("resp")]))
# prop.table(table(gbmul[which(gbmul$effect == 1 & gbmul$pre == 0.5), c("resp")]))
# 
# 
# 
# prop.table(table(rtmul[which(rtmul$effect == 0 & rtmul$pre == -0.5), c("resp")]))
# prop.table(table(rtmul[which(rtmul$effect == 0 & rtmul$pre == 0.5), c("resp")]))
# prop.table(table(rtmul[which(rtmul$effect == 1 & rtmul$pre == -0.5), c("resp")]))
# prop.table(table(rtmul[which(rtmul$effect == 1 & rtmul$pre == 0.5), c("resp")]))
# 
# 
# prop.table(table(cdmul[which(cdmul$effect == 0), c("resp")]))
# prop.table(table(cdmul[which(cdmul$effect == 1), c("resp")]))
# prop.table(table(cd1mul[which(cd1mul$effect == 1), c("resp")]))
# 
# 
# prop.table(table(mvmul[which(mvmul$effect == 0), c("resp")]))
# prop.table(table(mvmul[which(mvmul$effect == 1), c("resp")]))

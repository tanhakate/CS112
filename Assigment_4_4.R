# Reload the data and remove rows where values are NA
foo <- read.csv("https://course-resources.minerva.kgi.edu/uploaded_files/mke/00086677-3767/peace.csv")
foo <- foo[c(-19, -47), ]
foo <- foo[!is.na(foo$pbs2l),]
foo <- foo[!is.na(foo$pbs5l),]
# redefine treatment according to instructions
Tr <- rep(0, length(foo$uncint))
Tr[which(foo$uncint!= "None" & foo$uncint != "1")] <- 1
foo$Tr <- Tr

# binary for pbs2l
foo$pbs2l <- ifelse(foo$pbs2l=="Failure",0,1) #recoding variables

# binary for pbs2l
foo$pbs5l <- ifelse(foo$pbs5l=="Failure",0,1) #recoding variables

library(Matching)
#Balance for the logistic regression model

match_balance1 <- MatchBalance(Tr~wartype+logcost+wardur+factnum+factnum2+trnsfcap+develop+exp+decade+treaty, data=foo, nboots = 1000)

# Logistic regression approach: Two years
glm3 <- glm(pbs2l ~ wartype + logcost + wardur + factnum + factnum2 + trnsfcap+develop+exp+decade+treaty+Tr, data = foo[!NAs,], family ="binomial")
summary(glm3)
# Logistic regression approach: Five years
glm4 <- glm(pbs5l ~ wartype + logcost + wardur + factnum + factnum2 + trnsfcap + develop + exp + decade + treaty + Tr, data = foo[!NAs,], family = "binomial")
summary(glm4)


# Here, we will reverse the values of the treatment assignment to predict
# the counterfactual peace keeping success if these regions did not recieve 
# any intrusive peacekeeping operation

# Two years
foo.counter_factual <- foo[!NAs,]
foo.counter_factual$Tr <- rep(1,nrow(foo[!NAs,])) - foo$Tr
counter_factuals <- predict(glm3, newdata=foo.counter_factual, type="response")
unit_treat_effects <- rep(NA, nrow(foo))

treated_units <- foo$Tr == 1
unit_treat_effects[treated_units] <- glm3$fitted.values[treated_units] - counter_factuals[treated_units]
unit_treat_effects[!treated_units] <- counter_factuals[!treated_units] - glm3$fitted.values[!treated_units]
mean(unit_treat_effects)

# Five years
foo.counter_factual <- foo[!NAs,]
foo.counter_factual$Tr <- rep(1,nrow(foo[!NAs,])) - foo$Tr[!NAs]
counter_factuals <- predict(glm4, newdata=foo.counter_factual, type="response")
unit_treat_effects <- rep(NA, nrow(foo[!NAs,]))

treated_units <- foo$Tr == 1
unit_treat_effects[treated_units] <- glm4$fitted.values[treated_units] - counter_factuals[treated_units]
unit_treat_effects[!treated_units] <- counter_factuals[!treated_units] - glm4$fitted.values[!treated_units]
mean(unit_treat_effects)


## Propensity Score Matching

#p-score model for pbs21
glm5 <- glm(Tr~wartype + logcost + wardur + factnum + factnum2 + trnsfcap + develop + exp + decade + treaty, data = foo, family = "binomial")

summary(glm5)

X <- glm5$fitted.values
Y_2 <- foo$pbs2l
Y_5 <- foo$pbs5l

m1 <- Match (Y=Y_2[which(!is.na(Y_2))], Tr=Tr[which(!is.na(Y_2))], X=X[which(!is.na(Y_2))], M=1, BiasAdjust = T)
summary(m1)
#estimates without and with bias
m1$est.noadj
m1$est

#balance and get p value

mb_1 <- MatchBalance(Tr ~ wartype + logcost + wardur + factnum + factnum2 + trnsfcap + treaty + develop + exp + decade, data = foo, match.out = m1, nboots = 500)
mb_1$AMsmallest.p.value

#repeat for pbs5l

m2 <- Match(Y=Y_5[which(!is.na(Y_5))], Tr=Tr[which(!is.na(Y_5))], X=X[which(!is.na(Y_5))], M=1, BiasAdjust = T)
#estimates without and with bias
m2$est.noadj
m2$est
#balance and get p-value
mb_2 <- MatchBalance(Tr~wartype + logcost + wardur + factnum + factnum2 + trnsfcap + treaty + develop + exp + decade, data = foo, match.out = m2, nboots = 500)
mb_2$AMsmallest.p.value

## Genetic Matching

# for pbs2l
X=cbind(glm5$fitted.values,foo$wartype, foo$logcost, foo$wardur, foo$factnum, foo$factnum2, foo$trnsfcap, foo$treaty, foo$develop, foo$exp, foo$decade)

genout <- GenMatch (Tr=Tr, X=X, M=1, pop.size = 200, max.generations = 10, wait.generations = 25, replace = FALSE)
m3 <- Match(Y=Y_2[which(!is.na(Y_2))], Tr=Tr[which(!is.na(Y_2))], X=X[which(!is.na(Y_2))], M=1, BiasAdjust = T, Weight.matrix =genout)
m3$est # Bias Adjustment can only be estimated when ties==TRUE and replace=TRUE.  Setting BiasAdjust=FALSE
mb_3 <- MatchBalance(Tr ~ wartype + logcost + wardur + factnum + factnum2 + trnsfcap + develop + exp + decade + treaty, data = foo, match.out = m3, nboots=500)
mb_3$AMsmallest.p.value

m4<- Match(Y=Y_5[which(!is.na(Y_5))], Tr=Tr[which(!is.na(Y_5))], X=X[which(!is.na(Y_5)),], M=1, BiasAdjust = T, Weight.matrix = genout)
summary(m4)
m4$est
mb_4 <- MatchBalance(Tr ~ wartype + logcost + wardur + factnum + factnum2 + trnsfcap + develop + exp + decade + treaty, data = foo, match.out = m4, nboots=500)
mb_4$AMsmallest.p.value

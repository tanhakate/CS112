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
glm3 <- glm(pbs2l ~ wartype + logcost + wardur + factnum + factnum2 + trnsfcap+develop+exp+decade+treaty+Tr, data = foo, family ="binomial")
summary(glm3)
# Logistic regression approach: Five years
glm4 <- glm(pbs5l ~ wartype + logcost + wardur + factnum + factnum2 + trnsfcap + develop + exp + decade + treaty + Tr, data = foo, family = "binomial")
summary(glm4)

## Propensity Score Matching

#p-score model 
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
Xs=cbind(glm5$fitted.values,foo$wartype, foo$logcost, foo$wardur, foo$factnum, foo$factnum2, foo$trnsfcap, foo$treaty, foo$develop, foo$exp, foo$decade)

genout <- GenMatch (Tr=Tr, X=Xs, M=1, pop.size = 200, max.generations = 30, wait.generations = 10, replace = FALSE)

m3 <- Match(Y=Y_2[which(!is.na(Y_2))], Tr=Tr[which(!is.na(Y_2))], X=Xs[which(!is.na(Y_2))], M=1, BiasAdjust = TRUE, Weight.matrix =genout)

m3$est # Bias Adjustment can only be estimated when ties==TRUE and replace=TRUE.  Setting BiasAdjust=FALSE

mb_3 <- MatchBalance(Tr ~ wartype + logcost + wardur + factnum + factnum2 + trnsfcap + develop + exp + decade + treaty, data = foo, match.out = m3, nboots=500)
mb_3$AMsmallest.p.value


genout_2 <- GenMatch (Tr=Tr, X=Xs, M=1, pop.size = 200, max.generations = 30, wait.generations = 10, replace = FALSE)

m4<- Match(Y=Y_5[which(!is.na(Y_5))], Tr=Tr[which(!is.na(Y_5))], X=Xs[which(!is.na(Y_5)),], M=1, BiasAdjust = T, Weight.matrix = genout_2)
summary(m4)
m4$est
mb_4 <- MatchBalance(Tr ~ wartype + logcost + wardur + factnum + factnum2 + trnsfcap + develop + exp + decade + treaty, data = foo, match.out = m4, nboots=500)
mb_4$AMsmallest.p.value

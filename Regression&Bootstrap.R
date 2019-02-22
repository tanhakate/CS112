## Question 1

seed(123)
library("ggplot2")
years_of_education <- runif(999,0,30) 
life_expectancy <- 71.5 + 0.15 * years_of_education
life_expectancy_with_error <- life_expectancy + rnorm(999,0,1)
df <- data.frame(Years = years_of_education, Expectancy = life_expectancy_with_error)
ggplot(df, aes(x=Years, y=Expectancy)) + geom_point(size=0.5, shape=23)

fit<- lm(life_expectancy_with_error~years_of_education)
summary(fit)

df_new <- rbind(df, c(0, 1222))
ggplot(df_new, aes(x=Years, y=Expectancy)) + geom_point(size=0.5, shape=23)
fit_with_outlier <- lm(Expectancy~Years, data=df_new)
summary(fit_with_outlier)

plot(df$Years, df$Expectancy, xlab = "Years of Education", ylab = "Life Expectancy (years)")
lines(df$Years, predict(fit), col = "blue")
lines(df_new$Years, predict(fit_with_outlier), col = "red")


ggplot(df_new) + geom_smooth(aes(x=Years, y=Expectancy), data=df, method=lm, se=FALSE, color ="blue") + geom_smooth(aes(x=Years, y=Expectancy), data=df_new, method=lm, se=FALSE, color = "red")  + labs(x = "Years of Education", y = "Life Expectancy (years)") + xlim(0,35) + ggtitle("Regression Line Based on 999 Points (Blue) and Based on 1000 points (Red)")


## Question 2

library(Matching)
library(arm)
data(lalonde)
df_control = subset(lalonde, treat == 0)

reg_control <- glm(re78 ~ age + educ + re74 + re75 + educ*re74 + educ*re75 + age*re74 + age*re75 + re74*re75, data=df_control, family = "binomial")
confint(reg_control)

med_educ=median(df_control$educ)
med_re74=median(df_control$re74)
med_re75=median(df_control$re75)

Simulation <- sim(reg_control, n.sims = 10000)

storagedf<- matrix(NA, nrow = 10000, ncol=39 )

for (age in 17:55){
  for(i in 1:10000){
    
    predictedYs <- Simulation@coef[i,1] * 1 + 
      Simulation@coef[i,2] * age + 
      Simulation@coef[i,3] * med_educ + 
      Simulation@coef[i,4] * med_re74 +
      Simulation@coef[i,5] * med_re75 +
      Simulation@coef[i,6] * med_educ*med_re74 +
      Simulation@coef[i,7] * med_educ*med_re75 +
      Simulation@coef[i,8] * age*med_re74 +
      Simulation@coef[i,9] * age*med_re75 +
      Simulation@coef[i,10] * med_re74*med_re75 
    #+ rnorm(1,0,Simulation@sigma[i])
    # put preductedYs into a matrix
      storagedf[i,age-16] = predictedYs
      
  }
}

head(storagedf)

lowbounds.medians = rep(NA, 39)
upperbounds.medians = rep(NA, 39)
age <- c(17:55)

for (i in 1:ncol(storagedf)) {
  lowbounds.medians[i] = quantile(storagedf[,i],0.025)
  upperbounds.medians[i] = quantile(storagedf[,i],0.975)
  
}

data.frame(Age = as.numeric(age_list), Lower_Quantile = lowbounds.medians, Upper_Quantile =upperbounds.medians, med_educ, med_re74, med_re75, med_educ*med_re74, med_educ*med_re75, age*med_re74, age*med_re75, med_re74*med_re75)


confidence_intervals_one <- quantile(storagedf, probs = c(0.025, 0.975))


quant_re74 = quantile(df_control$re74)[4]
quant_educ = quantile(df_control$educ)[4]
quant_re75 = quantile(df_control$re75)[4]

storagedf1<- matrix(NA, nrow = 10000, ncol=39 )

for (age in 17:55){
  for(i in 1:10000){
    
    predictedYs <- Simulation@coef[i,1] * 1 + 
      Simulation@coef[i,2] * age + 
      Simulation@coef[i,3] * quant_educ + 
      Simulation@coef[i,4] * quant_re74 +
      Simulation@coef[i,5] * quant_re75 +
      Simulation@coef[i,6] * quant_educ*quant_re74 +
      Simulation@coef[i,7] * quant_educ*quant_re75 +
      Simulation@coef[i,8] * age*quant_re74 +
      Simulation@coef[i,9] * age*quant_re75 +
      Simulation@coef[i,10] * quant_re74*quant_re75 
    #+ rnorm(1,0,Simulation@sigma[i])
    # put preductedYs into a matrix
    storagedf1[i,age-16] = predictedYs
    
  }
}

head(storagedf1)

lowbounds.medians1 = rep(NA, 39)
upperbounds.medians1 = rep(NA, 39)

for (i in 1:ncol(storagedf1)) {
  lowbounds.medians1[i] = quantile(storagedf1[,i],0.025)
  upperbounds.medians1[i] = quantile(storagedf1[,i],0.975)
  
}

data.frame(Age = as.numeric(age_list), Lower_Quantile = lowbounds.medians1, Upper_Quantile =upperbounds.medians1, quant_educ, quant_re74, quant_re75, quant_educ*quant_re74, quant_educ*quant_re75, age*quant_re74, age*quant_re75, quant_re74*quant_re75)

Simulation <- sim(reg_control, n.sims = 10000)



med_educ=median(df_control$educ)
med_re74=median(df_control$re74)
med_re75=median(df_control$re75)

storagedf2<- matrix(NA, nrow = 10000, ncol=39 )

for (age in 17:55){
  for(i in 1:10000){
    
    predictedYs <- Simulation@coef[i,1] * 1 + 
      Simulation@coef[i,2] * age + 
      Simulation@coef[i,3] * med_educ + 
      Simulation@coef[i,4] * med_re74 +
      Simulation@coef[i,5] * med_re75 +
      Simulation@coef[i,6] * med_educ*med_re74 +
      Simulation@coef[i,7] * med_educ*med_re75 +
      Simulation@coef[i,8] * age*med_re74 +
      Simulation@coef[i,9] * age*med_re75 +
      Simulation@coef[i,10] * med_re74*med_re75 
    + rnorm(1,0,Simulation@sigma[i])
    # put preductedYs into a matrix
    storagedf2[i,age-16] = predictedYs
    
  }
}

lowbounds.medians2 = rep(NA, 39)
upperbounds.medians2 = rep(NA, 39)

for (i in 1:ncol(storagedf1)) {
  lowbounds.medians2[i] = quantile(storagedf2[,i],0.025)
  upperbounds.medians2[i] = quantile(storagedf2[,i],0.975)
  
}

data.frame(Age = as.numeric(age_list), Lower_Quantile = lowbounds.medians2, Upper_Quantile =upperbounds.medians2, med_educ, med_re74, med_re75, med_educ*med_re74, med_educ*med_re75, age*med_re74, age*med_re75, med_re74*med_re75)

storagedf3<- matrix(NA, nrow = 10000, ncol=39 )

for (age in 17:55){
  for(i in 1:10000){
    
    predictedYs <- Simulation@coef[i,1] * 1 + 
      Simulation@coef[i,2] * age + 
      Simulation@coef[i,3] * quant_educ + 
      Simulation@coef[i,4] * quant_re74 +
      Simulation@coef[i,5] * quant_re75 +
      Simulation@coef[i,6] * quant_educ*quant_re74 +
      Simulation@coef[i,7] * quant_educ*quant_re75 +
      Simulation@coef[i,8] * age*quant_re74 +
      Simulation@coef[i,9] * age*quant_re75 +
      Simulation@coef[i,10] * quant_re74*quant_re75 
      + rnorm(1,0,Simulation@sigma[i])
    # put preductedYs into a matrix
    storagedf3[i,age-16] = predictedYs
    
  }
}

head(storagedf3)

lowbounds.medians3 = rep(NA, 39)
upperbounds.medians3 = rep(NA, 39)

for (i in 1:ncol(storagedf3)) {
  lowbounds.medians3[i] = quantile(storagedf3[,i],0.025)
  upperbounds.medians3[i] = quantile(storagedf3[,i],0.975)
  
}

data.frame(Age = as.numeric(age_list), Lower_Quantile = lowbounds.medians3, Upper_Quantile =upperbounds.medians3,quant_educ, quant_re74, quant_re75, quant_educ*quant_re74, quant_educ*quant_re75, age*quant_re74, age*quant_re75, quant_re74*quant_re75)

plot(x=c(1,100), y = c(1,100), type = "n", xlim = c(17,55), ylim = c(500,10000),
     main = "95% Prediction Interval (PI) for re78", xlab = "Age of Subject",
     ylab = "95% PI")

for (age in 17:55) {
  segments(
    x0 = age,
    y0 = lowbounds.medians2[age - 16],
    x1 = age,
    y1 = upperbounds.medians2[age-16],
    lwd = 2,
    col="red")
}

legend("topleft", c("Using Median Values for educ, re74 and re75"), col=c("red"), lwd=4)

plot(x=c(1,100), y = c(1,100), type = "n", xlim = c(17,55), ylim = c(500,10000),
     main = "95% Prediction Interval (PI) for re78", xlab = "Age of Subject",
     ylab = "95% PI")

for (age in 17:55) {
  segments(
    x0 = age,
    y0 = lowbounds.medians3[age - 16],
    x1 = age,
    y1 = upperbounds.medians3[age-16],
    lwd = 2,
    col="blue")
}

legend("topleft", c("Using 75% quantiles for educ, re74 and re75"), col=c("blue"), lwd=4)

## Question 3

data(PlantGrowth)
df <- subset(PlantGrowth, group!="trt2") #removing rows in treatment2
df$group <- ifelse(df$group=="ctrl", 0,1) #recoding variables

model_1 <- lm(weight~group,data=df)


confint(model_1, level = 0.95)

storage <- rep(0,10000)

for (i in 1:10000) {
  
  sample_index <- sample(nrow(df), nrow(df), replace = T)
  model <- lm(weight ~ group, data = df, subset = sample_index)
  se <- summary(model)$coef[2]
  storage[i] <- se
}

quantile(storage, probs = c(0.025, 0.975))

## Question 4
r_squared <- function(x,y){ #x is actual y and y is predicted y's
  rss <- sum((y - x) ^ 2)  ## residual sum of squares
  tss <- sum((x - mean(x)) ^ 2)  ## total sum of squares
  rsq <- 1 - rss/tss
  print(rsq)
}

r_squared(df$weight,predict(model_1))

## Question 5
library('arm')

df <- read.dta("C:/Users/tanha/Desktop/Direction Year Syllabi/CS112/nsw.dta")
#no re78

log_model <- glm(treat~ age + education + black + hispanic + married + nodegree + re75, data = df, family="binomial")


treat_group <- subset(df, treat == 1)

control_group <- subset(df, treat == 0)
  
hist(predict(log_model, newdata=treat_group, type = "response"), xlab = "Estimated Probability", main="Estimated Probability of Treatment for Treatment Group", col = "red")

legend("topright", c("Treatment Group"), col=c("red"), lwd=4)

p2 <- hist(predict(log_model, newdata=control_group, type = "response"),xlab = "Estimated Probability", main="Estimated Probability of Treatment for Control Group", col = "blue") 
legend("topright", c("Control Group"), col=c("blue"), lwd=4)

# Replicate figure 8 in https://gking.harvard.edu/files/counterf.pdf

# Preparing the Dataset

foo <- read.csv("https://course-resources.minerva.kgi.edu/uploaded_files/mke/00086677-3767/peace.csv")
# extract relevant columns
foo <- foo[, c(6:8, 11:16, 99, 50, 114, 49, 63, 136, 109, 126, 48, 160, 142, 10)]
# remove missing data
foo <- foo[c(-19, -47), ]
which(is.na(foo) == TRUE)
# Take predictors at their means
mean_wartype <- mean(foo$wartype)
mean_factnum <- mean(foo$factnum)
mean_wardur <- mean(foo$wardur)
mean_factnum2 <- mean(foo$factnum2)
mean_trnsfcap <- mean(foo$trnsfcap)
mean_develop <- mean(foo$develop)
mean_exp <- mean(foo$exp)
mean_decade <- mean(foo$decade)
mean_treaty <- mean(foo$treaty)

# Train the two models (without and with interaction term)

glm1 <- glm(pbs2s3 ~ wartype + logcost + wardur + factnum + factnum2 + 
              trnsfcap + develop + exp + decade + treaty + untype4, 
            data = foo, family = "binomial")
summary(glm1)

glm2 <- glm(pbs2s3 ~ wartype + logcost + wardur + factnum + factnum2 + 
                 trnsfcap + develop + exp + decade + treaty + untype4 + I(untype4*logcost), 
               data = foo, family = "binomial")
summary(glm2)


get_logit <- function(X, coef) {
  logit <- coef[1] + sum(coef[2:length(coef)]*X)
  return(exp(logit) / (1 + exp(logit)))
}

#Create matrix to store logitresults
storage_treated <- rep(NA, 10)
storage_control <- rep(NA, 10)

# For each logcost
for (logcost in 6:16) {
  
  # Hypothetical nation with predictors held at their means, varying duration of war.
  # Predicting outcome of peacekeeping success when treatment = 1 and when treatment = 0
  X_treat <- c(mean_wartype, logcost, mean_wardur, mean_factnum, mean_factnum2, 
               mean_trnsfcap, mean_develop, mean_exp, mean_decade, mean_treaty, 1)
  X_control <- c(mean_wartype, logcost, mean_wardur, mean_factnum, mean_factnum2, 
                 mean_trnsfcap, mean_develop, mean_exp, mean_decade, mean_treaty, 0)
  
  storage_treated[logcost]  <- get_logit(X_treat, coef(glm1))
  storage_control[logcost]  <- get_logit(X_control, coef(glm1))
}

# treatment effect should be difference between treatment = 1 and treatment = 0

original_y <- storage_treated - storage_control


# Logit results for modified model
storage_treated_it <- rep(NA, 10)
storage_control_it <- rep(NA, 10)

for (logcost in 6:16) {
  # Interaction term values when treatment = 1 is 1*mean_logcost and when treatment = 0, it's just 0.
  X_treat <- c(mean_wartype, logcost, mean_wardur, mean_factnum, mean_factnum2, 
               mean_trnsfcap, mean_develop, mean_exp, mean_decade, mean_treaty, 1, 1*logcost)
  X_control <- c(mean_wartype, logcost, mean_wardur, mean_factnum, mean_factnum2, 
                 mean_trnsfcap, mean_develop, mean_exp, mean_decade, mean_treaty, 0, 0*logcost)
  storage_treated_it[logcost]  <- get_logit(X_treat, coef(glm2))
  storage_control_it[logcost]  <- get_logit(X_control, coef(glm2))
}

modified_y <- storage_treated_it - storage_control_it

# Plot
plot(1:16, original_y, type = "l", ylim = c(0, 0.8),lty=4, ylab = "Marginal effects of UN peacekeeping operations", xlab = "Logcost")
par(new=TRUE)
plot(1:16, modified_y, type = "l", ylim = c(0, 0.8), ylab = "Marginal effects of UN peacekeeping operations", xlab = "Logcost")
legend("topleft", legend= c("Original Model","Model with Interaction Term"), lty=c(4,1), cex=1) 
title(main="Causal Effect of Multidimensional UN Peacekeeping Operations") 

##Question 3

foo <- read.csv("https://course-resources.minerva.kgi.edu/uploaded_files/mke/00086677-3767/peace.csv")
Tr <- rep(0, length(foo$uncint))
Tr[which(foo$uncint!= "None" & foo$uncint != "1")] <- 1


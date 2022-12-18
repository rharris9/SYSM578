
#Final Exam 

library(car)
library(MASS)	 
library(lmtest)	
library(caret) # for cross-validation!!!
library(sandwich)
library(leaps)
#Question 1
# Load the readxl package
install.packages("readxl")
library(readxl)

# Read the data from the final_2022_data.xlsx file into a data frame
df <- read_excel("/Users/rharris/Desktop/final2022_data.xlsx", sheet = "T1")

# Load the caret package
install.packages("vctrs")
install.packages("data.table")

library(caret)

# Split the data into a training set and a test set
set.seed(123)
train_ind <- createDataPartition(df$Y, p = 0.8, list = FALSE)
train <- df[train_ind, ]
test <- df[-train_ind, ]

# Fit the LS Linear Regression model to the training data
model <- lm(Y ~ ., data = train)
# Make predictions on the test data using the fitted model
predictions <- predict(model, newdata = test)
# Evaluate the model using statistical measures
summary(model)


# Fit the LS Linear Regression model to the entire dataset
model <- lm(Y ~ ., data = df)
# Evaluate the model using statistical measures
summary(model)

# Evaluate the models using statistical measures
summaries <- lapply(models, summary)

#Brute force regression
reg1 <- lm(Y ~ X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X12+X13+X14+X15+X16+X17+X18+X19+X20+X21+X22+X23+X24+X25+X26+X27+X28+X29+X30+X31+X32+X33, data=df)
summary(reg1)
#Looking at VIF data
print(vif(reg), digits=2)

bptest(reg)	#test for heteroscedasticity

regLM <- lm(Y ~ X2+X5+X9+X14+X18+X22+X23+X25+X33, data=df)
summary(regLM)

#Looking at VIF data
print(vif(regLM), digits=2)

bptest(regLM)	#test for heteroscedasticity

# Let's do Cross-Validation (CV) with "caret" package - library(caret)
my_data <- data.frame(df)	# make sure your dataset looks like a "matrix"
data_ctrl <- trainControl(method = "cv", number = 3)	# set the number of folds
model_caret <- train(Y ~ X2+X5+X9+X14+X18+X22+X23+X25+X33, data=df, trControl=data_ctrl, method="lm", na.action = na.pass)              
model_caret	# provides the average stats for the test samples
model_caret$finalModel	# model fit on full sample
model_caret$resample	# key stats on folds
sd(model_caret$resample$Rsquared)	# std. dev. of R-squared across folds
sd(model_caret$resample$RMSE)	# std. dev. of RMSE across folds (output shown below)


#=================Question 2==============================
library(glmnet)


df <- read_excel("/Users/rharris/Desktop/final2022_data.xlsx", sheet = "T2")

#Fitting a Linear Regression model on the dataset
lm_model <- lm(Y ~ ., data = df)
summary(lm_model)

# Fit a Ridge Regression model on the data et
ridge_model <- glmnet(as.matrix(df[, -1]), df$Y, alpha = 0, lambda = 0.1)
summary(ridge_model)

# doing a lasso model on the data set
lasso_model <- glmnet(as.matrix(df[, -1]), df$Y, alpha = 1, lambda = 0.1)
summary(lasso_model)

# Define the predictor variables and the response variable
X <- df[, -1]
Y <- df$Y

# Fit a linear regression model used cross vailidation cross-validation
lin_model <- train(X, Y, method = "lm")

# Fit a ridge regression model using cross-validation
ridge_model <- train(X, Y, method = "glmnet", trControl = trainControl(method = "cv"), tuneGrid = expand.grid(alpha = 0, lambda = seq(0, 1, 0.1)))

# Fit a lasso model using cross-validation
lasso_model <- train(X, Y, method = "glmnet", trControl = trainControl(method = "cv"), tuneGrid = expand.grid(alpha = 1, lambda = seq(0, 1, 0.1)))

# Extract the MSE for each model
lin_mse <- mean(lin_model$delta)
lin_mse <- lin_model$results[, "MSE"]
ridge_mse <- ridge_model$results[, "MSE"]
lasso_mse <- lasso_model$results[, "MSE"]

# Print the MSE for each model
print(paste("Linear Regression MSE:", mean(lin_mse)))
print(paste("Ridge Regression MSE:", mean(ridge_mse)))
print(paste("Lasso MSE:", mean(lasso_mse)))




#Lasso Model
my_X <- as.matrix(sapply(df[,1:33], as.numeric))
my_Y <- as.matrix(sapply(Y, as.numeric))


my_alpha <- 1	
my_folds <- 3


cv_output <- cv.glmnet(x=my_X, y=my_Y, alpha = my_alpha, type.measure="mse", nfolds=my_folds)
plot(cv_output)	# MSE plot
best_lambda <- cv_output$lambda.min
print(best_lambda)

lasso_best <- glmnet(x=my_X, y=my_Y, alpha=my_alpha, lambda=best_lambda)
lasso_coef <- coef(lasso_best)	# lasso coefficients
print(lasso_coef)
lasso_pred <- predict(lasso_best, s=best_lambda, newx=my_X)	


rss <- sum((lasso_pred - my_Y) ^ 2)	# residual sum of squares
tss <- sum((my_Y - mean(my_Y)) ^ 2)	# total sum of squares
rsq <- 1 - rss/tss
print(rsq)

#===========================Question 3===============================
install.packages("flexsurv")
install.packages("rms")
library(survival)
library(KMsurv)
library(flexsurv)
library(rms)
data <- read_excel("/Users/rharris/Desktop/final2022_data.xlsx", sheet = "T3")
data



#create an empirical CDF and S(t) assuming no censoring !!
my_cdf <- ecdf(data$lived)
len <- length(knots(my_cdf)) #CDF at how many points?
St <- 1 - my_cdf(1:len)	#create S(t)
plot(St, xlab="survival time", ylab="probability", main="Empirical S(t)")

#test the difference between the two treatments
survdiff(Surv(lived, status)~drug, data=data)

#fit parametric models
reg <- survreg( Surv(lived, status) ~ drug+stage+lived+status+rating+diagnosed+age+therapy, 
                data=data, dist="loglogistic")
summary(reg)



T_hat <- predict(reg, data=data, type="response", se=TRUE)
error <- residuals(reg, type="response", rsigma=TRUE) 


#using flexsurv - this is easy for generating/plotting S(t)
reg <- flexsurvreg( Surv(lived, status) ~ drug+stage+lived+status+rating+diagnosed+age+therapy, 
                    data=data, dist="lognormal")
reg
summary(reg)
plot(reg, xlab="t", ylab="S(t)", main="S(t), KM & CIs")

#Cox Proportional Hazards (PH) model
reg <- coxph(Surv(lived, status) ~ drug, 
                  data=data )
summary(reg)
Ht <- predict(reg, type="expected") # gives the cum hazard H(t)= expected number of events at t
St <- exp(-Ht) # predicted S(t) at each observation
St
# plot the predicted survival probabilities for the data sample
plot(data$lived[order(data$lived)], St[order(-St)], xlab="survival time in data sample", ylab="Cox S(t)")

# check constant PH assumption by seeing if the smoothed line has 0 slope 
ph <- cox.zph(reg) 
ph
plot(ph[1]) # plot scaled Schoenfeld residuals - use ph[1] etc. for each X
plot(survfit(reg, censor=TRUE)) # plot cox survival curve at the means of the Xs

# variable selection with coxph using MASS
library(MASS)
stepAIC(reg, direction="both")
#stepAIC(reg, direction="backward")


#Buckley-James Regression
reg_bj <- bj(Surv(lived, status) ~ drug, 
             link="log", data=data, x=TRUE, y=TRUE)
reg_bj
predict(reg_bj)	# predicted values

#=================Question 4===============================

data <- read_excel("/Users/rharris/Desktop/final2022_data.xlsx", sheet = "T4")


# brute force logit
reg <- glm(Y ~ X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X12+X13+X14+X15+X16+X17+X18+X19+X20
           +X21+X22+X23+X24+X25+X26+X27+X28+X29+X30+X31+X32+X33, family=binomial, data=data)
Yhat0 <- predict(reg, data=data, type="response")

step(reg, trace=0) # backward selection

# Let's re-run Xs selected by "backward selection"
reg_backward <- glm(Y ~ X1 + X2 + X5 + X8 + X9 + X11 + X12 + X13 + X14 + X17 + X18 + X19 + X20 
                    + X23 + X25 + X26 + X29 + X32, family = binomial, data = data)
summary(reg_backward) # several Xs are not significant
Yhat_backward <- predict(reg_backward, data=data, type="response")


# Let's see what forward selection picks
nothing <- glm(Y ~ 1, family=binomial, data=data)
step(nothing, scope=list(lower=formula(nothing),upper=formula(reg)), direction="forward", trace=0)

# Let's see how forward selected Xs look
reg_forward <- glm(Y ~ X22 + X32 + X19, family=binomial, data=data)
summary(reg_forward)	# X19 sig at 10%, but let's keep it
Yhat_forward <- predict(reg_forward, data=data, type="response")


# Let's see what "both" selection picks
nothing <- glm(Y ~ 1, family=binomial, data=data)
step(nothing, scope=list(lower=formula(nothing),upper=formula(reg)), direction="both", trace=0)


# get KS by comparing distribution of predictions between Y=0 and Y=1
# START defining your inputs
my_dataset <- data		# which dataset are you using?
my_Y <- my_dataset$Y				# what is your Y variable?
my_yhat <- Yhat_backward			# what are your predictions?
# END defining your inputs

# START KS code - NO need to change this block of KS code!!!
my_yhat1 <- pmax( my_yhat,  rep(0,length(my_yhat)) )	# set predictions < 0 to 0
my_yhat2 <- pmin( my_yhat1, rep(1,length(my_yhat)) )	# set predictions > 1 to 1
yhat_when_Y_is_0 <- subset(cbind(my_yhat2, my_Y), my_Y==0) #find predictions for Y=0
yhat_when_Y_is_1 <- subset(cbind(my_yhat2, my_Y), my_Y==1) #find predictions for Y=1
yhat0 <- as.matrix(sapply(yhat_when_Y_is_0[,1], as.numeric))
yhat1 <- as.matrix(sapply(yhat_when_Y_is_1[,1], as.numeric))
seq = seq(0, 1, by=0.01)	# use to output CDF values from ecdf
cdf0 <- ecdf(yhat0)	# get the empirical CDF for yhat0
cdf1 <- ecdf(yhat1)	# get the empirical CDF for yhat1
ks.test(cdf0(seq),cdf1(seq))
# END KS code


predicted_probability_1 <- predict(reg_backward, newdata = data[1, ])
print(predicted_probability_1)

predicted_probability_37 <- predict(reg_backward, newdata = data[37, ])
print(predicted_probability_37)
#==================Question 5==============================
#=Method1=
data <- read_excel("/Users/rharris/Desktop/final2022_data.xlsx", sheet = "T4")
data
#running a least squares regression
reg <- lm(Y ~ ., data = data)
summary(model)

# this block of code is one way to get Hal White's t and p values
White_t <- coef(summary(reg))[ ,1]/sqrt(diag(vcovHC(reg)))	
error_df <- nrow(data) - length(White_t)
# Get the note the two-tailed p value by multiplying by 2 !!!
White_p <- ( 1 - pt( abs(White_t) , error_df) )*2	# 2-tailed White p-value
print(White_t, digits=2) # White t-values rounded to 2 digits
print(White_p, digits=2) # White p-values rounded to 2 digits
subset(White_p, White_p <= 0.20)	# significant variables and their White p values
subset(White_p, White_p > 0.20)	# NON-significant variables and their White p values

# Ridge Regression w/ selected variables

X <- data[,c(2,13,18,25,29)] # Xs finally selected by RR

Y <- data[,34]

lambda <- 0.03588769


alpha <- 0.15
# code for RR
tiny <- 10^(-9)	#use for a slight perturbation if needed when doing ordinary least squares (LS)

p <- ncol(X)
n <- nrow(X)
I <- diag(p)

X.corr <- scale(X, center=TRUE, scale=TRUE)/sqrt(n - 1)	# standardize X
Y.centered <- scale(Y, center=TRUE, scale=FALSE)	# center Y


Z.LS <- solve(t(X.corr)%*%X.corr + tiny*I)	# do LS estimation
b.LS <- Z.LS %*% t(X.corr) %*% Y.centered	# get LS coefficients
LS.vif <- diag( Z.LS %*% (t(X.corr)%*%X.corr) %*% Z.LS  )	# VIF in LS
error.df <- n-1-p	# LS error degrees of freedom (df)
critical.t <- qt(alpha,error.df)	# critical t-value
MSE.LS <- ( t(Y.centered - X.corr %*% b.LS) %*% (Y.centered - X.corr %*% b.LS) )/error.df	# get LS MSE
regSS  <- t(b.LS)%*%(t(X.corr)%*%X.corr) %*% b.LS	# LS regression sum of squares (SS) 
regMSE <- regSS/p;	# LS regression MSE	
LS.F <- regMSE/MSE.LS	# LS F-value
b.LS.SE <- sqrt( diag(Z.LS %*% t(X.corr) %*% X.corr %*% Z.LS) * drop(MSE.LS) )
b.LS.t.values <- b.LS/b.LS.SE
LS.significant <- (abs(b.LS.t.values) >= abs(critical.t))	# flagging, using "1", significant LS coefficients 
HKB <- p*MSE.LS/(t(b.LS)%*%b.LS)	# Hoerl-Kennard-Baldwin lambda
LW <- 1/LS.F	# Lawless and Wang lambda

Z <- solve(t(X.corr)%*%X.corr + lambda*I)	# do RR estimation with your chosen lambda
b.RR <- Z %*% t(X.corr) %*% Y.centered	# get RR standardized coefficients

b.RR.SE <- sqrt( diag(Z %*% t(X.corr) %*% X.corr %*% Z) * drop(MSE.LS) )
b.RR.t.values <- b.RR/b.RR.SE

RR.significant <- (abs(b.RR.t.values) >= abs(critical.t))	# flagging using "1" significant RR coefficients 
Y.RR <- mean(Y) + X.corr%*%b.RR
RR.vif <- diag( Z %*% (t(X.corr)%*%X.corr) %*% Z  )	# VIF in RR
MSE.RR <- (t(Y-Y.RR)%*%(Y-Y.RR))/error.df	# approximate MSE for RR

cbind(sqrt(MSE.LS), LS.F, sqrt(MSE.RR), HKB, LW)
cbind(b.RR, b.RR.t.values, LS.vif, LS.significant, RR.vif, RR.significant)

# unstandardize LS coefficients
b.LS.raw <- b.LS*( 1/sqrt( diag(var( as.matrix(sapply(X, as.numeric)) ) )*(n-1)) )
intercept.LS <- mean(Y)- sum(b.LS.raw*colMeans(as.matrix(sapply(X, as.numeric))))
print(cbind(intercept.LS, t(b.LS.raw)))	# LS coefficients in original units

# unstandardize RR coefficients
b.RR.raw <- b.RR*( 1/sqrt( diag(var( as.matrix(sapply(X, as.numeric)) ) )*(n-1)) )
intercept.RR <- mean(Y)- sum(b.RR.raw*colMeans(as.matrix(sapply(X, as.numeric))))
print(cbind(intercept.RR, t(b.RR.raw)))	# RR coefficients in original units

# Once you have selected the Xs using RR, get the weights and do a WLS LPM
# START entering your INPUTS:
my_dataset <- data	# which dataset are you using?
my_X <- my_dataset[,c(2,13,18,25,29)]	# what are the columns of your selected Xs?
my_Y <- my_dataset[,34]		# what's the column of your Y?
k_lpm <- 1.4	# by trial and error pick your lambdas for RR
# END your inputs



# START RR LPM code, which you DO NOT have to modify at all !!! 
X.mat <- cbind(scale(my_X, center=TRUE, scale=TRUE)/sqrt(nrow(my_dataset) - 1), my_Y ) # Put X in correlation form
X.mat.raw <- as.matrix(cbind( my_X, my_Y ))	# Keep X as-is
number_of_Xs_chosen <- ncol(my_X)			
column_number_of_Y <- number_of_Xs_chosen + 1

# make X0 a matrix with a column of ones to account for intercept and reproduce 
# the coefficients given by "reg" above
intercept <- rep(1,nrow(X.mat.raw))
X.mat1 <- as.matrix(cbind( intercept, X.mat.raw[,1:number_of_Xs_chosen]))
Z.LS <- solve( t(X.mat1) %*% X.mat1 )	# inv(X'X)
beta.LS <- Z.LS %*% t(X.mat1) %*% X.mat.raw[,column_number_of_Y]	#same coefficients as those from "reg" above
# calculate mean squared error (MSE) for LS, to be used in RR as well
SSE.LS <- (t( (X.mat1 %*% beta.LS - X.mat.raw[,column_number_of_Y]) ) %*% 
             (X.mat1 %*% beta.LS - X.mat.raw[,column_number_of_Y]))/(nrow(X.mat1) - 1 - number_of_Xs_chosen)
beta.LS.SE <- sqrt(diag(Z.LS)*drop(SSE.LS))	# variance of coefficients
beta.LS.t.values <- beta.LS/beta.LS.SE		# t-values of coefficients
cbind(beta.LS, beta.LS.t.values)	# gives you ordinary regression coefficients and t-values	

Z <- solve(t(X.mat[,1:number_of_Xs_chosen])%*%X.mat[,1:number_of_Xs_chosen] + k_lpm*diag(ncol(X.mat)-1))	# inv(X'X+kI)
beta.RR <- Z%*%t(X.mat[,1:number_of_Xs_chosen])%*%X.mat[,column_number_of_Y]	# ZX'Y, standardized coefficients
beta.SE.RR <- sqrt( (diag((Z%*%t(X.mat[,1:number_of_Xs_chosen])%*%X.mat[,1:number_of_Xs_chosen]%*%Z)))*drop(SSE.LS) )
beta.RR.t.values <- beta.RR/beta.SE.RR
pred.RR <- mean(X.mat[,column_number_of_Y]) + X.mat[,1:number_of_Xs_chosen]%*%beta.RR
bad_prob.RR <- subset(pred.RR, pred.RR > 1 | pred.RR < 0) 
length(bad_prob.RR)
cbind(beta.RR, beta.RR.t.values)	# gives you RR coefficients and t-values
print(k_lpm)
# END RR LPM code
X

# MANUALLY do WLS LPM estimation using pred.RR to construct your weights!!!
W <- 1/(pred.RR*(1-pred.RR))
reg_wls <- lm(D ~ Z2+Z13+Z18+Z25, data=my_dataset, weights=W)
summary(reg_wls)
yhat_wls <- predict(reg_wls)


# get KS by comparing distribution of predictions between Y=0 and Y=1
# START defining your inputs
my_dataset <- data_clean	# which dataset are you using?
my_Y <- my_dataset$D		# what is your Y variable?
my_yhat <- yhat_wls			# what are your predictions?
# END defining your inputs

# START KS code - NO need to change this block of KS code!!!
my_yhat1 <- pmax( my_yhat,  rep(0,length(my_yhat)) )	# set predictions < 0 to 0
my_yhat2 <- pmin( my_yhat1, rep(1,length(my_yhat)) )	# set predictions > 1 to 1
yhat_when_Y_is_0 <- subset(cbind(my_yhat2, my_Y), my_Y==0) #find predictions for Y=0
yhat_when_Y_is_1 <- subset(cbind(my_yhat2, my_Y), my_Y==1) #find predictions for Y=1
yhat0 <- as.matrix(sapply(yhat_when_Y_is_0[,1], as.numeric))
yhat1 <- as.matrix(sapply(yhat_when_Y_is_1[,1], as.numeric))
seq = seq(0, 1, by=0.01)	# use to output CDF values from ecdf
cdf0 <- ecdf(yhat0)	# get the empirical CDF for yhat0
cdf1 <- ecdf(yhat1)	# get the empirical CDF for yhat1
ks.test(cdf0(seq),cdf1(seq))
# END KS code


#==========Method 2=============
data <- read_excel("/Users/rharris/Desktop/final2022_data.xlsx", sheet = "T4")
data
#running a least squares regression
reg <- lm(Y ~ ., data = data)
summary(reg)


library(MASS)
stepAIC(reg, direction="both")
#stepAIC(reg, direction="backward")

model <- step(reg, scope = reg, direction = "both", k = 1, trace = 0, criterion = "Cp")
print(model)

# Ridge Regression w/ selected variables

X <- data[,c(1,2,5,6,8,12)] # Xs finally selected by RR

Y <- data[,34]

lambda <- 0.03588769


alpha <- 0.15
# code for RR
tiny <- 10^(-9)	#use for a slight perturbation if needed when doing ordinary least squares (LS)

p <- ncol(X)
n <- nrow(X)
I <- diag(p)

X.corr <- scale(X, center=TRUE, scale=TRUE)/sqrt(n - 1)	# standardize X
Y.centered <- scale(Y, center=TRUE, scale=FALSE)	# center Y


Z.LS <- solve(t(X.corr)%*%X.corr + tiny*I)	# do LS estimation
b.LS <- Z.LS %*% t(X.corr) %*% Y.centered	# get LS coefficients
LS.vif <- diag( Z.LS %*% (t(X.corr)%*%X.corr) %*% Z.LS  )	# VIF in LS
error.df <- n-1-p	# LS error degrees of freedom (df)
critical.t <- qt(alpha,error.df)	# critical t-value
MSE.LS <- ( t(Y.centered - X.corr %*% b.LS) %*% (Y.centered - X.corr %*% b.LS) )/error.df	# get LS MSE
regSS  <- t(b.LS)%*%(t(X.corr)%*%X.corr) %*% b.LS	# LS regression sum of squares (SS) 
regMSE <- regSS/p;	# LS regression MSE	
LS.F <- regMSE/MSE.LS	# LS F-value
b.LS.SE <- sqrt( diag(Z.LS %*% t(X.corr) %*% X.corr %*% Z.LS) * drop(MSE.LS) )
b.LS.t.values <- b.LS/b.LS.SE
LS.significant <- (abs(b.LS.t.values) >= abs(critical.t))	# flagging, using "1", significant LS coefficients 
HKB <- p*MSE.LS/(t(b.LS)%*%b.LS)	# Hoerl-Kennard-Baldwin lambda
LW <- 1/LS.F	# Lawless and Wang lambda

Z <- solve(t(X.corr)%*%X.corr + lambda*I)	# do RR estimation with your chosen lambda
b.RR <- Z %*% t(X.corr) %*% Y.centered	# get RR standardized coefficients

b.RR.SE <- sqrt( diag(Z %*% t(X.corr) %*% X.corr %*% Z) * drop(MSE.LS) )
b.RR.t.values <- b.RR/b.RR.SE

RR.significant <- (abs(b.RR.t.values) >= abs(critical.t))	# flagging using "1" significant RR coefficients 
Y.RR <- mean(Y) + X.corr%*%b.RR
RR.vif <- diag( Z %*% (t(X.corr)%*%X.corr) %*% Z  )	# VIF in RR
MSE.RR <- (t(Y-Y.RR)%*%(Y-Y.RR))/error.df	# approximate MSE for RR

cbind(sqrt(MSE.LS), LS.F, sqrt(MSE.RR), HKB, LW)
cbind(b.RR, b.RR.t.values, LS.vif, LS.significant, RR.vif, RR.significant)

# unstandardize LS coefficients
b.LS.raw <- b.LS*( 1/sqrt( diag(var( as.matrix(sapply(X, as.numeric)) ) )*(n-1)) )
intercept.LS <- mean(Y)- sum(b.LS.raw*colMeans(as.matrix(sapply(X, as.numeric))))
print(cbind(intercept.LS, t(b.LS.raw)))	# LS coefficients in original units

# unstandardize RR coefficients
b.RR.raw <- b.RR*( 1/sqrt( diag(var( as.matrix(sapply(X, as.numeric)) ) )*(n-1)) )
intercept.RR <- mean(Y)- sum(b.RR.raw*colMeans(as.matrix(sapply(X, as.numeric))))
print(cbind(intercept.RR, t(b.RR.raw)))	# RR coefficients in original units


# MANUALLY do WLS LPM estimation using pred.RR to construct your weights!!!
W <- 1/(pred.RR*(1-pred.RR))
reg_wls <- lm(D ~ Z1+Z2+Z5+Z6+Z8+Z12, data=data_clean, weights=W)
summary(reg_wls)
yhat_wls <- predict(reg_wls)


# get KS by comparing distribution of predictions between Y=0 and Y=1
# START defining your inputs
my_dataset <- data_clean	# which dataset are you using?
my_Y <- my_dataset$D		# what is your Y variable?
my_yhat <- yhat_wls			# what are your predictions?
# END defining your inputs

# START KS code - NO need to change this block of KS code!!!
my_yhat1 <- pmax( my_yhat,  rep(0,length(my_yhat)) )	# set predictions < 0 to 0
my_yhat2 <- pmin( my_yhat1, rep(1,length(my_yhat)) )	# set predictions > 1 to 1
yhat_when_Y_is_0 <- subset(cbind(my_yhat2, my_Y), my_Y==0) #find predictions for Y=0
yhat_when_Y_is_1 <- subset(cbind(my_yhat2, my_Y), my_Y==1) #find predictions for Y=1
yhat0 <- as.matrix(sapply(yhat_when_Y_is_0[,1], as.numeric))
yhat1 <- as.matrix(sapply(yhat_when_Y_is_1[,1], as.numeric))
seq = seq(0, 1, by=0.01)	# use to output CDF values from ecdf
cdf0 <- ecdf(yhat0)	# get the empirical CDF for yhat0
cdf1 <- ecdf(yhat1)	# get the empirical CDF for yhat1
ks.test(cdf0(seq),cdf1(seq))
# END KS code

#==========Method 3===============


#===============Question 6=======================

data <- read_excel("/Users/rharris/Desktop/final2022_data.xlsx", sheet = "T5")

#----Trying Linear Regression===
model <- lm(infections ~ age_group + swimmer + swam_in, data = data)
summary(model)

# Printing the R-squared and adjusted R-squared values
r_squared <- summary(model)$r.squared
adj_r_squared <- summary(model)$adj.r.squared
print(paste("R-squared =", r_squared))
print(paste("Adjusted R-squared =", adj_r_squared))

# Performing stepwise selection with AIC 
stepwise_model <- step(model, direction = "both", k = 2, trace = 0)
summary(stepwise_model)

print(vif(model), digits=2)	# looks like VIFs are good! all less than 10! 

# looks like these 2 look good too! p-values are comfortably larger than 5%
bptest(model)	#test for heteroscedasticity
#------ Trying Lasso Regression
install.packages("glmnet")
library(glmnet)


data <- read_excel("/Users/rharris/Desktop/final2022_data.xlsx", sheet = "T5")


X <- as.matrix(data[, -1])
Y <- as.matrix(data[, 1])
lasso_model <- glmnet(as.matrix(df[, -1]), df$Y, alpha = 1, lambda = 0.1)
summary(lasso_model)


my_X <- as.matrix(sapply(data[,1:3], as.numeric))
my_Y <- as.matrix(sapply(data[,4], as.numeric))


my_alpha <- 1	
my_folds <- 3


cv_output <- cv.glmnet(x=my_X, y=my_Y, alpha = my_alpha, type.measure="mse", nfolds=my_folds)
plot(cv_output)	# MSE plot
best_lambda <- cv_output$lambda.min
print(best_lambda)

lasso_best <- glmnet(x=my_X, y=my_Y, alpha=my_alpha, lambda=best_lambda)
lasso_coef <- coef(lasso_best)	# lasso coefficients
print(lasso_coef)
lasso_pred <- predict(lasso_best, s=best_lambda, newx=my_X)	


rss <- sum((lasso_pred - my_Y) ^ 2)	# residual sum of squares
tss <- sum((my_Y - mean(my_Y)) ^ 2)	# total sum of squares
rsq <- 1 - rss/tss
print(rsq)
#Attempting Logistical regression
data <- read_excel("/Users/rharris/Desktop/final2022_data.xlsx", sheet = "T5")
demo <- infections ~ age_group + swimmer + swam_in
model <- glm(infections ~ age_group + swimmer + swam_in, data = data, family = binomial(link = "logit"))
# Attempting Ridge Regression
data <- read_excel("/Users/rharris/Desktop/final2022_data.xlsx", sheet = "T5")

X <- data[, -4]
Y <- data$infections
X
# Fit a Ridge Regression model on the data et
ridge_model <- glmnet(as.matrix(X), Y, alpha = 0, lambda = 0.1)
summary(ridge_model)

#using elastic net regression
my_folds <- 4	# how many folds do you want for cross-validation? should be >= 3



# ************************* NO changes needed below *********************************************************************

my_ENET = train(
  Y ~ ., 
  data=my_data,
  method = "glmnet",
  trControl = trainControl(method = "cv", number = my_folds),
  tunelength = 10
)

my_ENET

# choosing alpha and lambda "judgmentally" by picking alpha such that the RMSEs by alpha have the smallest
# standard deviation; and then picking the lambda corresponding to the alpha in that group with the smallest RMSE
min_rmse_sd <- tapply(my_ENET$results[,3], my_ENET$results[,1], sd)
idx <- which(min_rmse_sd == min(min_rmse_sd))
alpha_best <- as.numeric(noquote(names(min_rmse_sd[idx])))

lambda_best0 <- my_ENET$results[my_ENET$results[,1]==alpha_best, 1:3]
min_RMSE_for_alpha <- min(lambda_best0[,3])
idx0 <- which(lambda_best0[,3] == min(lambda_best0[,3]))
lambda_best <- lambda_best0[idx0,2]

enet_model <- glmnet(x=X, y=Y, alpha=alpha_best, lambda=lambda_best)
betas <- coef(enet_model)      # judgmentally selected model
num_betas <- sum(betas !=0)    # number of Xs selected judgmentally


# choosing a model by picking the alpha-lambda combo that yields the lowest RMSE
best = which(rownames(my_ENET$results) == rownames(my_ENET$bestTune))
best_result = my_ENET$results[best, ]   # stats for R selected model
rownames(best_result) = NULL
best_result
enet_model1 <- glmnet(x=X, y=Y, alpha=best_result[1,1], lambda=best_result[1,2])
betas1 <- coef(enet_model1) 
print(sum(betas1 !=0))   # R picked number of Xs
# judgmentally picked alpha, lambda, number of Xs & RMSE
print(cbind(alpha_best, lambda_best, num_betas, min_RMSE_for_alpha))   
cbind(betas, betas1)  # Coefficients of judgmentally and R picked models




#==============Question 7=============================
data <- read_excel("/Users/rharris/Desktop/final2022_data.xlsx", sheet = "T6")

#linear regression
fit_lm <- lm(Y ~ ., data = df)
mse <- mean((predictions - data$Y)^2)
summary(fit_lm)

#logistical regression
fit_glm <- glm(Y ~ ., data = data, family = binomial)

#Lasso Regression
cols <- ncol(data)
rows <- nrow(data)

# define your X and Y variables as matrices

my_X <- as.matrix(sapply(data[,2:89], as.numeric))
my_Y <- as.matrix(sapply(Y, as.numeric))

my_alpha <- 1	# set your alpha value. For Lasso, alpha is ALWAYS 1
my_folds <- 3

cv_output <- cv.glmnet(x=my_X, y=my_Y, alpha = my_alpha, type.measure="mse", nfolds=my_folds)
plot(cv_output)	# MSE plot
best_lambda <- cv_output$lambda.min
print(best_lambda)

lasso_best <- glmnet(x=my_X, y=my_Y, alpha=my_alpha, lambda=best_lambda)
lasso_coef <- coef(lasso_best)	# lasso coefficients
print(lasso_coef)
lasso_pred <- predict(lasso_best, s=best_lambda, newx=my_X)	# lasso predictions for lasso_best

# calculate R-squared
rss <- sum((lasso_pred - my_Y) ^ 2)	# residual sum of squares
tss <- sum((my_Y - mean(my_Y)) ^ 2)	# total sum of squares
rsq <- 1 - rss/tss
print(rsq)

#Least Squares Regression
data <- read_excel("/Users/rharris/Desktop/final2022_data.xlsx", sheet = "T6")

reg <- lm(Y ~ ., data = data)
summary(reg)
# Calculating the AIC and BIC 
aic_full <- AIC(reg)
bic_full <- BIC(reg)

# Calculate the adjusted R-squared for the full model
adj_r2_full <- summary(reg)$adj.r.squared

aic_full
bic_full 
# finding the optimal subset of features
fit_subsets <- regsubsets(y ~ ., data = data, nbest = 1)
#Had some problems with completing the code to get the full features selected for this model

#====================Question 8====================================
library(stats)

data <- read_excel("/Users/rharris/Desktop/final2022_data.xlsx", sheet = "T7")

rac <- glm(STA ~ RACE, data = data, family = "binomial")
summary(rac)

subset <- regsubsets(STA ~ ., data = data, nbest = 1)
cp <- Cp(rac)
adj_r2 <- rsquaredAdj(rac)
aic_full <- AIC(rac)
aic_full
bic_full <- BIC(rac)
bic_full
data_clean=data


# defining gender:
data_clean$male <- ifelse(data_clean$GENDER=="Male", 1, 0)
# defining race:
data_clean$white <- ifelse(data_clean$RACE==1, 1, 0)
data_clean$black <- ifelse(data_clean$RACE==2, 1, 0)

# defining SER: 		
data_clean$ser1 <- ifelse(data_clean$SER=="Medical", 1, 0)
# defining CAN:
data_clean$can1 <- ifelse(data_clean$CAN=="Yes", 1, 0)
# defining CRN:
data_clean$crn1 <- ifelse(data_clean$CRN=="Yes", 1, 0)
# defining INF:
data_clean$inf1 <- ifelse(data_clean$INF=="Yes", 1, 0)
# defining CPR:
data_clean$cpr1 <- ifelse(data_clean$CPR=="Yes", 1, 0)
# defining PRE:
data_clean$pre1 <- ifelse(data_clean$PRE=="Yes", 1, 0)
# defining TYP:
data_clean$typ1 <- ifelse(data_clean$TYP=="Elective", 1, 0)
# defining FRA:
data_clean$fra1 <- ifelse(data_clean$FRA=="Yes", 1, 0)
# defining LOC:
data_clean$loc1 <- ifelse(data_clean$LOC==1, 1, 0)
data_clean$loc2 <- ifelse(data_clean$LOC==2, 1, 0)
reg <- glm(STA ~ AGE + male + white + black + ser1 + can1 + crn1 + inf1 + cpr1 + SYS + HRA + pre1 + typ1
           + fra1 + PO2 + PH + PCO + BIC + CRE + loc1 + loc2, family=binomial, data=data_clean)
summary(reg)


# Run "backward" selection. 
step(reg, trace=0) # backward selection
# Then run with "backward" selected Xs
reg_backward <- glm(STA ~ AGE + black + can1 + SYS + pre1 + typ1 + PH + PCO + loc1 + loc2, 
                    family=binomial, data=data_clean)
summary(reg_backward)
# store the predictions made by the "backward" selected model
yhat_backward <- predict(reg_backward, data=data_clean, type="response")


# Run forward selection. Then run with "forward" selected Xs
nothing <- glm(STA ~ 1, family=binomial, data=data_clean)
step(nothing, scope=list(lower=formula(nothing),upper=formula(reg)), direction="forward", trace=0)
reg_forward <- glm(STA ~ loc2 + loc1 + typ1 + AGE + can1 + SYS + black + pre1, family=binomial, data=data_clean)
summary(reg_forward)
yhat_forward <- predict(reg_forward, data=data_clean, type="response")

# Let's do "both" ways selection
nothing <- glm(STA ~ 1, family=binomial, data=data_clean)
step(nothing, scope=list(lower=formula(nothing),upper=formula(reg)), direction="both", trace=0)
# didn't run with "both" selected Xs, because they are identical to the Xs selected by "backward" selection

# get KS by comparing distribution of predictions between Y=0 and Y=1
# START defining your inputs
my_dataset <- data_clean		# which dataset are you using?
my_Y <- my_dataset$STA			# what is your Y variable?
my_yhat <- yhat_forward		# what are your predictions?
# END defining your inputs

# START KS code - NO need to change this block of KS code!!!
my_yhat1 <- pmax( my_yhat,  rep(0,length(my_yhat)) )	# set predictions < 0 to 0
my_yhat2 <- pmin( my_yhat1, rep(1,length(my_yhat)) )	# set predictions > 1 to 1
yhat_when_Y_is_0 <- subset(cbind(my_yhat2, my_Y), my_Y==0) #find predictions for Y=0
yhat_when_Y_is_1 <- subset(cbind(my_yhat2, my_Y), my_Y==1) #find predictions for Y=1
yhat0 <- as.matrix(sapply(yhat_when_Y_is_0[,1], as.numeric))
yhat1 <- as.matrix(sapply(yhat_when_Y_is_1[,1], as.numeric))
seq = seq(0, 1, by=0.01)	# use to output CDF values from ecdf
cdf0 <- ecdf(yhat0)	# get the empirical CDF for yhat0
cdf1 <- ecdf(yhat1)	# get the empirical CDF for yhat1
ks.test(cdf0(seq),cdf1(seq))
# END KS code



#=================Question 9===============
data <- read_excel("/Users/rharris/Desktop/final2022_data.xlsx", sheet = "T8")

# Ridge Regression Model

x <- data[, 6, 1]
x
y <- data[, 7]

y <- data$Loss
X <- data[, c("borrowers", "county", "density", "rating", "age", "defaults")]

# scaling the data
X <- scale(X)

# Fit the ridge regression model
model <- glmnet(X, y, alpha = 0, lambda = 0.1)
summary(model)
# making prediction
predictions <- predict(model, newx = X)
predictions


# Calculating the mean squared error
mse <- mean((predictions - y)^2)

# Print the mean squared error
print(mse)

#checking data
cp <- Cp(model)
adj_r2 <- rsquare(model)
aic_full <- AIC(model)
aic_full
bic_full <- BIC(model)
bic_full

#=============================Question 10===================
data <- read_excel("/Users/rharris/Desktop/final2022_data.xlsx", sheet = "T9")

library(MASS)
fit <- fitdistr(X, "normal")
fit
summary(fit)
estimate <- qnorm(0.95, mean=fit$estimate[1], sd=fit$estimate[2])
estimate

#trying for poisson
fitpoi <- fitdistr(X, "poisson")
fitpoi
summary(fitpoi)

#saving the mean and SD from the normal set
mean <- fit$estimate[1]
sd1 <- fit$estimate[2]
mean
# calculating the 95th percentile of the PD
quantile <- qnorm(0.95, mean, sd1)

# Print the 95th percentile
print(quantile)

#Printing Empirical 95th percentile of data
Empirical <- quantile(X, 0.95)
print(Empirical)

#finding Standard Deviation of Median 
standard_deviation <- sd(X)
print(standard_deviation)

# Calculate the interquartile range (IQR) of the data to calculate the standard deviation of the median
IQR <- quantile(X, 0.75) - quantile(X, 0.25)
n <- length(X)
sd_median <- (1.253 * IQR)/sqrt(n)

# Printing
print(sd_median)

#Using KS Test to test
# get KS by comparing distribution of predictions between Y=0 and Y=1
# START defining your inputs
my_dataset <- data	# which dataset are you using?
my_Y <- data$D		# what is your Y variable?
my_yhat <- yhat_wls			# what are your predictions?
# END defining your inputs

# START KS code - NO need to change this block of KS code!!!
my_yhat1 <- pmax( my_yhat,  rep(0,length(my_yhat)) )	# set predictions < 0 to 0
my_yhat2 <- pmin( my_yhat1, rep(1,length(my_yhat)) )	# set predictions > 1 to 1
yhat_when_Y_is_0 <- subset(cbind(my_yhat2, my_Y), my_Y==0) #find predictions for Y=0
yhat_when_Y_is_1 <- subset(cbind(my_yhat2, my_Y), my_Y==1) #find predictions for Y=1
yhat0 <- as.matrix(sapply(yhat_when_Y_is_0[,1], as.numeric))
yhat1 <- as.matrix(sapply(yhat_when_Y_is_1[,1], as.numeric))
seq = seq(0, 1, by=0.01)	# use to output CDF values from ecdf
cdf0 <- ecdf(yhat0)	# get the empirical CDF for yhat0
cdf1 <- ecdf(yhat1)	# get the empirical CDF for yhat1
ks.test(cdf0(seq),cdf1(seq))
# END KS code


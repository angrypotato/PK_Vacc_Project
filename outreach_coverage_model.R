# Purpose: 

# 1. Find Which Covariates Are the Most Significant in Predicting 
#  Pentavalent Vacc Coverage via outreach program
# 2.  Develop Models Using Such Covariates in Order to Predict this outreach 
# Pentavalent Vaccination Rate - Feature selection using RFE, Boruta.  Predictive Modeling using GBM, Lasso and GAM.  


# prep ----

source(file='PreRunNew.r')
library(genridge)
library(Boruta)
library(mgcv)
library(glmnet)

std_mean <- function(x) sd(x)/sqrt(length(x))

test_scale <- function(raw_test, train.mean, train.sd) {
  df <- raw_test
  for (n in 1:ncol(df)) {
    df[,n] <- (raw_test[,n] - train.mean[n])/train.sd[n]
  }
  df
}



# Tehsil ----

### Take the existing Tehsil level data with covariates and Vaccination outreachs and parse out the 
### covariates from the Y (Outreach Vaccination Coverage)

# tehsils <- read.csv("results/tehsils_complete_8.15.csv")
tehsils.outreach <-  tehsils[,c(4:18,20,22,25,30,27)]

tehsils.outreach <- tehsils.outreach[complete.cases(tehsils.outreach),]

### Split Tehsil data into train and test set

set.seed(43)
data_split = sample.split(tehsils.outreach, SplitRatio = 0.8)

pentaTrain.raw <- subset(tehsils.outreach, data_split == TRUE) 
train.sd <- apply(pentaTrain.raw, 2, sd)
train.mean <- apply(pentaTrain.raw, 2, mean)

pentaTrain <- scale(pentaTrain.raw) %>%
  as.data.frame()

pentaTest.raw <-subset(tehsils.outreach, data_split == FALSE) 

pentaTest <- test_scale(pentaTest.raw,train.mean,train.sd)



## Feature Selection ----

### RFE Feature Selection ----
### Use Recursive Feature Elimination for Selection of Signficant Features

rfcontrol <- rfeControl(functions=rfFuncs, method="repeatedcv", number=10,repeats=3)
results <- rfe(pentaTrain[,1:19], pentaTrain[,20],sizes=c(1:19), rfeControl=rfcontrol)

# summarize the results

print(results)
predictors(results)


### Boruta ----
# Use Boruta Selection as another metric to find significant feats

set.seed(22)

boruta_output <- Boruta(TotalOutreachCoverage ~ ., data=pentaTrain, doTrace=2)  # perform Boruta search

boruta_signif <- names(boruta_output$finalDecision[boruta_output$finalDecision %in% c("Confirmed", "Tentative")])  # collect Confirmed and Tentative variables
print(boruta_signif)  # significant variables

## Plot signficance of covariates in predicting Y and then determine which are listed as confirmed, 
## tentative or rejected.   Tentative and Confirmed Covariates will be ultimately considered significant

plot(boruta_output, cex = .5,cex.main = .7,font.axis=.3, cex.axis=.5, las=1, xlab="Covariate", main="Variable Importance")  # plot variable importance
outreach_df <- attStats(boruta_output)

### Those covariates that were determined 
### as confirmed or tentatively significant by the Boruta Models along with those that 
### were deemed as signfiicant by the RFE featire selection should be those included in modeling
### c(1,2,4,6:8,10,11,15)


## GBM ----

### feature pruning 

outreach.step <- gbm.step(
  data=pentaTrain, 
  gbm.x = c(1,2,4,6:8,10,11,15),
  gbm.y = 20,
  family = "gaussian",
  tree.complexity = 2,
  learning.rate = 0.005,
  bag.fraction = 0.5,
  cv_folds = 10
)

gbm_pred = predict(outreach.step)
R2(pentaTrain[,20],gbm_pred)

### Get the relative influences provided by GBM to see which features are being most utilized by the model

gbm_cfs <- summary(outreach.step)
gbm_cfs <- cbind(data.frame(gbm_cfs[,1]),data.frame(gbm_cfs[,2]))
names(gbm_cfs) <- c("Feature","Rel.Influence")
xtable(data.frame(gbm_cfs))


### calculate standard error using bootstrapping method 
set.seed(0)

coefs <- data.frame("fertility"=NA, "elevation"=NA, "night_lights"=NA,  "distance_to_cities"=NA, "Population"=NA, 
                    "child_population"=NA,"radio"=NA, "electricity"=NA,"antenatal_care"=NA)

mod_performance <- data.frame("RMSE" = rep(0, 1000), "R2" = rep(0, 1000), "MAE"=rep(0, 1000))

for (i in 1:1000) {
  sample_raw = pentaTrain.raw[sample(1:nrow(pentaTrain.raw), nrow(pentaTrain.raw), replace = TRUE), ]
  train.sd <- apply(sample_raw, 2, sd)
  train.mean <- apply(sample_raw, 2, mean)
  sample_d <- scale(sample_raw) %>%
    as.data.frame()
  
  pentaTest <- test_scale(pentaTest.raw,train.mean,train.sd)
  
  clinic.step <- gbm.step(
    data=sample_d, 
    gbm.x =  c(1,2,4,6:8,10,11,15),   # selected features 
    gbm.y = 20,
    family = "gaussian",
    tree.complexity = 2,
    learning.rate = 0.005,
    bag.fraction = 0.5,
    cv_folds = 10,
    plot.main = F,
    verbose = F
  )
  
  raw_pred = predict(clinic.step,pentaTest)
  preds <- raw_preds*train.sd[20]+train.mean[20]
  rmse <- rmse(pentaTest.raw[,20],preds)
  r2 <- R2(pentaTest.raw[,20],preds)
  mae <- MAE(pentaTest.raw[,20],preds)
  
  ## fill in the blank list
  
  gbm_cfs <- summary(clinic.step)
  sing.mod <- data.frame(matrix(ncol = 9, nrow = 0))
  names(sing.mod) <- gbm_cfs[,1]
  sing.mod[1,] <- gbm_cfs[,2]
  
  coefs <- rbind(coefs, sing.mod)
  
  mod_performance[i,1] <- rmse
  mod_performance[i,2] <- r2
  mod_performance[i,3] <- mae
  
  print(i)
}

coefs <- coefs[-1,]

coef_final <- data.frame("fertility"=c(mean(coefs$fertility), std_mean(coefs$fertility)), 
                         "elevation"=c(mean(coefs$elevation), std_mean(coefs$elevation)), 
                         "night_lights"=c(mean(coefs$night_lights), std_mean(coefs$night_lights)), 
                         "distance_to_cities"=c(mean(coefs$distance_to_cities), std_mean(coefs$distance_to_cities)), 
                         "Population"=c(mean(coefs$Population), std_mean(coefs$Population)), 
                         "child_population"=c(mean(coefs$child_population), std_mean(coefs$child_population)), 
                         "radio"=c(mean(coefs$radio), std_mean(coefs$radio)), 
                         "electricity"=c(mean(coefs$electricity), std_mean(coefs$electricity)),
                         "antenatal_care"=c(mean(coefs$antenatal_care), std_mean(coefs$antenatal_care)))


data.frame("RMSE" = mean(mod_performance$RMSE), "R2" = mean(mod_performance$R2), "MAE" = mean(mod_performance$MAE))




## GAM ----

### feature pruning 

gam.form <- as.formula(TotalOutreachCoverage ~ s(fertility, k=5) + s(elevation, k=5) + s(night_lights, k=5) + 
                         s(distance_to_cities, k=5) + s(Population, k=5) + s(child_population, k=5) + 
                         s(radio, k=5) + s(electricity, k=5) + s(antenatal_care, k=5))

outreach_gam_model <- gam(gam.form, data = pentaTrain, method = "REML") 

preds <- predict(outreach_gam_model)
R2(pentaTrain[,20],preds)

outreach_gam_summary <- summary(outreach_gam_model)
outreach_gam_cfs <- -log10(as.data.frame(outreach_gam_summary$s.table)['p-value'])
xtable(data.frame(outreach_gam_cfs))

### calculate standard error using bootstrapping method 
set.seed(0)

coefs <- data.frame("fertility"=NA,"elevation"=NA, "night_lights"=NA, "distance_to_cities"=NA, "Population"=NA,
                    "child_population"=NA,  "radio"=NA,  "electricity"=NA, "antenatal_care"=NA)

mod_performance <- data.frame("RMSE" = rep(0, 1000), "R2" = rep(0, 1000), "MAE"=rep(0, 1000))

for (i in 1:1000) {
  sample_raw = pentaTrain.raw[sample(1:nrow(pentaTrain.raw), nrow(pentaTrain.raw), replace = TRUE), ]
  train.sd <- apply(sample_raw, 2, sd)
  train.mean <- apply(sample_raw, 2, mean)
  sample_d <- scale(sample_raw) %>%
    as.data.frame()
  
  pentaTest <- test_scale(pentaTest.raw,train.mean,train.sd)
  
  gam.mod <- gam(gam.form, data = sample_d, method = "REML") 
  
  raw_pred = predict(gam.mod,pentaTest)
  preds <- raw_pred*train.sd[20]+train.mean[20]
  rmse <- rmse(pentaTest.raw[,20],preds)
  r2 <- R2(pentaTest.raw[,20],preds)
  mae <- MAE(pentaTest.raw[,20],preds)
  
  
  ## fill in the blank list
  
  clinic_gam_summary <- summary(gam.mod$finalModel)
  clinic_gam_cfs <- -log10(as.data.frame(summary(gam.mod)$s.table)['p-value'])
  clinic_gam_cfs  <- as.data.frame(t(clinic_gam_cfs))
  names(clinic_gam_cfs) <- c("fertility","elevation", "night_lights", "distance_to_cities", "Population",
                             "child_population",  "radio",  "electricity", "antenatal_care")
  
  coefs <- rbind(coefs, clinic_gam_cfs)
  
  mod_performance[i,1] <- rmse
  mod_performance[i,2] <- r2
  mod_performance[i,3] <- mae
}

coefs <- coefs[-1,]

coef_clean <- coefs[is.finite(rowSums(coefs)),]
coef_final <- data.frame("fertility"=c(mean(coef_clean$fertility), std_mean(coef_clean$fertility)),
                         "elevation"=c(mean(coef_clean$elevation), std_mean(coef_clean$elevation)),
                         "night_lights"=c(mean(coef_clean$night_lights), std_mean(coef_clean$night_lights)),
                         "distance_to_cities"=c(mean(coef_clean$distance_to_cities), std_mean(coef_clean$distance_to_cities)),
                         "Population"=c(mean(coef_clean$Population), std_mean(coef_clean$Population)), 
                         "child_population"=c(mean(coef_clean$child_population), std_mean(coef_clean$child_population)),
                         "radio"=c(mean(coef_clean$radio),std_mean(coef_clean$radio)),  
                         "electricity"=c(mean(coef_clean$electricity), std_mean(coef_clean$electricity)),
                         "antenatal_care"=c(mean(coef_clean$antenatal_care),std_mean(coef_clean$antenatal_care)))


mod_clean <- mod_performance[is.finite(rowSums(coefs)),]
data.frame("RMSE" = mean(mod_performance$RMSE), "R2" = mean(mod_performance$R2), "MAE" = mean(mod_performance$MAE))




## Ridge ----


### feature pruning 

y <- pentaTrain$TotalOutreachCoverage
x <- data.matrix(pentaTrain[, c(1,2,4,6:8,10,11,15)])

ridge_model <- cv.glmnet(x, y, alpha = 0, standardize = F)

best_lambda <- ridge_model$lambda.min

ridge_best_model <- glmnet(x, y, alpha = 0, lambda = best_lambda, standardize = F)

preds <- predict(ridge_best_model, newx=data.matrix(pentaTrain[, c(1,2,4,6:8,10,11,15)]))
R2(pentaTrain[,20],preds)

ridge_outcome <- coef(ridge_best_model)
View(data.frame(ridge_outcome@Dimnames[[1]], ridge_outcome@x))


###  calculate standard error using bootstrapping method 
set.seed(0)

coefs <- data.frame("Intercept"= rep(0, 1000),  "fertility"=rep(0, 1000), "elevation"=rep(0, 1000),"night_lights"=rep(0, 1000), "distance_to_cities"=rep(0, 1000),
                    "Population"=rep(0, 1000), "child_population"=rep(0, 1000), "radio"=rep(0, 1000), "electricity"=rep(0, 1000),
                    "antenatal_care"=rep(0, 1000))

mod_performance <- data.frame("RMSE" = rep(0, 1000), "R2" = rep(0, 1000), "MAE"=rep(0, 1000))

for (i in 1:1000) {
  sample_raw = pentaTrain.raw[sample(1:nrow(pentaTrain.raw), nrow(pentaTrain.raw), replace = TRUE), ]
  train.sd <- apply(sample_raw, 2, sd)
  train.mean <- apply(sample_raw, 2, mean)
  sample_d <- scale(sample_raw) %>%
    as.data.frame()
  
  pentaTest <- test_scale(pentaTest.raw,train.mean,train.sd)
  
  y <- sample_d$TotalOutreachCoverage
  x <- data.matrix(sample_d[, c(1,2,4,6:8,10,11,15)])
  
  ridge_model <- cv.glmnet(x, y, alpha = 0,family = c("gaussian"), standardize = F)
  
  best_lambda <- ridge_model$lambda.min
  
  ridge_best_model <- glmnet(x, y, alpha = 0, lambda = best_lambda,family = c("gaussian"), standardize = F)
  ridge_outcome <- coef(ridge_best_model)
  
  raw_preds <- predict(ridge_best_model, newx=data.matrix(pentaTest[,c(1,2,4,6:8,10,11,15)]))
  preds <- raw_preds*train.sd[20]+train.mean[20]
  rmse <- rmse(pentaTest.raw[,20],preds)
  r2 <- R2(pentaTest.raw[,20],preds)
  mae <- MAE(pentaTest.raw[,20],preds)
  
  ## fill in the blank list
  coefs[i,] <- ridge_outcome@x
  mod_performance[i,1] <- rmse
  mod_performance[i,2] <- r2
  mod_performance[i,3] <- mae
}


coef_final <- data.frame("Intercept"= c(mean(coefs$Intercept), std_mean(coefs$Intercept)), 
                         "fertility"=c(mean(coefs$fertility), std_mean(coefs$fertility)), 
                         "elevation"=c(mean(coefs$elevation), std_mean(coefs$elevation)), 
                         "night_lights"=c(mean(coefs$night_lights), std_mean(coefs$night_lights)), 
                         "distance_to_cities"=c(mean(coefs$distance_to_cities), std_mean(coefs$distance_to_cities)), 
                         "Population"=c(mean(coefs$Population), std_mean(coefs$Population)), 
                         "child_population"=c(mean(coefs$child_population), std_mean(coefs$child_population)), 
                         "radio"=c(mean(coefs$radio), std_mean(coefs$radio)), 
                         "electricity"=c(mean(coefs$electricity), std_mean(coefs$electricity)),
                         "antenatal_care"=c(mean(coefs$antenatal_care), std_mean(coefs$antenatal_care)))
data.frame("RMSE" = mean(mod_performance$RMSE), "R2" = mean(mod_performance$R2), "MAE" = mean(mod_performance$MAE))




# FOR UC ----

## read in data and split train set and test set ----

ucs <- read.csv("results/uc_complete_clean.csv")
ucs <- ucs[, c(5:11, 13)] %>%  # 7 features + last col outcome
  na.omit()

data_split <- sample.split(ucs, SplitRatio = 0.8)
pentaTrain.raw <- subset(ucs, data_split == TRUE) 
train.sd <- apply(pentaTrain.raw, 2, sd)
train.mean <- apply(pentaTrain.raw, 2, mean)

pentaTrain <- scale(pentaTrain.raw) %>%
  as.data.frame()

pentaTest.raw <-subset(ucs, data_split == FALSE) 

pentaTest <- test_scale(pentaTest.raw,train.mean,train.sd)


## Feature Selection ----

### RFE Feature Selection ----

rfcontrol <- rfeControl(functions=rfFuncs, method="repeatedcv", number=10,repeats=3)
results <- rfe(pentaTrain[,1:7], pentaTrain[,8],sizes=c(1:7), rfeControl=rfcontrol)

# summarize the results

print(results)
predictors(results)

### Boruta Selection ----

set.seed(1)

boruta_output <- Boruta(TotalOutreachCoverage ~ ., data=pentaTrain, doTrace=2)  # perform Boruta search

boruta_signif <- names(boruta_output$finalDecision[boruta_output$finalDecision %in% c("Confirmed", "Tentative")])  # collect Confirmed and Tentative variables
print(boruta_signif)  # significant variables

plot(boruta_output, cex = .5,cex.main = .7,font.axis=.3, cex.axis=.5, las=1, xlab="Covariate", main="Variable Importance")  # plot variable importance
outreach_df <- attStats(boruta_output)


## ridge model ----

### VIF for lambda selection ----

lmod <- lm(TotalOutreachCoverage ~., data=pentaTrain)
vif(lmod)

y <- pentaTrain[, "TotalOutreachCoverage"]
X <- data.matrix(pentaTrain[, c(1:7)])

lambda <- c(0, 0.005, 0.01, 0.02, 0.04, 0.08, 1,5,10,20,22,23,24,25,30,40,50)
lridge <- ridge(y,X, lambda=lambda)
coef(lridge)

vridge <- vif(lridge)
vridge


### SE With lambda fixed ----

set.seed(0)

coefs <- data.frame("Intercept"= rep(0, 1000), "fertility"=rep(0, 1000), "elevation"=rep(0, 1000), "poverty"=rep(0, 1000), "distance_to_cities"=rep(0, 1000),
                    "Population"=rep(0, 1000), "child_population"=rep(0, 1000), "population_density"=rep(0, 1000))

mod_performance <- data.frame("RMSE" = rep(0, 1000), "R2" = rep(0, 1000), "MAE"=rep(0, 1000))

for (i in 1:1000) {
  sample_raw = pentaTrain.raw[sample(1:nrow(pentaTrain.raw), nrow(pentaTrain.raw), replace = TRUE), ]
  train.sd <- apply(sample_raw, 2, sd)
  train.mean <- apply(sample_raw, 2, mean)
  sample_d <- scale(sample_raw) %>%
    as.data.frame()
  
  pentaTest <- test_scale(pentaTest.raw,train.mean,train.sd)
  
  y <- sample_d$TotalOutreachCoverage
  x <- data.matrix(sample_d[, -8])
  
  ridge_best_model <- glmnet(x, y, alpha = 0, lambda = 25, standardize = F)
  ridge_outcome <- coef(ridge_best_model)
  
  raw_preds <- predict(ridge_best_model, newx=data.matrix(pentaTest[,-8]))
  preds <- raw_preds*train.sd[8]+train.mean[8]
  rmse <- rmse(pentaTest.raw[,8],preds)
  r2 <- R2(pentaTest.raw[,8],preds)
  mae <- MAE(pentaTest.raw[,8],preds)
  
  ## fill in the blank list
  coefs[i,] <- ridge_outcome@x
  mod_performance[i,1] <- rmse
  mod_performance[i,2] <- r2
  mod_performance[i,3] <- mae
  
}

coef_final <- data.frame("Intercept"= c(mean(coefs$Intercept), std_mean(coefs$Intercept)), 
                         "fertility"=c(mean(coefs$fertility), std_mean(coefs$fertility)), 
                         "elevation"=c(mean(coefs$elevation), std_mean(coefs$elevation)), 
                         "poverty"=c(mean(coefs$poverty), std_mean(coefs$poverty)), 
                         "distance_to_cities"=c(mean(coefs$distance_to_cities), std_mean(coefs$distance_to_cities)), 
                         "Population"=c(mean(coefs$Population), std_mean(coefs$Population)), 
                         "child_population"=c(mean(coefs$child_population), std_mean(coefs$child_population)), 
                         "population_density"=c(mean(coefs$population_density), std_mean(coefs$population_density)))
data.frame("RMSE" = mean(mod_performance$RMSE), "R2" = mean(mod_performance$R2), "MAE" = mean(mod_performance$MAE))



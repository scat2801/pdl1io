install.packages("pacman")

pacman::p_load("BiocManager","glmnet", "caret","ellipsis", "vctrs", "survival", "survminer", "readr", "grpreg","testthat", "pkgload", "devtools", "ROCit", "car", "ggpubr", "gplots", "testthat", "pkgload")

suppressPackageStartupMessages(library("survival","survminer", "grpreg"))
suppressPackageStartupMessages(library(glmnet))
remotes::install_github("cran/plotmo", force = TRUE)
suppressPackageStartupMessages(library(plotmo))
suppressPackageStartupMessages(library(readxl))
suppressPackageStartupMessages(library(caret, compareGroups))
suppressPackageStartupMessages(library(devtools))
suppressPackageStartupMessages(library(ggkm))
suppressPackageStartupMessages(library(ROCit))

install.packages("varhandle")
suppressPackageStartupMessages(library("varhandle"))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library("car"))
suppressPackageStartupMessages(library(gridExtra, grid, "ggpubr"))
suppressPackageStartupMessages(library(lattice, forcats, ellipsis))
suppressPackageStartupMessages(library(vctrs))

datapath <- "IOopsheet_git1.xlsx"
TV_st_col <- 17
EV_st_col <- 23
T_st_col <- 31

TVi <- read_excel(datapath, sheet = "TV-i")
TVi$Age <- as.numeric(TVi$Age)

#Rename AUC columns
names(TVi)[names(TVi) == "AUC-CSH"] <- "AUC_CSH"
names(TVi)[names(TVi) == "AUC-CSH_LHL"] <- "AUC_CSH_LHL"
names(TVi)[names(TVi) == "AUC-CSH_LHH"] <- "AUC_CSH_LHH"
names(TVi)[names(TVi) == "AUC-CSH_LLH"] <- "AUC_CSH_LLH"
names(TVi)[names(TVi) == "AUC-CSH_LLL"] <- "AUC_CSH_LLL"
names(TVi)[names(TVi) == "AUC-CSH_HLL"] <- "AUC_CSH_HLL"
names(TVi)[names(TVi) == "AUC-CSH_HLH"] <- "AUC_CSH_HLH"
names(TVi)[names(TVi) == "AUC-CSH_HHL"] <- "AUC_CSH_HHL"
names(TVi)[names(TVi) == "AUC-CSH_HHH"] <- "AUC_CSH_HHH"
names(TVi)[names(TVi) == "N voxels"] <- "N_voxels"

TVs <- read_excel(datapath, sheet = "TV-s")

#Rename AUC columns
names(TVs)[names(TVs) == "AUC-CSH"] <- "AUC_CSH"
names(TVs)[names(TVs) == "AUC-CSH_LHL"] <- "AUC_CSH_LHL"
names(TVs)[names(TVs) == "AUC-CSH_LHH"] <- "AUC_CSH_LHH"
names(TVs)[names(TVs) == "AUC-CSH_LLH"] <- "AUC_CSH_LLH"
names(TVs)[names(TVs) == "AUC-CSH_LLL"] <- "AUC_CSH_LLL"
names(TVs)[names(TVs) == "AUC-CSH_HLL"] <- "AUC_CSH_HLL"
names(TVs)[names(TVs) == "AUC-CSH_HLH"] <- "AUC_CSH_HLH"
names(TVs)[names(TVs) == "AUC-CSH_HHL"] <- "AUC_CSH_HHL"
names(TVs)[names(TVs) == "AUC-CSH_HHH"] <- "AUC_CSH_HHH"
names(TVs)[names(TVs) == "N voxels"] <- "N_voxels"

TVp <- read_excel(datapath, sheet = "TV-p")

#Rename AUC columns
names(TVp)[names(TVp) == "AUC-CSH"] <- "AUC_CSH"
names(TVp)[names(TVp) == "AUC-CSH_LHL"] <- "AUC_CSH_LHL"
names(TVp)[names(TVp) == "AUC-CSH_LHH"] <- "AUC_CSH_LHH"
names(TVp)[names(TVp) == "AUC-CSH_LLH"] <- "AUC_CSH_LLH"
names(TVp)[names(TVp) == "AUC-CSH_LLL"] <- "AUC_CSH_LLL"
names(TVp)[names(TVp) == "AUC-CSH_HLL"] <- "AUC_CSH_HLL"
names(TVp)[names(TVp) == "AUC-CSH_HLH"] <- "AUC_CSH_HLH"
names(TVp)[names(TVp) == "AUC-CSH_HHL"] <- "AUC_CSH_HHL"
names(TVp)[names(TVp) == "AUC-CSH_HHH"] <- "AUC_CSH_HHH"
names(TVp)[names(TVp) == "N voxels"] <- "N_voxels"


#External Validation 
EVi <- read_excel(datapath, sheet = "EV-i")

#Rename AUC columns
names(EVi)[names(EVi) == "AUC-CSH"] <- "AUC_CSH"
names(EVi)[names(EVi) == "AUC-CSH_LHL"] <- "AUC_CSH_LHL"
names(EVi)[names(EVi) == "AUC-CSH_LHH"] <- "AUC_CSH_LHH"
names(EVi)[names(EVi) == "AUC-CSH_LLH"] <- "AUC_CSH_LLH"
names(EVi)[names(EVi) == "AUC-CSH_LLL"] <- "AUC_CSH_LLL"
names(EVi)[names(EVi) == "AUC-CSH_HLL"] <- "AUC_CSH_HLL"
names(EVi)[names(EVi) == "AUC-CSH_HLH"] <- "AUC_CSH_HLH"
names(EVi)[names(EVi) == "AUC-CSH_HHL"] <- "AUC_CSH_HHL"
names(EVi)[names(EVi) == "AUC-CSH_HHH"] <- "AUC_CSH_HHH"
names(EVi)[names(EVi) == "N voxels"] <- "N_voxels"

EVs <- read_excel(datapath,  sheet = "EV-s")

#Rename AUC columns
names(EVs)[names(EVs) == "AUC-CSH"] <- "AUC_CSH"
names(EVs)[names(EVs) == "AUC-CSH_LHL"] <- "AUC_CSH_LHL"
names(EVs)[names(EVs) == "AUC-CSH_LHH"] <- "AUC_CSH_LHH"
names(EVs)[names(EVs) == "AUC-CSH_LLH"] <- "AUC_CSH_LLH"
names(EVs)[names(EVs) == "AUC-CSH_LLL"] <- "AUC_CSH_LLL"
names(EVs)[names(EVs) == "AUC-CSH_HLL"] <- "AUC_CSH_HLL"
names(EVs)[names(EVs) == "AUC-CSH_HLH"] <- "AUC_CSH_HLH"
names(EVs)[names(EVs) == "AUC-CSH_HHL"] <- "AUC_CSH_HHL"
names(EVs)[names(EVs) == "AUC-CSH_HHH"] <- "AUC_CSH_HHH"
names(EVs)[names(EVs) == "N voxels"] <- "N_voxels"


EVp <- read_excel(datapath, sheet = "EV-p")

#Rename AUC columns
names(EVp)[names(EVp) == "AUC-CSH"] <- "AUC_CSH"
names(EVp)[names(EVp) == "AUC-CSH_LHL"] <- "AUC_CSH_LHL"
names(EVp)[names(EVp) == "AUC-CSH_LHH"] <- "AUC_CSH_LHH"
names(EVp)[names(EVp) == "AUC-CSH_LLH"] <- "AUC_CSH_LLH"
names(EVp)[names(EVp) == "AUC-CSH_LLL"] <- "AUC_CSH_LLL"
names(EVp)[names(EVp) == "AUC-CSH_HLL"] <- "AUC_CSH_HLL"
names(EVp)[names(EVp) == "AUC-CSH_HLH"] <- "AUC_CSH_HLH"
names(EVp)[names(EVp) == "AUC-CSH_HHL"] <- "AUC_CSH_HHL"
names(EVp)[names(EVp) == "AUC-CSH_HHH"] <- "AUC_CSH_HHH"
names(EVp)[names(EVp) == "N voxels"] <- "N_voxels"

#Testing
Ti <- read_excel(datapath, sheet = "T-i")
Ti$PDL1 <- as.numeric(Ti$PDL1)

#Rename AUC columns
names(Ti)[names(Ti) == "AUC-CSH"] <- "AUC_CSH"
names(Ti)[names(Ti) == "AUC-CSH_LHL"] <- "AUC_CSH_LHL"
names(Ti)[names(Ti) == "AUC-CSH_LHH"] <- "AUC_CSH_LHH"
names(Ti)[names(Ti) == "AUC-CSH_LLH"] <- "AUC_CSH_LLH"
names(Ti)[names(Ti) == "AUC-CSH_LLL"] <- "AUC_CSH_LLL"
names(Ti)[names(Ti) == "AUC-CSH_HLL"] <- "AUC_CSH_HLL"
names(Ti)[names(Ti) == "AUC-CSH_HLH"] <- "AUC_CSH_HLH"
names(Ti)[names(Ti) == "AUC-CSH_HHL"] <- "AUC_CSH_HHL"
names(Ti)[names(Ti) == "AUC-CSH_HHH"] <- "AUC_CSH_HHH"
names(Ti)[names(Ti) == "N voxels"] <- "N_voxels"


Ts <- read_excel(datapath, sheet = "T-s")

#Rename AUC columns
names(Ts)[names(Ts) == "AUC-CSH"] <- "AUC_CSH"
names(Ts)[names(Ts) == "AUC-CSH_LHL"] <- "AUC_CSH_LHL"
names(Ts)[names(Ts) == "AUC-CSH_LHH"] <- "AUC_CSH_LHH"
names(Ts)[names(Ts) == "AUC-CSH_LLH"] <- "AUC_CSH_LLH"
names(Ts)[names(Ts) == "AUC-CSH_LLL"] <- "AUC_CSH_LLL"
names(Ts)[names(Ts) == "AUC-CSH_HLL"] <- "AUC_CSH_HLL"
names(Ts)[names(Ts) == "AUC-CSH_HLH"] <- "AUC_CSH_HLH"
names(Ts)[names(Ts) == "AUC-CSH_HHL"] <- "AUC_CSH_HHL"
names(Ts)[names(Ts) == "AUC-CSH_HHH"] <- "AUC_CSH_HHH"
names(Ts)[names(Ts) == "N voxels"] <- "N_voxels"


Tp <- read_excel(datapath, sheet = "T-p")

#Rename AUC columns
names(Tp)[names(Tp) == "AUC-CSH"] <- "AUC_CSH"
names(Tp)[names(Tp) == "AUC-CSH_LHL"] <- "AUC_CSH_LHL"
names(Tp)[names(Tp) == "AUC-CSH_LHH"] <- "AUC_CSH_LHH"
names(Tp)[names(Tp) == "AUC-CSH_LLH"] <- "AUC_CSH_LLH"
names(Tp)[names(Tp) == "AUC-CSH_LLL"] <- "AUC_CSH_LLL"
names(Tp)[names(Tp) == "AUC-CSH_HLL"] <- "AUC_CSH_HLL"
names(Tp)[names(Tp) == "AUC-CSH_HLH"] <- "AUC_CSH_HLH"
names(Tp)[names(Tp) == "AUC-CSH_HHL"] <- "AUC_CSH_HHL"
names(Tp)[names(Tp) == "AUC-CSH_HHH"] <- "AUC_CSH_HHH"
names(Tp)[names(Tp) == "N voxels"] <- "N_voxels"


##############Combine the three masks#################
#.x: lesion, .y: shell, .: patch

data_interim <- merge(TVi, TVs, by = "Patient.ID", all=TRUE)
TV_all <- merge(data_interim, TVp, by = "Patient.ID", all=TRUE)

data_interim <- merge(EVi, EVs, by = "Patient.ID", all=TRUE)
EV_all <- merge(data_interim, EVp, by = "Patient.ID", all=TRUE)

#remove NAs
EV_all <- EV_all[complete.cases(EV_all[ , 4]),]

data_interim <- merge(Ti, Ts, by = "Patient.ID", all=TRUE)
T_all <- merge(data_interim, Tp, by = "Patient.ID", all=TRUE)

rm(data_interim, TVi, TVs, TVp, EVi, EVs, EVp, Ti, Ts, Tp)

covariates <- c(colnames(TV_all))
covariates <- covariates[TV_st_col:ncol(TV_all)]


##################Define high PDL1 groups##############

#T_nor (0-1)

for (i in 1:nrow(T_all)){
  if (!is.na(T_all$PDL1[i])) {
    if (T_all$PDL1[i]<0.01){
      T_all$PDL1b[i]<- 0
    } else if (T_all$PDL1[i]<0.5){
      T_all$PDL1b[i]<- 1
    } else if (T_all$PDL1[i]<0.9){
      T_all$PDL1b[i]<- 2
    } else if (T_all$PDL1[i]>0.9){
      T_all$PDL1b[i]<- 3
    } 
  }
}


#################Standardisation########################

#Training Validation Set
TV_nor <- TV_all
for (i in TV_st_col:ncol(TV_all)){
  if (sd((TV_all[[covariates[i-TV_st_col+1]]]))!=0){
    TV_nor[[covariates[i-TV_st_col+1]]]<- scale(TV_all[[covariates[i-TV_st_col+1]]])
  } else{
    TV_nor[[covariates[i-TV_st_col+1]]] <- 0
  }
}

#Testing Set 1
T_nor <- T_all
for (i in T_st_col:ncol(T_all)){
  if (sd((T_all[[covariates[i-T_st_col+1]]]))!=0){
    T_nor[[covariates[i-T_st_col+1]]]<- scale(T_all[[covariates[i-T_st_col+1]]])
  } else{
    T_nor[[covariates[i-T_st_col+1]]] <- 0
  }
}

#Testing Set 2
#External Validation Set
EV_nor <- EV_all
for (i in (EV_st_col+1):(ncol(EV_all)+1)){
  if (sd((EV_all[[covariates[i-EV_st_col+1]]]))!=0){
    EV_nor[[covariates[i-EV_st_col+1]]]<- scale(EV_all[[covariates[i-EV_st_col+1]]])
  } else{
    EV_nor[[covariates[i-EV_st_col+1]]] <- 0
  }
}

###########Randomised Stratification of Training Validation Set#################
library(caret)

set.seed(32)

TV_bk <- TV_nor
TV_nor <- TV_nor[!is.na(TV_nor$Overall.survival..days.), ]

train.index <- createDataPartition(TV_nor$CD274, p = 3/4, list = FALSE)

TV_dis <- TV_nor[ train.index,]
TV_val  <- TV_nor[-train.index,]

##############Univariable FDR############################

univ_formulas <- sapply(covariates, function(x) as.formula(paste('CD274~', x)))
univ_models <- lapply( univ_formulas, function(x){lm(x, data = TV_dis)})

univ_results <- lapply(univ_models,
                       function(x){ 
                         x <- summary(x)
                         p.value<-signif(x$coefficients[2,4], digits=5)
                         R_squared <-signif(x$r.squared, digits=2)
                         beta<-signif(x$coefficients[2], digits=2);#coefficient beta
                         intercept<-signif(x$coefficients[1,1], digits=2);
                         res<-c(beta, intercept, R_squared, p.value)
                         names(res)<-c("beta", "intercept", "R_squared","p.value")
                         return(res)
                       })

res <- t(as.data.frame(univ_results, check.names = FALSE))
lmresult <- as.data.frame(res)

rm(res)
rm(univ_models)
rm(univ_results)
rm(univ_formulas)

##################Direct LASSO############################

p_thres <- 0.05

nFolds <- 50
foldid <- sample(rep(seq(nFolds), length.out = nrow(TV_dis)))

forlasso <- TV_dis[ , row.names(subset(lmresult, p.value<p_thres))]

list.of.fits <- list()
for (i in 0:10){
  cvfit.name <- paste0("cvalpha", i/10)
  list.of.fits[[cvfit.name]]<- cv.glmnet(x=as.matrix(forlasso), y=TV_dis$CD274, type.measure = "mse", alpha = i/10, family = "gaussian", nfolds = nFolds, foldid = foldid)
}

results <- data.frame()

#test it
for (i in 0:10){
  fit.name <- paste0("alpha", i/10)
  cvfit.name <- paste0("cvalpha", i/10)
  list.of.fits[[fit.name]] <- glmnet(x=as.matrix(forlasso), y=TV_dis$CD274, type.measure = "mse", alpha=i/10,  family="gaussian", nfolds = nFolds, lambda=list.of.fits[[cvfit.name]]$lambda.min, foldid = foldid)
  
  predicted <- predict(list.of.fits[[fit.name]], newx=as.matrix(TV_val[ ,row.names(subset(lmresult, p.value<p_thres))]), list.of.fits[[cvfit.name]]$lambda.min)
  
  mse <- mean((y= TV_val$CD274 - predicted)^2)
  results <- rbind(results, mse)
}

cvfit <- cv.glmnet(x=as.matrix(forlasso), y=TV_dis$CD274, type.measure = "mse", alpha = 1, family = "gaussian", nfolds = nFolds, foldid = foldid)
plot(cvfit)

#Get cross validated R squared for goodness of fit
rsq = 1 - cvfit$cvm/var(y)
plot(cvfit$lambda,rsq)

fit <- glmnet(x=as.matrix(forlasso), y=TV_dis$CD274, type.measure = "mse", alpha = 0.1, family = "gaussian", nfolds = nFolds, lambda=cvfit$lambda.min, foldid = foldid)
fit$beta[,1]

coef(fit)

#Prediction
prediction_model_dis <- predict(fit, newx=as.matrix(TV_dis[ ,row.names(subset(lmresult, p.value<p_thres))]), cvfit$lambda.min)

#Internal validation
prediction_model_val <- predict(fit, newx=as.matrix(TV_val[ ,row.names(subset(lmresult, p.value<p_thres))]), cvfit$lambda.min)

#External testing sets
prediction_EV <- predict(fit, newx=as.matrix(EV_nor[ ,row.names(subset(lmresult, p.value<p_thres))]), cvfit$lambda.min)
prediction_T <- predict(fit, newx=as.matrix(T_nor[ ,row.names(subset(lmresult, p.value<p_thres))]), cvfit$lambda.min)

EV_nor<-cbind(EV_nor,prediction_EV)
T_nor<-cbind(T_nor,prediction_T)

#######################Discovery Cohort#################

#Training Set
cate_dis <- TV_dis$PDL1

for (i in 1:length(cate_dis)) {
  if (cate_dis[i] == "1"){
    cate_dis[i] <- '-'
  } else {
    cate_dis[i] <- '+'
  }
}

score_dis <- unname(prediction_model_dis)

ROCit_obj <- rocit(score=score_dis, class=cate_dis, negref = "+", method ="bin")

AUC_obj <- ciAUC(ROCit_obj, level = 0.95)
p <- plot(ROCit_obj)
text(0.95, 0.2, paste0("AUC=", round(AUC_obj$AUC, 2), ", 95% CI [", round(AUC_obj$lower, 2), ",", round(AUC_obj$upper, 2), "]"), adj = 1, font = 4, cex=1.0)
title("LCI-RPV: PD-L1 Positivity - TCIA Training Set")

#Internal Validation Set
cate_val <- TV_val$PDL1

for (i in 1:length(cate_val)) {
  if (cate_val[i] == "1"){
    cate_val[i] <- '-'
  } else {
    cate_val[i] <- '+'
  }
}

score_val <- unname(prediction_model_val)

ROCit_obj <- rocit(score=score_val, class=cate_val, negref = "+", method = "bin") 

AUC_obj <- ciAUC(ROCit_obj, level = 0.95)
p <- plot(ROCit_obj)
text(0.95, 0.2, paste0("AUC=", round(AUC_obj$AUC, 2), ", 95% CI [", round(AUC_obj$lower, 2), ",", round(AUC_obj$upper, 2), "]"), adj = 1, font = 4, cex=1.0)
title("LCI-RPV: PD-L1 Positivity - TCIA Validation Set")

################External Validation (ICHNT and LCWES)#########################

#LCWES

#Include only NSCLC cases: Histology == 1 - squam, 2 - adeno, 3 - unspecified NSCLC
EV_nor <- subset (EV_nor, Histology < 4)
cate_EV <- EV_nor$PDL1b
score_EV <- unname(EV_nor$s1)

for (i in 1:length(cate_EV)) {
  if (cate_EV[i] >= "1"){
    cate_EV[i] <- '-'
  } else {
    cate_EV[i] <- '+'
  }
}

ROCit_obj1 <- rocit(score=score_EV, class=cate_EV, negref = "+", method = "bin") 
AUC_obj1 <- ciAUC(ROCit_obj1, level = 0.95)

plot(ROCit_obj1, col = c(1,"gray50"), 
     legend = FALSE, YIndex = FALSE)
text(0.95, 0.40, paste0("AUC=", round(AUC_obj1$AUC, 2), ", 95% CI [", round(AUC_obj1$lower, 2), ",", round(AUC_obj1$upper, 2), "]"), adj = 1, font = 4, cex=1, col = 1)
title("LCI-RPV: PD-L1 Positivity - LCWES")


#ICNHT
T_roc <- T_nor[complete.cases(T_nor[, 4]),]

cate_T <- T_roc$PDL1b

for (i in 1:length(cate_T)) {
  if (cate_T[i] >= "1"){
    cate_T[i] <- '-'
  } else {
    cate_T[i] <- '+'
  }
}

score_T <- unname(T_roc$s1)
ROCit_obj <- rocit(score=score_T, class=cate_T, negref = "+", method = "bin") 

AUC_obj <- ciAUC(ROCit_obj, level = 0.95)
p <- plot(ROCit_obj)
text(0.95, 0.2, paste0("AUC=", round(AUC_obj$AUC, 2), ", 95% CI [", round(AUC_obj$lower, 2), ",", round(AUC_obj$upper, 2), "]"), adj = 1, font = 4, cex=1.0)
title("LCI-RPV: PD-L1 Positivity - ICHNT")

###################High PDL1 Group Prediction###############################
T_nor_new <- T_roc

cate_T <- T_nor_new$PDL1b
prediction_T <- T_nor_new$s1

for (i in 1:length(cate_T)) {
  if (cate_T[i] >= "1"){
    cate_T[i] <- '-'
  } else {
    cate_T[i] <- '+'
  }
}

score_T <- unname(prediction_T)
ROCit_obj1 <- rocit(score=score_T, class=cate_T, negref = "+", method = "bin") 
AUC_obj1 <- ciAUC(ROCit_obj1, level = 0.95)

cate_T <- T_nor_new$PDL1b

for (i in 1:length(cate_T)) {
  if (cate_T[i] >= "2"){
    cate_T[i] <- '-'
  } else {
    cate_T[i] <- '+'
  }
}

score_T <- unname(prediction_T)
ROCit_obj2 <- rocit(score=score_T, class=cate_T, negref = "+", method = "bin") 
AUC_obj2 <- ciAUC(ROCit_obj2, level = 0.95)

cate_T <- T_nor_new$PDL1b

for (i in 1:length(cate_T)) {
  if (cate_T[i] >= "3"){
    cate_T[i] <- '-'
  } else {
    cate_T[i] <- '+'
  }
}

score_T <- unname(prediction_T)
ROCit_obj3 <- rocit(score=score_T, class=cate_T, negref = "+", method = "bin") 
AUC_obj3 <- ciAUC(ROCit_obj3, level = 0.95)

plot(ROCit_obj1, col = c(1,"gray50"), 
     legend = FALSE, YIndex = FALSE)
lines(ROCit_obj2$TPR ~ ROCit_obj2$FPR, 
      col = "steelblue3", lwd = 2)
lines(ROCit_obj3$TPR ~ ROCit_obj3$FPR, 
      col = "khaki3", lwd = 2)
legend("bottomright", col = c(1,"steelblue3","khaki3"),
       c(">1% PD-L1 ROC curve", ">50% PD-L1 ROC curve", ">90% PD-L1 ROC curve"), lwd = 2)
text(0.95, 0.40, paste0("AUC=", round(AUC_obj1$AUC, 2), ", 95% CI [", round(AUC_obj1$lower, 2), ",", round(AUC_obj1$upper, 2), "]"), adj = 1, font = 4, cex=1, col = 1)
text(0.95, 0.33, paste0("AUC=", round(AUC_obj2$AUC, 2), ", 95% CI [", round(AUC_obj2$lower, 2), ",", round(AUC_obj2$upper, 2), "]"), adj = 1, font = 4, cex=1, col = "steelblue3")
text(0.95, 0.26, paste0("AUC=", round(AUC_obj3$AUC, 2), ", 95% CI [", round(AUC_obj3$lower, 2), ",", round(AUC_obj3$upper, 2), "]"), adj = 1, font = 4, cex=1, col = "khaki3")
title("LCI-RPV: PD-L1 Positivity - ICHNT")

#######################Treatment response###########################
T_3m <- T_nor[complete.cases(T_nor$`3m`),]
T_3m <- T_3m[complete.cases(T_3m$PDL1),]

#Treatment Completion
#T_3m <- subset(T_3m, Completed==0)

#Rad
#T_3m <- subset(T_3m, Rad==0)

cate_T <- T_3m$`3m`
score_T <- unname(T_3m$s1)

for (i in 1:length(cate_T)) {
  if (cate_T[i] == "1"){
    cate_T[i] <- '-'
  } else {
    cate_T[i] <- '+'
  }
}

ROCit_obj <- rocit(score=score_T, class=cate_T, negref = "+", method = "bin") 

AUC_obj <- ciAUC(ROCit_obj, level = 0.95)
p <- plot(ROCit_obj)
text(0.95, 0.2, paste0("AUC=", round(AUC_obj$AUC, 2), ", 95% CI [", round(AUC_obj$lower, 2), ",", round(AUC_obj$upper, 2), "]"), adj = 1, font = 4, cex=1.0)
title("LCI-RPV: 3-Months Response - ICHNT")

########Use PDL1 to predict##############

score_T <- T_3m$PDL1b

cate_T <- T_3m$`3m`

for (i in 1:length(cate_T)) {
  if (cate_T[i] == "1"){
    cate_T[i] <- '-'
  } else {
    cate_T[i] <- '+'
  }
}

ROCit_obj <- rocit(score=score_T, class=cate_T, negref = "+", method = "bin") 

AUC_obj <- ciAUC(ROCit_obj, level = 0.95)
p <- plot(ROCit_obj)
text(0.95, 0.2, paste0("AUC=", round(AUC_obj$AUC, 2), ", 95% CI [", round(AUC_obj$lower, 2), ",", round(AUC_obj$upper, 2), "]"), adj = 1, font = 4, cex=1.0)
title("PD-L1 IHC: 3-Months Response - ICHNT")

#############Pneumonitis##############
T_final <- T_nor[complete.cases(T_nor$Pneumonitis),]
T_final <- T_final[complete.cases(T_final$PDL1),]

cate_T <- T_final$Pneumonitis

for (i in 1:length(cate_T)) {
  if (cate_T[i] == "1"){
    cate_T[i] <- '+'
  } else {
    cate_T[i] <- '-'
  }
}

score_T <- unname(T_final$s1)
ROCit_obj <- rocit(score=score_T, class=cate_T, negref = "+", method = "bin") 
AUC_obj <- ciAUC(ROCit_obj, level = 0.95)
p <- plot(ROCit_obj)
text(0.95, 0.2, paste0("AUC=0.64, 95% CI [0.47,0.80]"), adj = 1, font = 4, cex=1.0)
#text(0.95, 0.2, paste0("AUC=", round(AUC_obj$AUC, 2), ", 95% CI [", round(AUC_obj$lower, 2), "-", round(AUC_obj$upper, 2), "]"), adj = 1, font = 4, cex=1.0)
title("LCI-RPV: Pneumonitis - ICHNT")

#Use PDL1 to predict

T_final <- T_nor[complete.cases(T_nor$Pneumonitis),]
T_final <- T_final[complete.cases(T_final$PDL1),]

test <- T_final

score_T <- test$PDL1
cate_T <-test$Pneumonitis

for (i in 1:length(cate_T)) {
  if (cate_T[i] == "1"){
    cate_T[i] <- '+'
  } else {
    cate_T[i] <- '-'
  }
}

ROCit_obj <- rocit(score=score_T, class=cate_T, negref = "+", method = "bin") 
AUC_obj <- ciAUC(ROCit_obj, level = 0.95)
p <- plot(ROCit_obj)
text(0.95, 0.2, paste0("AUC=", round(AUC_obj$AUC, 2), ", 95% CI [", round(AUC_obj$lower, 2), ",", round(AUC_obj$upper, 2), "]"), adj = 1, font = 4, cex=1.0)
title("PD-L1 IHC: Pneumonitis - ICHNT")

############prediction k means function######################
predict.kmeans <- function(object, newdata){
  centers <- object$centers
  n_centers <- nrow(centers)
  dist_mat <- as.matrix(dist(rbind(centers, newdata)))
  dist_mat <- dist_mat[-seq(n_centers), seq(n_centers)]
  max.col(-dist_mat)
}

#########Prognostic Stratification################
install.packages("devtools", dependencies = TRUE)
library(devtools)
install_github("michaelway/ggkm", force = TRUE)
library(ggkm)

install.packages ("gtsummary")
library(gtsummary)

install.packages("survminer", repos = "https://cran.microsoft.com/snapshot/2022-06-24/")
library(survival)
library(survminer)

#External validation and testing sets

for (i in 1:nrow(EV_nor)){
  if (!is.na(EV_nor$Overall.survival..days.[i])){
    if (EV_nor$Overall.survival..days.[i] >= 1095){
      EV_nor$Overall.survival..days.[i] <- 1095
      EV_nor$OS.event[i] <- 0
    }
  }
}

no_partition <- 5

lassomat_EV <- EV_nor[ , row.names(subset(lmresult, p.value<p_thres))]
postlasso_EV <- lassomat_EV
set.seed(42)
km.res <- kmeans(postlasso_EV, no_partition, nstart = 1)

EV_nor$LCRPV_Grouping <- km.res$cluster

for (i in 1:nrow(EV_nor)){
  if ((EV_nor$LCRPV_Grouping[i] == 1) | (EV_nor$LCRPV_Grouping[i] == 5)) {
    EV_nor$LCRPV_Grouping[i] <- 1
  }
  else{
    EV_nor$LCRPV_Grouping[i] <- 2
  }
}

fit <- survfit(Surv(Overall.survival..days.,OS.event) ~ LCRPV_Grouping, data= EV_nor)

theme <- theme(axis.line = element_line(colour = "black"),
               panel.grid.major = element_line(colour = "white"),
               panel.grid.minor = element_line(colour = "white"),
               panel.border = element_blank(),
               panel.background = element_blank()
               ) 

#Fancy KM Plot
a <- ggsurvplot(
  fit,     # survfit object with calculated statistics.
  data = EV_nor,               # data used to fit survival curves. 
  pval = FALSE,             # show p-value of log-rank test.
  conf.int = TRUE,         # show confidence intervals for 
  # point estimaes of survival curves.
  xlim = c(0,1100),        # present narrower X axis, but not affect
  # survival estimates.
  break.time.by = 5,     # break X axis in time intervals by 100.
  ggtheme = theme,         # customize plot and risk table with a theme.
  risk.table.y.text.col = F, # colour risk table text annotations.
  risk.table.y.text = T, # show bars instead of names in text annotations
  # in legend of risk table
  legend.labs=c("High Risk", "Low Risk"),
  risk.table = T,
  palette="jco",
  tables.theme = theme_survminer(font.main = 12),
  title = "Kaplan-Meier Plots of Patient Groups \n Stratified Based on LCI-RPV - LCWES",
  xlab="Time in Days",
  ylab="Probability of Overall Survival",
  #surv.median.line = "v",
  ylim=c(0,1),
  cumevents=F,
  surv.scale="percent",
  font.main = c(14, "bold"),
  font.x = c(12), 
  font.y = c(12), font.tickslab = c(12),
  font.legend = c(12), risk.table.fontsize = 4
)


# extract ggplot object from ggsurvplot
p <- a$plot 
p <- p + scale_x_continuous(breaks = c(0, 200, 400, 600, 800, 1000, 1095))

# extract table object from ggsurvplot
tab <- a$table
tab$layers = NULL # clear labels
tab <- tab + 
  geom_text(aes(x = time, y = rev(strata), label = llabels), data = tab$data[tab$data$time %in% c(0, 200, 400, 600, 800, 1000, 1095),]) +
  scale_x_continuous(breaks = c(0, 200, 400, 600, 800, 1000, 1095))

# extract cumevents object from ggsurvplot
tab2 <- a$cumevents
tab2$layers = NULL # clear labels
tab2 <- tab2 + 
  geom_text(aes(x = time, y = rev(strata), label = cum.n.event), data = tab$data[tab$data$time %in% c(0, 200, 400, 600, 800, 1000, 1095),]) +
  scale_x_continuous(breaks = c(0, 200, 400, 600, 800, 1000, 1095))

# Add plots back
a$plot <- p
a$table <- tab
a$cumevents <- tab2

a

######Testing Cohort########################

T_nor_new <- T_nor

lassomat_T <- T_nor_new[ , row.names(subset(lmresult, p.value<p_thres))]
#postlasso_T <- lassomat_T
postlasso_T <- lassomat_T[, fit$beta@i]
postlasso_T <- cbind(postlasso_T, T_nor_new$s1)
postlasso_T <- cbind(postlasso_T, T_nor_new$Drug)

set.seed(12)
no_partition <- 5
km.res <- kmeans(postlasso_T, no_partition, nstart = 1)
T_nor_new$LCRPV_Grouping <- predict(km.res, postlasso_T)

for (i in 1:nrow(T_nor_new)){
  if ((T_nor_new$LCRPV_Grouping[i] == 1)|(T_nor_new$LCRPV_Grouping[i] == 3)) {
    T_nor_new$LCRPV_Grouping[i] <- 1
  }
  else{
    T_nor_new$LCRPV_Grouping[i] <- 2
  }
}

fit <- survfit(Surv(Overall.survival..days.,OS.event) ~ LCRPV_Grouping, data= T_nor_new)

theme <- theme(axis.line = element_line(colour = "black"),
               panel.grid.major = element_line(colour = "white"),
               panel.grid.minor = element_line(colour = "white"),
               panel.border = element_blank(),
               panel.background = element_blank()
) 

#Fancy KM Plot
a <- ggsurvplot(
  fit,     # survfit object with calculated statistics.
  data = T_nor_new,               # data used to fit survival curves. 
  pval = FALSE,             # show p-value of log-rank test.
  conf.int = TRUE,         # show confidence intervals for 
  # point estimaes of survival curves.
  xlim = c(0,1100),        # present narrower X axis, but not affect
  # survival estimates.
  break.time.by = 5,     # break X axis in time intervals by 100.
  ggtheme = theme,         # customize plot and risk table with a theme.
  risk.table.y.text.col = F, # colour risk table text annotations.
  risk.table.y.text = T, # show bars instead of names in text annotations
  # in legend of risk table
  legend.labs=c("High Risk", "Low Risk"),
  risk.table = T,
  palette="jco",
  tables.theme = theme_survminer(font.main = 12),
  title = "Kaplan-Meier Plots of Patient Groups \n Stratified Based on LCI-RPV - ICHNT",
  xlab="Time in Days",
  ylab="Probability of Overall Survival",
  #surv.median.line = "v",
  ylim=c(0,1),
  cumevents=F,
  surv.scale="percent",
  font.main = c(14, "bold"),
  font.x = c(12), 
  font.y = c(12), font.tickslab = c(12),
  font.legend = c(12), risk.table.fontsize = 4
)

p <- a$plot
p <- p + scale_x_continuous(breaks = c(0, 200, 400, 600, 800, 1000, 1095))

tab <- a$table
tab$layers = NULL # clear labels
tab <- tab + 
  geom_text(aes(x = time, y = rev(strata), label = llabels), data = tab$data[tab$data$time %in% c(0, 200, 400, 600, 800, 1000, 1095),]) +
  scale_x_continuous(breaks = c(0, 200, 400, 600, 800, 1000, 1095))

tab2 <- a$cumevents
tab2$layers = NULL # clear labels
tab2 <- tab2 + 
  geom_text(aes(x = time, y = rev(strata), label = cum.n.event), data = tab$data[tab$data$time %in% c(0, 200, 400, 600, 800, 1000, 1095),]) +
  scale_x_continuous(breaks = c(0, 200, 400, 600, 800, 1000, 1095))

a$plot <- p
a$table <- tab
a$cumevents <- tab2

a

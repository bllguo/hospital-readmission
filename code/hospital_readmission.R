library(leaps)
library(pROC)
library(glmnet)
library(MASS)
library(dplyr)
library(car)
rm(list=ls())
setwd("C:/Users/bllguo/Dropbox/penn_spring_15-16/STAT471/mini_Project")
data=read.csv("readmission.csv")
dim(data)
sum(is.na(data))
summary(data)
# levels(data$readmitted)
# y = data$readmitted
# levels(y)[levels(y) == "NO" | levels(y) == ">30"] = "OTHER"

# Percent of admissions <30
table(data$readmitted)[[1]] / (table(data$readmitted)[[2]]+table(data$readmitted)[[3]])
plot(data$readmitted)

# Number of unique patients
patients = unique(data$patient_nbr)
length(patients)

# Intuitively, if someone was admitted before and needs to come in again, they likely have special health issues and are more
# likely to be readmitted
# Construct an indicator variable
data[,"prev_admit"] = as.integer(duplicated(data$patient_nbr))

# Remove ID's
data$encounter_id = NULL
data$patient_nbr = NULL

levels(data$readmitted)[levels(data$readmitted)==">30"] <- "NO"
data$readmitted = relevel(data$readmitted,"NO")

# LASSO continuous only
# levels(data$readmitted)[levels(data$readmitted)==">30"] <- "NO"
# vars = c(3:10, 29:30)
# data1 = data[vars]
# X=model.matrix(readmitted~., data1)[,-1]
# dim(X)
# Y = data1$readmitted
# 
# set.seed(123)
# fit.cv.dev=cv.glmnet(X, Y, alpha=1, family="binomial", nfolds = 10, type.measure = "deviance")
# fit.cv.class=cv.glmnet(X, Y, alpha=1, family="binomial", nfolds = 10, type.measure = "class")
# fit.cv.auc=cv.glmnet(X, Y, alpha=1, family="binomial", nfolds = 10, type.measure = "auc")
# plot(fit.cv.dev)
# coef.min.dev=coef(fit.cv.dev, s="lambda.1se")
# coef.min.dev
# plot(fit.cv.class)
# coef.min.class=coef(fit.cv.class, s="lambda.min")
# coef.min.class
# plot(fit.cv.auc)
# coef.min.auc=coef(fit.cv.auc, s="lambda.1se")
# coef.min.auc
# 
# cat_vars = paste(c(names(data)[1:2], names(data)[11:28]), collapse = " + ")
# 
# var.names.dev=coef.min.dev[which(coef.min.dev!=0), ]
# var.names.dev
# var.dev=rownames(as.matrix(var.names.dev))
# var.dev
# lm.input.dev = as.formula(paste(paste("readmitted", "~", paste(var.dev[-1], collapse = "+"), "+", cat_vars)))
# lm.input.dev
# 
# var.names.class=coef.min.auc[which(coef.min.class!=0), ]
# var.names.class
# var.class=rownames(as.matrix(var.names.class))
# var.class
# lm.input.class = as.formula(paste(paste("readmitted", "~", paste(var.class[-1], collapse = "+"), "+", cat_vars)))
# lm.input.class
# 
# var.names.auc=coef.min.auc[which(coef.min.auc!=0), ]
# var.names.auc
# var.auc=rownames(as.matrix(var.names.auc))
# var.auc
# lm.input.auc = as.formula(paste(paste("readmitted", "~", paste(var.auc[-1], collapse = "+"), "+", cat_vars)))
# lm.input.auc

#1
#LASSO
set.seed(123)
X2 = model.matrix(readmitted~., data)[,-1]
dim(X2)
Y2 = data$readmitted
fit2.cv.dev=cv.glmnet(X2, Y2, alpha=1, family="binomial", nfolds = 10, type.measure = "deviance")
fit2.cv.class=cv.glmnet(X2, Y2, alpha=1, family="binomial", nfolds = 10, type.measure = "class")
fit2.cv.auc=cv.glmnet(X2, Y2, alpha=1, family="binomial", nfolds = 10, type.measure = "auc")
plot(fit2.cv.dev)
coef.min.dev=coef(fit2.cv.dev, s="lambda.1se")
coef.min.dev
plot(fit2.cv.class)
coef.min.class=coef(fit2.cv.class, s="lambda.min")
coef.min.class
plot(fit2.cv.auc)
coef.min.auc=coef(fit2.cv.auc, s="lambda.1se")
coef.min.auc

var.names.dev=coef.min.dev[which(coef.min.dev!=0), ]
var.names.dev
fit.dev = glm(readmitted ~ 
                time_in_hospital +
                num_medications+
                number_emergency+
                number_inpatient+
                number_diagnoses+
                diabetesMed+
                disch_disp_modified+
                age_mod+
                diag1_mod+
                prev_admit, data, family="binomial")
summary(fit.dev)
Anova(fit.dev)
# All variables significant at 0.05 level
fit.dev.roc=roc(data$readmitted, fit.dev$fitted, plot=T, col="blue")  
fit.dev.roc$auc
# 0.6551

#2 MCE
var.names.class=coef.min.class[which(coef.min.class!=0), ]
var.names.class
# Only 2 variables!
# 
fit.class = glm(readmitted ~ number_inpatient + prev_admit, data, family="binomial")
summary(fit.class)
fit.class.roc=roc(data$readmitted, fit.class$fitted, plot=T, col="blue")  
fit.class.roc$auc
# 0.6154

var.names.auc=coef.min.auc[which(coef.min.auc!=0), ]
var.names.auc
fit.auc = glm(readmitted ~ 
                time_in_hospital +
                num_medications+
                number_emergency+
                number_inpatient+
                number_diagnoses+
                A1Cresult+
                metformin+
                insulin+
                diabetesMed+
                disch_disp_modified+
                age_mod+
                diag1_mod+
                diag2_mod+
                diag3_mod+
                prev_admit, data, family="binomial")
fit.auc.roc=roc(data$readmitted, fit.auc$fitted, plot=T, col="blue")  
fit.auc.roc$auc
# 0.6598

# Unsurprisingly the model selected by cross-validation with type.measure="auc" has the best AUC.
# I am not sure how much weight we can put on the AUC criterion.
# Let's evaluate our models further by seeing how they perform under the Bayesian classifier.
# a10 = 1
# a01 = 2

fit.dev.pred=rep("0", 101766)
fit.dev.pred[fit.dev$fitted > 1/3]="1" 
MCE.dev=(sum(2*(fit.dev.pred[data$readmitted == "<30"] != "1")) 
         + sum((fit.dev.pred[data$readmitted == "NO"] != "0")))/length(data$readmitted)
MCE.dev

fit.class.pred=rep("0", 101766)
fit.class.pred[fit.class$fitted > 1/3]="1" 
MCE.class=(sum(2*(fit.class.pred[data$readmitted == "<30"] != "1")) 
           + sum((fit.class.pred[data$readmitted == "NO"] != "0")))/length(data$readmitted)
MCE.class

fit.auc.pred=rep("0", 101766)
fit.auc.pred[fit.auc$fitted > 1/3]="1" 
MCE.auc=(sum(2*(fit.auc.pred[data$readmitted == "<30"] != "1")) 
         + sum((fit.auc.pred[data$readmitted == "NO"] != "0")))/length(data$readmitted)
MCE.auc




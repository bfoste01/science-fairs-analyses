# Set working directory ----
setwd("/Users/bfoster/Desktop/2017-edc/science-fairs-analyses")
# Load packaes ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(MplusAutomation, psych, tidyr, foreign, ggplot2, ggalt, ggthemes, 
    readr, dplyr, knitr, scales, pander, lavaan, kableExtra, rhdf5, stringr, scales)

# Import the data ----
joined.dat <- readRDS(file = "data/joined.dat")

# Munge data ----
colnames(joined.dat)
items <- joined.dat %>%
  dplyr::select(StudentID, s_preInt_Des_15, s_preInt_Des_17, s_preInt_Des_21, s_preInt_Des_24, 
                s_preInt_Des_26, s_preInt_Des_29, s_preInt_Des_33, s_preInt_Car_16, s_preInt_Car_18, s_preInt_Car_20,
                s_preInt_Car_23, s_preInt_Car_25, s_preInt_Car_28, s_preInt_Car_30, s_preInt_Car_32, 
                s_preInt_Car_34, s_preInt_Car_36, s_preInt_Self_19, s_preInt_Self_22, 
                s_preInt_Self_27, s_preInt_Self_31 ,s_preInt_Self_35)

# munge the variable names to remove pre_ and post_ prefixes
items.stringr.prune <- items %>% 
  rename_(.dots=setNames(names(.), gsub("s_preInt_", "", names(.))))
items.stringr.prune <- items.stringr.prune %>%
    mutate(
      Des_15 = Des_15 - 1, 
      Des_17 = Des_17 - 1,
      Des_21 = Des_21 - 1,
      Des_24 = Des_24 - 1, 
      Des_26 = Des_26 - 1,
      Des_29 = Des_29 - 1,
      Des_33 = Des_33 - 1,
      Car_16 = Car_16 - 1,
      Car_18 = Car_18 - 1,
      Car_20 = Car_20 - 1,
      Car_23 = Car_23 - 1,
      Car_25 = Car_25 - 1,
      Car_28 = Car_28 - 1,
      Car_30 = Car_30 - 1,
      Car_32 = Car_32 - 1, 
      Car_34 = Car_34 - 1,
      Car_36 = Car_36 - 1,
      Self_19 = Self_19 - 1,
      Self_22 = Self_22 - 1, 
      Self_27 = Self_27 - 1,
      Self_31 = Self_31 - 1,
      Self_35 = Self_35 - 1)

irt_1pl <- '
THETA =~ 
  NA*Des_15 + a*Des_15 + a*Des_17  + a*Des_21  + a*Des_24 + a*Des_26  + a*Des_29  + a*Des_33  + a*Car_16 + a*Car_18  + a*Car_20  + a*Car_23 + a*Car_25 + a*Car_28  + a*Car_30  + a*Car_32  + a*Car_34 + a*Car_36  + a*Self_19 + a*Self_22 + a*Self_27 + a*Self_31 + a*Self_35
THETA~~1*THETA
'
fit <- cfa(irt_1pl, data = items.stringr.prune,
           ordered=c(colnames(items.stringr.prune)[-1]))
summary(fit)

#-------------------------------
# MIRT
#-------------------------------
library(mirt)
# convert syntax and estimate model
#items.stringr.prune.na <- na.omit(items.stringr.prune)
# lavaan 2 mirt
#res <- sirt::lavaan2mirt(items.stringr.prune.na,  irt_1pl , verbose=TRUE)
# converted mirt model
#cat(res$mirt.syntax)
# mirt parameter table
#res$mirt.pars
# estimate model using generated objects
#res2 <- mirt::mirt( res$dat , res$mirt.model , pars=res$mirt.pars )
#mirt.wrapper.coef(res2)    
# parameter estimates

# make the MIRT model part 1
mod.syntax_1pl_fixed <- '
THETA=1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22
CONSTRAIN = (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,a1)
COV = THETA*THETA
'
# make the MIRT model part 2
mirt_1pl_fixed  <- mirt.model(mod.syntax_1pl_fixed)

# run the mirt model
model_1pl_fixed <- mirt(items.stringr.prune[-1], model, itemnames = c(colnames(items.stringr.prune.na[-1])), 
                        itemtype = 'graded', technical = list(removeEmptyRows=TRUE))
summary(model_1pl_fixed)  # model summary
coef(model_1pl_fixed)     # model coefficients

# Item fit statistics 
mirtCluster()  # run in parallel to impute because missing
itemfit_1pl_fixed <- itemfit(model_1pl_fixed, impute = 100) # impute because of missing
itemfit_1pl_fixed.imp <- itemfit_1pl_fixed %>%
  slice(24:45) %>% 
  arrange(p.S_X2)
itemfit_1pl_fixed.imp <- tibble::add_column(itemfit_1pl_fixed.imp, rank = 1:22, "Benjamin-Hochberg (.05)" = rank*(.05/22)) %>%  
  mutate("Significant?" = ifelse(p.S_X2 <= "Benjamin-Hochberg (.05)", "Yes", "No"))
kable(itemfit_1pl_fixed.imp)

colnames(itemfit_1pl_fixed.imp)

# examine test information
Theta <- matrix(seq(-6,6,.01))
tinfo <- testinfo(model_1pl_fixed, Theta)
plot(Theta, tinfo, type = 'l')
plot(model_1pl_fixed, type = "infoSE") 

# Coefficients 
coef(model_1pl_fixed, simplify = TRUE) 
coef(model_1pl_fixed, IRTpars = TRUE, simplify = TRUE) 

# Item fit statistics 
mirtCluster() # run in parallel
itemfit(model_1pl_fixed, impute = 10) # impute because of missing

# plots for misfitting items (Car_32, Self_22, Self_19, Des_15, Car_36, Self_31, Des_26)
# plots
itemplot(model_1pl_fixed, "Car_32")
itemplot(model_1pl_fixed, "Car_32", type = 'score')
itemplot(model_1pl_fixed, "Car_32", type = 'infotrace')

itemplot(model_1pl_fixed, 2)
itemplot(model_1pl_fixed, 3)
itemplot(model_1pl_fixed, 4)
itemplot(model_1pl_fixed, 5) #flag
itemplot(model_1pl_fixed, 6)
itemplot(model_1pl_fixed, 7)
itemplot(model_1pl_fixed, 8) #flag
itemplot(model_1pl_fixed, 9)
itemplot(model_1pl_fixed, 10)
itemplot(model_1pl_fixed, 11)
itemplot(model_1pl_fixed, 12) #flag
itemplot(model_1pl_fixed, 13)
itemplot(model_1pl_fixed, 14)
itemplot(model_1pl_fixed, 15) #flag
itemplot(model_1pl_fixed, 16) #flag
itemplot(model_1pl_fixed, 17) #flag
itemplot(model_1pl_fixed, 18)
itemplot(model_1pl_fixed, 19) #flag
itemplot(model_1pl_fixed, 20) #flag
itemplot(model_1pl_fixed, 21)
itemplot(model_1pl_fixed, 22) #flag

colnames(items.stringr.prune[-1])
itemplot(model_1pl_fixed, 3)
itemplot(model_1pl_fixed, 3, CE = TRUE)
itemplot(model_1pl_fixed, 3, type = 'score')
itemplot(model_1pl_fixed, 3, type = 'infotrace')

# Factor scores vs Standardized total scores 
fs <- as.vector(fscores(model_1pl_fixed)) 
sts <- as.vector(scale(apply(items.stringr.prune.na[-1], 1, sum))) 
plot(fs ~ sts) 

# Item Characteristic Curves 
plot(model_1pl_fixed, type = "trace", facet_items = F) 
# Item Information Curves 
plot(model_1pl_fixed, type = "infotrace", facet_items = F) 
# Test Information Function 
plot(model_1pl_fixed, type = "infoSE") 

# plots
itemplot(model_1pl_fixed, 1)
itemplot(model_1pl_fixed, 2)
itemplot(model_1pl_fixed, 3)
itemplot(model_1pl_fixed, 4)
itemplot(model_1pl_fixed, 5) #flag
itemplot(model_1pl_fixed, 6)
itemplot(model_1pl_fixed, 7)
itemplot(model_1pl_fixed, 8) #flag
itemplot(model_1pl_fixed, 9)
itemplot(model_1pl_fixed, 10)
itemplot(model_1pl_fixed, 11)
itemplot(model_1pl_fixed, 12) #flag
itemplot(model_1pl_fixed, 13)
itemplot(model_1pl_fixed, 14)
itemplot(model_1pl_fixed, 15) #flag
itemplot(model_1pl_fixed, 16) #flag
itemplot(model_1pl_fixed, 17) #flag
itemplot(model_1pl_fixed, 18)
itemplot(model_1pl_fixed, 19) #flag
itemplot(model_1pl_fixed, 20) #flag
itemplot(model_1pl_fixed, 21)
itemplot(model_1pl_fixed, 22) #flag

# residuals
test <- residuals(model_1pl_fixed, type = 'exp', method = 'ML', supress = .15, tables = TRUE)
colnames(test)
plot(model_1pl_fixed)


# MIRT Sandbox ------------------------
M2(model_1pl_fixed)
M2(model_1pl_fixed, impute = 100)
itemplot(model_1pl_fixed, 2)
itemplot(model_1pl_fixed, 3)

plot(test)
residuals(model_1pl_fixed, restype = "exp")
# Values for detecting peculiar response patterns (e.g., someone answers all the hard questions right but easy ones wrong). Same for items, but could also also calculate a χ2 test and plot the fitted values.
personfit(model_1pl_fixed)
itemfit(model_1pl_fixed, X2 = TRUE)  
itemfit(model_1pl_fixed)        

itemfit(model_1pl_fixed, empirical.plot = 10)
thetas_1pl_fixed <- fscores(model_1pl_fixed, method = "EAP")
head(thetas_1pl_fixed)
hist(thetas_1pl_fixed)
?itemfit

#https://philchalmers.github.io/mirt/html/testinfo.html
Theta <- matrix(seq(-6,6,.01))
tinfo <- testinfo(model_1pl_fixed, Theta)
plot(Theta, tinfo, type = 'l')
summary(model_1pl_fixed)

plot(fit, type = "infoSE") 

# ltm-----------------------
library(ltm)
descript(items.stringr.prune.na[-1])                          # runs frequency tables for every item

# GSM
# There are two basic models that we can evaluate when setting up our IRT analysis: 
# we can assume that all of the items are equally good at discriminating among respondents, 
# or we can assume that each of the items have a different discrimination parameter. 
# Thus, we are comparing between a “constrained” model and an “unconstrained” model - 
# and, as we will see, it is possible to directly test the difference between these models, in terms of their fit to the data.
fit_grm_constrain <-grm(items.stringr.prune.na[-1], IRT.param = TRUE, constrained = TRUE)    # constrained
fit_grm_unconstrain <-grm(items.stringr.prune.na[-1], IRT.param = TRUE, constrained = FALSE) # unconstrained

# compare model fit
anova(fit_grm_constrain, fit_grm_unconstrain)

# print cutpoints and discrimination
fit_grm_unconstrain

# check the fit to the two-way margins
margins(fit_grm_unconstrain, type = c("two-way"), rule = 3.5, nprint = 3,)

# plots
plot(fit_grm_unconstrain, legend = TRUE)

plot(fit_grm_unconstrain, type = "IIC", items = 0,       # test information curve
     cex = 0.8, legend = TRUE,
     cx = "topleft", xlab = "",
     cex.main = 1, cex.lab = 1, cex.axis = 1)

# By this we mean estimating the model parameters using all the items and investigating how well the model reproduces the responses to a single item (a univariate margin), or to a pair of items (a bivariate margin).

# thetas
theta_1pl_unconstrained <- factor.scores(fit_grm_unconstrain)

# irt parameters
param_grm_unconstrain <- as_data_frame(coef(fit_grm_unconstrain)) %>%
  mutate(item = row.names(param_grm_unconstrain)) %>%
  arrange(Dscrmn)
kable(param_grm_unconstrain)

information(fit_grm_unconstrain, items=0)

ltm(items.stringr.prune.na[-1]~z1, IRT.param=TRUE)

plot(fit_grm_constrain, legend = TRUE, cx = "bottomright")
margins(fit_grm_constrain)

test <- residuals(fit_grm_constrain, order = FALSE)
test[23, ]
fit_grm_unconstrain
plot(fit_grm_unconstrain[, "z"], 1 / sqrt(info1[, "info"]), type = "l", lwd = 2, xlab = "Attitude", ylab = "Standard Error", 
     main = "Standard Error of Measurement")

# check the fit to the two-way margins
margins(fit_grm_unconstrain)

# check the fit to the three-way margins
margins(fit_grm_unconstrain, type = "three-way")


# Standard Error of Measurement
plot(fit_grm_unconstrain[, "z"], 1 / sqrt(fit_grm_unconstrain[, "info"]), type = "l", lwd = 2, xlab = "Attitude", ylab = "Standard Error", 
     main = "Standard Error of Measurement")

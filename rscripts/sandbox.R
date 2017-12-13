
# Let's first fit a Rasch model to the data
# The 1 is for "one-factor"
# run the Rasch model
rasch.mirt <- mirt(items[-1], 1, itemnames = 
                        c(colnames(items[-1])), 
                        itemtype = 'Rasch', 
                        technical = list(removeEmptyRows=TRUE, parallel=TRUE), 
                        empiricalhist = TRUE,
                        SE = TRUE, SE.type = 'sandwich',
                        verbose = FALSE)
# Let's look at the estimated parameters

# a1, corresponds to the item discrimation
# d, corresponds to the item difficulty
# g, is the guessing parameter
# u, is the sqrt of the factor uniqueness
coef(rasch.mirt)

rasch.mirt
# The provides various fit output measures

# Let's fit a 2-PL model
mirt.2pl <-  mirt(items[-1], 1, itemnames = 
                        c(colnames(items[-1])), 
                        itemtype = '2PL', 
                        technical = list(removeEmptyRows=TRUE, parallel=TRUE), 
                        empiricalhist = TRUE,
                        SE = TRUE, SE.type = 'sandwich',
                        verbose = FALSE)
coef(mirt.2pl)

# Finally, let's fit a 3-PL
mirt.3pl <- mirt(items[-1], 1, itemnames = 
                        c(colnames(items[-1])), 
                        itemtype = '3PL', 
                        technical = list(removeEmptyRows=TRUE, parallel=TRUE), 
                        empiricalhist = TRUE,
                        SE = TRUE, SE.type = 'sandwich',
                        verbose = FALSE)
coef(mirt.3pl)

# But was it appropriate to run unidimensional IRT?
oneF <- mirt(items[-1],1, technical = list(removeEmptyRows=TRUE, parallel=TRUE))
twoF <- mirt(items[-1],2, technical = list(removeEmptyRows=TRUE, parallel=TRUE))

summary(oneF)
summary(twoF)
# h2 is the proportion of a manifest variables variance explained by the factor structure (n.b. when rotation is orthogonal this is the sum of their squared factor loadings)

# You can rotate the structure to make is more understandable and suppress loadings
# By default you see it does an oblimin rotation, which is an oblique rotation
# varimax is a common orthogonal rotation
# There are a whole slew of rotations available
# ?GPArotation
# We'll also suppress the loadings that are 0.25

summary(twoF,rotate="varimax", suppress = 0.25) 

## Notice the factor correlation below

# If you want empirically compare the two models
# H_0 is that fit is the same
# reject H_0 means that we favor the more complex model 

anova(oneF,twoF)

# Given this, is it appropriate to even use the one factor model and unidimensional IRT?

# Recall that one of the assumptions is local independence
# i.e. two items are only related to one another through the factor
# and any residual errors should be uncorrelated.
# This can be formally examined

residuals(oneF)

# This prints a local dependence pairwise statistic between the items
# This statistic has a chi-square distribution with 1 df under H_0.
# Formally, extreme values larger than ...

qchisq(.95,df=1)

# Indicate violations of local independence
# This is along the bottom of the triangle

# Standardized Cramer's V, similar to correlations 
# are above the triangle
# This is again evidence that a one-factor model may be in sufficient

# Ability estimates

# Defaults to EAP
# This is expected a posteriori, or Bayes mean estimate
# This is takes the mean of the posterior distribution of the person ability estimates
fscores(rasch.mirt)

# MAP scores
# This is maximum a posteriori, or Bayes modal estimate
# This is takes the mode of the posterior distribution of the person ability estimates
fscores(rasch.mirt, method = "MAP")
# Empircial reliability is the ratio of the variance of the MAP (EAP) thetas to the
# sum of the variance of the thetas and error variance


# The prior distribution is a multivariate normal distribution with a mean vector of 0s
# and an identity matrix for the covariance matrix.
# The mean and covariances can be specified, but it doesn't appear
# as though you are able to change the actual distribution.

# These two estimates are affected by the choice of the prior distribution on the person abilities
# Therefore, if the prior distribution doesn't reflect reality, then these estimates will be biased


# Finally, ML scores
# These are maximum likelihood estimates and are based solely on the data, i.e. the likelihood
fscores(rasch.mirt, method = "ML")
# There is a problem here ...

# Can also set quadrature points here, but again how many?

# Let's see how well these models relate to one another
rs <- as.data.frame(fscores(rasch.mirt, method = "MAP"))
s2pl <- as.data.frame(fscores(mirt.2pl, method = "MAP"))
#s3pl <- as.data.frame(fscores(mirt.3pl, method = "MAP"))
scores <- data.frame(rs$F1,s2pl$F1)
cor(scores)


#########
# Plots #
#########

# To plot the test information
plot(rasch.mirt)

# To plot the item response functions
plot(rasch.mirt,type = "trace")

# To plot just the irf for item 1
plot(rasch.mirt,type = "trace",which.items= 1)

# To plot the item information functions
plot(rasch.mirt,type = "infotrace", facet_items = TRUE)

# Remove the facet
plot(rasch.mirt,type = "infotrace", facet_items = FALSE)

# To plot just the irf for item 1,2 & 5
plot(rasch.mirt,type = "infotrace",which.items= c(1:2,5))

# test response function, i.e. the expected total score
plot(rasch.mirt,type="score")

#######################
# Comparing model fit #
#######################

# First compare the rasch to the 2PL
anova(rasch.mirt,mirt.2pl)

# Compare the 2PL to the 3PL
anova(mirt.2pl,mirt.3pl) 

# There is no improvement in fit
# With these test also notice there 
# are various fit criteria such as AIC and BIC

# We can plot the 2PL as we did with the rasch
plot(mirt.2pl)
plot(mirt.2pl,type = "trace") 
# From this plot we can see that items 4 and 5 have the lowest discrimation
# and item 3 has the highest

# Let's examine information
plot(mirt.2pl,type = "infotrace") 
# How does this relate to discrimation, a?

# Interactive shiny plot
install.packages("shiny")
library("shiny")
itemplot(mirt.2pl, shiny = TRUE)

#SANDBOX

twoPL.1.model<-'
# loadings
Theta =~ l1*s_preSEP_01 + l2*s_preSEP_02 + l3*s_preSEP_03 + l4*s_preSEP_04 + l5*s_preSEP_05 + 
l6*s_preSEP_06 + l7*s_preSEP_07 + l8*s_preSEP_08 + l9*s_preSEP_09 + l10*s_preSEP_10 + 
l11*s_preSEP_11 + l12*s_preSEP_12 + l13*s_preSEP_13 + l14*s_preSEP_14

# thresholds
s_preSEP_01  | th1*t1
s_preSEP_02  | th2*t1
s_preSEP_03  | th3*t1
s_preSEP_04  | th4*t1
s_preSEP_05  | th5*t1
s_preSEP_06  | th6*t1
s_preSEP_07  | th7*t1
s_preSEP_08  | th8*t1
s_preSEP_09  | th9*t1
s_preSEP_10 | th10*t1
s_preSEP_11 | th11*t1
s_preSEP_12 | th12*t1
s_preSEP_13 | th13*t1
s_preSEP_14 | th14*t1

# convert loadings to slopes (normal)
alpha1.N := (l1)/sqrt(1-l1^2)
alpha2.N := (l2)/sqrt(1-l2^2)
alpha3.N := (l3)/sqrt(1-l3^2)
alpha4.N := (l4)/sqrt(1-l4^2)
alpha5.N := (l5)/sqrt(1-l5^2)
alpha6.N := (l6)/sqrt(1-l6^2)
alpha7.N := (l7)/sqrt(1-l7^2)
alpha8.N := (l8)/sqrt(1-l8^2)
alpha9.N := (l9)/sqrt(1-l9^2)
alpha10.N := (l10)/sqrt(1-l10^2)
alpha11.N := (l11)/sqrt(1-l11^2)
alpha12.N := (l12)/sqrt(1-l12^2)
alpha13.N := (l13)/sqrt(1-l13^2)
alpha14.N := (l14)/sqrt(1-l14^2)

# use thresholds to get difficulty: convert thresholds to intercepts (normal)
beta1.N  := (-th1)/sqrt(1-l1^2)
beta2.N  := (-th2)/sqrt(1-l2^2)
beta3.N  := (-th3)/sqrt(1-l3^2)
beta4.N  := (-th4)/sqrt(1-l4^2)
beta5.N  := (-th5)/sqrt(1-l5^2)
beta6.N  := (-th6)/sqrt(1-l6^2)
beta7.N  := (-th7)/sqrt(1-l7^2)
beta8.N  := (-th8)/sqrt(1-l8^2)
beta9.N  := (-th9)/sqrt(1-l9^2)
beta10.N := (-th10)/sqrt(1-l10^2)
beta11.N := (-th11)/sqrt(1-l11^2)
beta12.N := (-th12)/sqrt(1-l12^2)
beta13.N := (-th13)/sqrt(1-l13^2)
beta14.N := (-th14)/sqrt(1-l14^2)

# convert intercepts to locations (normal)
loc1  := -beta1.N/alpha1.N
loc2  := -beta2.N/alpha2.N
loc3  := -beta3.N/alpha3.N
loc4  := -beta4.N/alpha4.N
loc5  := -beta5.N/alpha5.N
loc6  := -beta6.N/alpha6.N
loc7  := -beta7.N/alpha7.N
loc8  := -beta8.N/alpha8.N
loc9  := -beta9.N/alpha9.N
loc10 := -beta10.N/alpha10.N
loc11 := -beta11.N/alpha11.N
loc12 := -beta12.N/alpha12.N
loc13 := -beta13.N/alpha13.N
loc14 := -beta14.N/alpha14.N

# convert loadings to discrimination: convert loadings to slopes (logistic)
alpha1.L  := (l1)/sqrt(1-l1^2)*1.7
alpha2.L  := (l2)/sqrt(1-l2^2)*1.7
alpha3.L  := (l3)/sqrt(1-l3^2)*1.7
alpha4.L  := (l4)/sqrt(1-l4^2)*1.7
alpha5.L  := (l5)/sqrt(1-l5^2)*1.7
alpha6.L  := (l6)/sqrt(1-l6^2)*1.7
alpha7.L  := (l7)/sqrt(1-l7^2)*1.7
alpha8.L  := (l8)/sqrt(1-l8^2)*1.7
alpha9.L  := (l9)/sqrt(1-l9^2)*1.7
alpha10.L := (l10)/sqrt(1-l10^2)*1.7
alpha11.L := (l11)/sqrt(1-l11^2)*1.7
alpha12.L := (l12)/sqrt(1-l12^2)*1.7
alpha13.L := (l13)/sqrt(1-l13^2)*1.7
alpha14.L := (l14)/sqrt(1-l14^2)*1.7

# convert thresholds to locations (logistic)
loc1.L := th1/l1
loc2.L := th2/l2
loc3.L := th3/l3
loc4.L := th4/l4
loc5.L := th5/l5
loc6.L := th6/l5
loc7.L := th7/l5
loc8.L := th8/l5
loc9.L := th9/l5
loc10.L := th10/l5
loc11.L := th11/l5
loc12.L := th12/l5
loc13.L := th13/l5
loc14.L := th14/l5

# convert locations to intercepts (logistic)
beta1.L  := (-alpha1.L)*loc1.L
beta2.L  := (-alpha2.L)*loc2.L
beta3.L  := (-alpha3.L)*loc3.L
beta4.L  := (-alpha4.L)*loc4.L
beta5.L  := (-alpha5.L)*loc5.L
beta6.L  := (-alpha6.L)*loc6.L
beta7.L  := (-alpha7.L)*loc7.L
beta8.L  := (-alpha8.L)*loc8.L
beta9.L  := (-alpha9.L)*loc9.L
beta10.L := (-alpha10.L)*loc10.L
beta11.L := (-alpha11.L)*loc11.L
beta12.L := (-alpha12.L)*loc12.L
beta13.L := (-alpha13.L)*loc13.L
beta14.L := (-alpha14.L)*loc14.L
'

# run the model
twoP.fit <- cfa(twoPL.1.model, data=data.frame(items),  std.lv=TRUE, 
                ordered = c("s_preSEP_01", "s_preSEP_02", "s_preSEP_03", 
                "s_preSEP_04", "s_preSEP_05", "s_preSEP_06", 
                "s_preSEP_07", "s_preSEP_08", "s_preSEP_09", 
                "s_preSEP_10", "s_preSEP_11", "s_preSEP_12", 
                "s_preSEP_13", "s_preSEP_14"))

summary(twoP.fit, standardized=TRUE, fit.measures=TRUE)
model_2pl

items.no.na <- na.omit(items)

test.mod<-'
# loadings
Theta =~ NA*l1*s_preSEP_01 + l2*s_preSEP_02 + l3*s_preSEP_03 + l4*s_preSEP_04 + l5*s_preSEP_05 + 
l6*s_preSEP_06 + l7*s_preSEP_07 + l8*s_preSEP_08 + l9*s_preSEP_09 + l10*s_preSEP_10 + 
l11*s_preSEP_11 + l12*s_preSEP_12 + l13*s_preSEP_13 + l14*s_preSEP_14

# thresholds
s_preSEP_01  | th1*t1
s_preSEP_02  | th2*t1
s_preSEP_03  | th3*t1
s_preSEP_04  | th4*t1
s_preSEP_05  | th5*t1
s_preSEP_06  | th6*t1
s_preSEP_07  | th7*t1
s_preSEP_08  | th8*t1
s_preSEP_09  | th9*t1
s_preSEP_10 | th10*t1
s_preSEP_11 | th11*t1
s_preSEP_12 | th12*t1
s_preSEP_13 | th13*t1
s_preSEP_14 | th14*t1
Theta~Theta*1
'
res <- sirt::lavaan2mirt( items.no.na , test.mod , technical=list(NCYCLES=20) , verbose=TRUE)



twoPL.1.model<-'
# loadings
Theta =~ l1*s_preSEP_01 + l2*s_preSEP_02 + l3*s_preSEP_03 + l4*s_preSEP_04 + l5*s_preSEP_05 + 
l6*s_preSEP_06 + l7*s_preSEP_07 + l8*s_preSEP_08 + l9*s_preSEP_09 + l10*s_preSEP_10 + 
l11*s_preSEP_11 + l12*s_preSEP_12 + l13*s_preSEP_13 + l14*s_preSEP_14

# thresholds
s_preSEP_01  | th1*t1
s_preSEP_02  | th2*t1
s_preSEP_03  | th3*t1
s_preSEP_04  | th4*t1
s_preSEP_05  | th5*t1
s_preSEP_06  | th6*t1
s_preSEP_07  | th7*t1
s_preSEP_08  | th8*t1
s_preSEP_09  | th9*t1
s_preSEP_10 | th10*t1
s_preSEP_11 | th11*t1
s_preSEP_12 | th12*t1
s_preSEP_13 | th13*t1
s_preSEP_14 | th14*t1

# convert loadings to slopes (normal)
alpha1.N := (l1)/sqrt(1-l1^2)
alpha2.N := (l2)/sqrt(1-l2^2)
alpha3.N := (l3)/sqrt(1-l3^2)
alpha4.N := (l4)/sqrt(1-l4^2)
alpha5.N := (l5)/sqrt(1-l5^2)
alpha6.N := (l6)/sqrt(1-l6^2)
alpha7.N := (l7)/sqrt(1-l7^2)
alpha8.N := (l8)/sqrt(1-l8^2)
alpha9.N := (l9)/sqrt(1-l9^2)
alpha10.N := (l10)/sqrt(1-l10^2)
alpha11.N := (l11)/sqrt(1-l11^2)
alpha12.N := (l12)/sqrt(1-l12^2)
alpha13.N := (l13)/sqrt(1-l13^2)
alpha14.N := (l14)/sqrt(1-l14^2)

# convert thresholds to intercepts (normal)
beta1.N  := (-th1)/sqrt(1-l1^2)
beta2.N  := (-th2)/sqrt(1-l2^2)
beta3.N  := (-th3)/sqrt(1-l3^2)
beta4.N  := (-th4)/sqrt(1-l4^2)
beta5.N  := (-th5)/sqrt(1-l5^2)
beta6.N  := (-th6)/sqrt(1-l6^2)
beta7.N  := (-th7)/sqrt(1-l7^2)
beta8.N  := (-th8)/sqrt(1-l8^2)
beta9.N  := (-th9)/sqrt(1-l9^2)
beta10.N := (-th10)/sqrt(1-l10^2)
beta11.N := (-th11)/sqrt(1-l11^2)
beta12.N := (-th12)/sqrt(1-l12^2)
beta13.N := (-th13)/sqrt(1-l13^2)
beta14.N := (-th14)/sqrt(1-l14^2)

# convert intercepts to locations (normal)
loc1  := -beta1.N/alpha1.N
loc2  := -beta2.N/alpha2.N
loc3  := -beta3.N/alpha3.N
loc4  := -beta4.N/alpha4.N
loc5  := -beta5.N/alpha5.N
loc6  := -beta6.N/alpha6.N
loc7  := -beta7.N/alpha7.N
loc8  := -beta8.N/alpha8.N
loc9  := -beta9.N/alpha9.N
loc10 := -beta10.N/alpha10.N
loc11 := -beta11.N/alpha11.N
loc12 := -beta12.N/alpha12.N
loc13 := -beta13.N/alpha13.N
loc14 := -beta14.N/alpha14.N

# convert loadings to slopes (logistic)
alpha1.L  := (l1)/sqrt(1-l1^2)*1.7
alpha2.L  := (l2)/sqrt(1-l2^2)*1.7
alpha3.L  := (l3)/sqrt(1-l3^2)*1.7
alpha4.L  := (l4)/sqrt(1-l4^2)*1.7
alpha5.L  := (l5)/sqrt(1-l5^2)*1.7
alpha6.L  := (l6)/sqrt(1-l6^2)*1.7
alpha7.L  := (l7)/sqrt(1-l7^2)*1.7
alpha8.L  := (l8)/sqrt(1-l8^2)*1.7
alpha9.L  := (l9)/sqrt(1-l9^2)*1.7
alpha10.L := (l10)/sqrt(1-l10^2)*1.7
alpha11.L := (l11)/sqrt(1-l11^2)*1.7
alpha12.L := (l12)/sqrt(1-l12^2)*1.7
alpha13.L := (l13)/sqrt(1-l13^2)*1.7
alpha14.L := (l14)/sqrt(1-l14^2)*1.7

# convert thresholds to locations (logistic)
loc1.L := th1/l1
loc2.L := th2/l2
loc3.L := th3/l3
loc4.L := th4/l4
loc5.L := th5/l5
loc6.L := th6/l5
loc7.L := th7/l5
loc8.L := th8/l5
loc9.L := th9/l5
loc10.L := th10/l5
loc11.L := th11/l5
loc12.L := th12/l5
loc13.L := th13/l5
loc14.L := th14/l5

# convert locations to intercepts (logistic)
beta1.L  := (-alpha1.L)*loc1.L
beta2.L  := (-alpha2.L)*loc2.L
beta3.L  := (-alpha3.L)*loc3.L
beta4.L  := (-alpha4.L)*loc4.L
beta5.L  := (-alpha5.L)*loc5.L
beta6.L  := (-alpha6.L)*loc6.L
beta7.L  := (-alpha7.L)*loc7.L
beta8.L  := (-alpha8.L)*loc8.L
beta9.L  := (-alpha9.L)*loc9.L
beta10.L := (-alpha10.L)*loc10.L
beta11.L := (-alpha11.L)*loc11.L
beta12.L := (-alpha12.L)*loc12.L
beta13.L := (-alpha13.L)*loc13.L
beta14.L := (-alpha14.L)*loc14.L
'

# run the model
twoP.fit <- cfa(twoPL.1.model, data=data.frame(items),  std.lv=TRUE, 
                ordered = c("s_preSEP_01", "s_preSEP_02", "s_preSEP_03", 
                "s_preSEP_04", "s_preSEP_05", "s_preSEP_06", 
                "s_preSEP_07", "s_preSEP_08", "s_preSEP_09", 
                "s_preSEP_10", "s_preSEP_11", "s_preSEP_12", 
                "s_preSEP_13", "s_preSEP_14"))
summary(twoP.fit, standardized=TRUE)
#summary(model_2pl)


twoPL.1.model <-
'
# loadings
Theta =~ l1*s_preSEP_01 + l2*s_preSEP_02 + l3*s_preSEP_03 + l4*s_preSEP_04 + l5*s_preSEP_05 + 
l6*s_preSEP_06 + l7*s_preSEP_07 + l8*s_preSEP_08 + l9*s_preSEP_09 + l10*s_preSEP_10 + 
l11*s_preSEP_11 + l12*s_preSEP_12 + l13*s_preSEP_13 + l14*s_preSEP_14

# thresholds
s_preSEP_01  | th1*t1
s_preSEP_02  | th2*t1
s_preSEP_03  | th3*t1
s_preSEP_04  | th4*t1
s_preSEP_05  | th5*t1
s_preSEP_06  | th6*t1
s_preSEP_07  | th7*t1
s_preSEP_08  | th8*t1
s_preSEP_09  | th9*t1
s_preSEP_10 | th10*t1
s_preSEP_11 | t11*t1
s_preSEP_12 | th12*t1
s_preSEP_13 | th13*t1
s_preSEP_14 | th14*t1
'

# run the model
twoP.fit <- cfa(twoPL.1.model, data=data.frame(items),  std.lv=TRUE, 
                ordered = c("s_preSEP_01", "s_preSEP_02", "s_preSEP_03", 
                "s_preSEP_04", "s_preSEP_05", "s_preSEP_06", 
                "s_preSEP_07", "s_preSEP_08", "s_preSEP_09", 
                "s_preSEP_10", "s_preSEP_11", "s_preSEP_12", 
                "s_preSEP_13", "s_preSEP_14"), parameterization='theta')
summary(twoP.fit, standardized=TRUE, fit.measures=TRUE)
Trying to model local dependence in MIRT
```{r}
#MODEL STATEMENT: 

mod.statement <- '
THETA = s_preSEP_01, s_preSEP_02, s_preSEP_03, s_preSEP_04, s_preSEP_05, s_preSEP_06, s_preSEP_07, s_preSEP_08, s_preSEP_09, s_preSEP_11, s_preSEP_12, s_preSEP_14

# two items exhibiting local dependence (should these two items not be included in the THETA call above?)
RESID.THETA = s_preSEP_10, s_preSEP_13         

# constrain the two slopes to identify the construct for the factor with local dependence
CONSTRAIN = (s_preSEP_10, s_preSEP_13, a2)   
COV = THETA*THETA
'

#RUN THE 2PL MODEL IN MIRT: 
 
mod1.2pl <- mirt(items[-1], mod.statement, itemnames = 
                        c(colnames(items[-1])), 
                        itemtype = '2PL', 
                        technical = list(removeEmptyRows=TRUE, parallel=TRUE, NCYCLES = 10000),
                        SE = TRUE, SE.type = 'sandwich')
mod1.2pl
summary(mod1.2pl)

coef(mod1.2pl, IRTparms = TRUE, simplify = TRUE)

itemfit_mod1.2pl <- itemfit(mod1.2pl, impute = 1000) 

# apply a false discovery rate to the p-value 
# p.fdr <- p.adjust(itemfit_2pl$p.S_X2,"BH")
# itemfit_2pl <- cbind(itemfit_2pl, p.fdr) # bind to postvious work

# sort the item fit statistics by p-value
itemfit_mod1.2pl %>%
  slice(1:14) %>% 
  arrange(p.S_X2) %>%
  kable(digits = 2, format="html", caption="Item Fit Statistics", escape = F) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed")) 

# Factor scores vs Standardized total scores 
fs.eap <- as_data_frame(fscores(mod1.2pl, method = "EAP", full.scores = TRUE)) %>%
  rename(theta.1.eap = F1, theta.2.eap = F2)
fs.wle <- as_data_frame(fscores(mod1.2pl, method = "WLE", full.scores = TRUE)) %>%
  rename(theta.1.wle = F1, theta.2.wle = F2)
fs.ml <- as_data_frame(fscores(mod1.2pl, method = "ML", full.scores = TRUE)) %>%
  rename(theta.1.ml = F1, theta.2.ml = F2)
sts <- as.vector(scale(apply((items)[-1], 1, sum))) 

# combine dataframe
hist(fs.eap$theta.1.eap)
hist(fs.eap$theta.2.eap)
hist(fs.wle$theta.1.wle)
hist(fs.wle$theta.2.wle)
hist(fs.ml$theta.1.ml)
hist(fs.ml$theta.2.ml)



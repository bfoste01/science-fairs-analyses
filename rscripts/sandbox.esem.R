Continuous 2 Factor with target rotation:
```{r, EfaSEMContTar.2, message=FALSE, warning=FALSE, tidy=TRUE}
efa.sem.2.obj.cont.tar <- update(efa.sem.2.obj.cont, ANALYSIS = ~ "ROTATION = TARGET;")
efaSEM2Tar.cont_mplus <- mplusModeler(efa.sem.2.obj.cont.tar,
  dataout = c("../data/06_Analayses_EfaSEMContTar.2.csv"),
  modelout = c("../mplus/06_Analayses_EfaSEMContTar.2.inp"), run=TRUE)

# extract model summaries
efaSEM2Tar_summary <- readModels("../mplus/06_Analayses_EfaSEMContTar.2.out")

# extract model summary statistics
efaSEM2Tar_summaryStats <- dplyr::select(efaSEM2Tar_summary$summaries, CFI, TLI, RMSEA_Estimate, RMSEA_90CI_LB, RMSEA_90CI_UB, SRMR, AIC, AICC, BIC, aBIC, AICC) 

# model parameters
efaSEM2Tar_summary$parameters %>%
  kable(digits = 3, format="html", caption="Model parameter estimates for the 2-factor EFA ESEM") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

# model modification indices
efaSEM2Tar_summary$mod_indices %>%
  kable(digits = 3, format="html", caption="Model modification indices for the 2-factor EFA ESEM") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
```

Continuous 2 Factor with geomin rotation:
```{r, EfaSEMContGeo.2, message=FALSE, warning=FALSE, tidy=TRUE}
efa.sem.2.obj.cont.geo <- update(efa.sem.2.obj.cont, ANALYSIS = ~ "ROTATION = GEOMIN;")
efaSEM2Tar.cont_mplus <- mplusModeler(efa.sem.2.obj.cont.tar,
  dataout = c("../data/06_Analayses_EfaSEMContGeo.2.csv"),
  modelout = c("../mplus/06_Analayses_EfaSEMContGeo.2.inp"), run=TRUE)

# extract model summaries
efaSEM2Geo_summary <- readModels("../mplus/06_Analayses_EfaSEMContGeo.2.out")

# extract model summary statistics
efaSEM2Geo_summaryStats <- dplyr::select(efaSEM2Geo_summary$summaries, CFI, TLI, RMSEA_Estimate, RMSEA_90CI_LB, RMSEA_90CI_UB, SRMR, AIC, AICC, BIC, aBIC, AICC) 

# model parameters
efaSEM2Geo_summary$parameters %>%
  kable(digits = 3, format="html", caption="Model parameter estimates for the 2-factor EFA ESEM") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

# model modification indices
efaSEM2Geo_summary$mod_indices %>%
  kable(digits = 3, format="html", caption="Model modification indices for the 2-factor EFA ESEM") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
```

Same two factor model, with categorical 
```{r, EfaSEMCat.2, message=FALSE, warning=FALSE, tidy=TRUE}
# create MPlus code
efa.sem.2.obj.cat <- mplusObject(
  TITLE = "Title: EFA of 2-factor solution using ESEM considering indicators as categorical;",
  VARIABLE = "
  Missing are all (-99, -9, -8, -1);,
  CATEGORICAL =     Des_15  Des_17  Des_21  
                    Des_24 Des_26  Des_29  
                    Des_33  Car_16 Car_18  
                    Car_20  Car_23  Car_25 
                    Car_28  Car_30  Car_32  
                    Car_34 Car_36  Self_19 
                    Self_22 Self_27 Self_31 
                    Self_35;,
  USEVARIABLES =    Des_15  Des_17  Des_21  
                    Des_24 Des_26  Des_29  
                    Des_33  Car_16 Car_18  
                    Car_20  Car_23  Car_25 
                    Car_28  Car_30  Car_32  
                    Car_34 Car_36  Self_19 
                    Self_22 Self_27 Self_31 
                    Self_35;",
  ANALYSIS = 
  "PARAMETERIZATION = THETA;
  ITERATIONS = 10000;
  ESTIMATOR = WLSMV;",
  MODEL = "f1-f2 BY Des_15  Des_17  Des_21  Des_24 
  Des_26  Des_29  Des_33  Car_16
  Car_18  Car_20  Car_23  Car_25
  Car_28  Car_30  Car_32  Car_34 
  Car_36  Self_19 Self_22 Self_27
  Self_31 Self_35 (*1);",
  OUTPUT = "SAMPSTAT MODINDICES(3.8) STANDARDIZED tech1;",
rdata = items.stringr.prune)

# examine the mplus model syntax
#str(pathmodel, max.level = 1, nchar.max = 30)

# compile and run the model in MPlus
efaSEM2.cat_mplus <- mplusModeler(efa.sem.2.obj.cat,
  dataout = c("../data/06_Analayses_EfaSEMCat.2.csv"),
  modelout = c("../mplus/06_Analayses_EfaSEMCat.2.inp"), run=TRUE)

# extract model summaries
efa_summaryStats <- readModels("../rmds/EfaSem.2.cat.out")
```

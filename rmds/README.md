About this `rmds` folder

Contains the R markdown files

This folder `rscripts` contains the scripts in R, mainly used when there is to much code to fit in a R markdown file or when external computation is used.

# 01_Import.rmds

The first script is used to import raw data (whatever the source) and save a local csv copy in data/.
Useful packages from the tidyverse here are `readr`, `readxl`, `rvest`, `haven`, and `jsonlite`.

Having the data ready as simple csv is useful to always be able to start from the beginning, even if the original source is unavailable.

# 02_Tidy.rmds

This step consists mostly of "non-destructive" data management: assign types to columns (factors with correct/human readable levels, dates, etc.), correct/censor obviously abnormal values and errors), transform between long and wide format, etc.

Useful packages here are `lubridate`, `stringr`, and `forcats`.

The results are saved in a data/tidy.rds file.

After this second step, you will have your full data ready to use in R and shouldn't have to run the first two steps anymore (unless you get hold of new data).

# 03_Analyses.rmds

This script will contain more data transforming, and the analyses with production of the resulting tables and plots.

There is a bit of an overlap between 03_Transform.R and 04_Analyze.R as it is often an iterative process. Both files can be merged into one, but it can be useful to have some time-consuming transformations in a separate script and have the results handy.

Useful packages here are broom, ggplot2, and modelr.

All the results from the analyses should be saved as-is without transformation, so that every result can be used in the Rmd. Having all the results pre-computed for the Rmd means that it will take mere seconds to re-compile, while still having access to all the results if you want/need to use them somewhere in the manuscript/report.

The results object can look like this:

    results
    ├─ tables
    │  ├─ demographics
    │  ├─ ttt_vs_control
    │  └─ table3
    ├─ list_of_interesting_values
    ├─ interesting_values2
    └─ plots
       ├─ figure1
       ├─ figure2
       └─ figure3



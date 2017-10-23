# compiles all the reports in the .rmds/ directory
source("functions/make_reports.R")

# compile individual reports
#report("rmds/01_Import.Rmd")
#report("rmds/02_Tidy.Rmd")
#report("rmds/03_Analyses.Rmd")
render("rmds/2017-10-23_04_Analyses.Rmd", "all", output_dir = "reports")

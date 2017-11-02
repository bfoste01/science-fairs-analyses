# compiles all the reports in the .rmds/ directory
source("functions/make_reports.R")
library(rmarkdown)
# compile individual reports
#report("rmds/01_Import.Rmd")
#report("rmds/02_Tidy.Rmd")
#report("rmds/03_Analyses.Rmd")
render("rmds/2017-10-23_04_Analyses.Rmd", "all", output_dir = "reports")
render("rmds/2017-10-24_05_Analyses.Rmd",  output_dir = "reports", 
       html_document(toc = TRUE, toc_depth = 3, toc_float = TRUE, theme = "sandstone",
                     code_folding = c("hide")))
render("rmds/2017-10-24_06_Analyses.Rmd",  output_dir = "reports", 
       html_document(toc = TRUE, toc_depth = 3, toc_float = TRUE, theme = "sandstone",
                     code_folding = c("hide")))
# valid themes: cerulean, cosmo, cyborgh, darkly, flatly, journal, lumen, paper, readable, sandstone, simplex, slate, solar, spacelan, superhero, united, yeti
# render(input, output_format = NULL, output_file = NULL, output_dir = NULL,
#        output_options = NULL, intermediates_dir = NULL,
#        knit_root_dir = NULL,
#        runtime = c("auto", "static", "shiny", "shiny_prerendered"),
#        clean = TRUE, params = NULL, knit_meta = NULL, envir = parent.frame(),
#        run_pandoc = TRUE, quiet = FALSE, encoding = getOption("encoding"))
render("rmds/01_Import.Rmd", "all", output_dir = "reports")

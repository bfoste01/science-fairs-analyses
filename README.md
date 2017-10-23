# btidy

This repo contains my personalized workflow for data analysis in R-Studio. I borrowed heavily from ideas in [this blog](http://blog.jom.link/implementation_basic_reproductible_workflow.html). 

# Install

If you are on github, simply fork the repo.

If you don't want to use github as your remote, clone the depo in a new directory

git clone https://www.github.com/bfoste01/btidy

Then change the git remote origin to your own remote repo.

git remote set-url your_repo_url

The project already contains a .gitignore file for R projects. Given the rules governing the use of most data and results, I have chosen to add all of the data files and reports to the .gitignore file. **This means that your data and html reports will not be stored on Github.** If there are any other files you do not want shared with Github, then these should be stored in the .gitignore file. 

# Directory structure

The project contains eight subdirectories: assets/, data/, functions/, packrat/, plots/, reports/, rmds/, and rscripts/. 

assets/ should contain any external files that are relevant for generating reports related to your project. For example, pictures or pdfs used in the reports.

data/ should contain the raw data when they exist as files (csv, xls(x), SQLite databases, SAS files, SPSS files, etc.).

functions/ should contain any function that gets used throughout your projects. Storing the functions in their own script can be useful for debugging issues related to executing the code, and more generally will keep your R Notebooks looking a bit cleaner. This folder contains a `make_reports.R` file used to compile *all* of the reports in the reports/ folder. 

packrat/ should contain all of the information about your R-session, including information about packages used for the analyses. 

plots/should contains the plots produced during the analysis in any format (bitmap or vectorial).

reports/ should contain all external documents you have about the project (synopsis, context articles/presentations, etc. 

rmds/ should contain any of the R Markdown files or R Notebooks used for analyses. 

rscripts/ should contain any complex code for modeling. Most of the time if I am interfacing between R and another program (e.g., MPlus), I'll dump my code here. 

# Other

There is a `run_all.R` file in the root directory, which if executed, will compile every file in the .rmds/ directory. 

## Resources

https://github.com/MaximeWack/tidyflow
http://blog.jom.link/implementation_basic_reproductible_workflow.html


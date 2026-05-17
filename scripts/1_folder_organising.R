
### It seems like a simple thing but organizing your project into 
### clearly labelled folders is a critical aspect of replicability 
### and being able to efficiently navigate your results


# use to quietly install purrr package 

if (!requireNamespace("purrr", quietly = TRUE)) {
  install.packages("purrr")
}

# load purrr package

library(purrr)


# Check your working directory 

getwd()

# set it to the right folder path if needed 

setwd("/Users/toby/Library/CloudStorage/OneDrive-SharkSpotters/R Projects/2026/ATAP Telemetry workshop")

# create a path object so that you can use purrr to iteratively create new
# folers in one call

folder_path <- "/Users/toby/Library/CloudStorage/OneDrive-SharkSpotters/R Projects/2026/ATAP Telemetry workshop"

# chose the folder names that you want to create

folders <- c("paper", "plots", "tables", "scripts", "data")

# run purrr:walk to create the folders

purrr::walk(
  file.path(folder_path, folders),
  dir.create,
  recursive = TRUE,
  showWarnings = FALSE
)

# Check folders were created

list.dirs(folder_path, recursive = FALSE)

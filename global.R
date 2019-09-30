# GLOBAL SETTINGS
set.seed(1337)



cleanloading = suppressPackageStartupMessages # Suppressing library's warnings/messages


cleanloading(library(DT))
cleanloading(library(shiny))
cleanloading(library(shinydashboard))
cleanloading(library(shinycssloaders))
cleanloading(library(shinyWidgets))
cleanloading(library(tibble))
cleanloading(library(e1071))
cleanloading(library(precrec))
cleanloading(library(ggplot2))
cleanloading(library(dplyr))
cleanloading(library(class))
cleanloading(library(caret))
cleanloading(library(xgboost))
cleanloading(library(corrplot))




###### BETTER SELECT INPUT #######
my_data <- tribble(
  ~cat1,	~cat2,
  "PCA Variables",	"V1",
  "PCA Variables",	"V2",
  "PCA Variables",	"V3",
  "PCA Variables",	"V4",
  "PCA Variables",	"V5",
  "PCA Variables",	"V6",
  "PCA Variables",	"V7",
  "PCA Variables",	"V8",
  "PCA Variables",	"V9",
  "PCA Variables",	"V10",
  "PCA Variables",	"V11",
  "PCA Variables",	"V12",
  "PCA Variables",	"V13",
  "PCA Variables",	"V14",
  "PCA Variables",	"V15",
  "PCA Variables",	"V16",
  "PCA Variables",	"V17",
  "PCA Variables",	"V18",
  "PCA Variables",	"V20",
  "PCA Variables",	"V21",
  "PCA Variables",	"V22",
  "PCA Variables",	"V23",
  "PCA Variables",	"V24",
  "PCA Variables",	"V25",
  "PCA Variables",	"V26",
  "PCA Variables",	"V27",
  "PCA Variables",	"V28",
  "NON-PCA Variables",	"Time",
  "NON-PCA Variables",	"Amount",
  "Target", "Class"
)

lapply(split(my_data$cat2, my_data$cat1), as.list)
###################
# sidebar.R
# 
# Create the sidebar menu options for the ui.
###################
sidebar <- dashboardSidebar(
  sidebarMenu(id = "sidebar",
    menuItem("A little introduction", tabName = "foa", icon = icon("envelope-open-text")),
    menuItem("How to SVM 101", tabName = "svm_101", icon = icon("vial")),
    menuItem("Our Analysis", tabName = "analysis", icon = icon("connectdevelop")),
    menuItem("Data Explanatory", tabName = "data_viz", icon = icon("database")),
    menuItem("SVM", tabName = "svm", icon = icon("chart-bar")),
    menuItem("XGBoost", tabName = "xgb", icon = icon("chart-bar")),
    menuItem("K-Nearest-Neighbors", tabName = "knn", icon = icon("chart-bar")),
    menuItem("Logistic Regression", tabName = "logreg", icon = icon("chart-bar")),
    menuItem("Model Comparison", tabName = "comparison", icon = icon("chart-bar"))
    
    
  )
)

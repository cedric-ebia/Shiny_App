
body <- dashboardBody(
  tabItems(
    tabItem( tabName = "foa",
      fluidPage(
        tags$iframe(src = './intro.html', 
                    width = '100%', height = '1500px', 
                    frameborder = 0, scrolling = 'auto'
        )
      )    
    ),
    
    
    
    
    tabItem(
      tabName = "svm_101",
      fluidPage(
        tags$iframe(src = './SVM_101.html', 
                    width = '100%', height = '1500px', 
                    frameborder = 0, scrolling = 'auto'
        )
      )
      

      
    ),
    
    tabItem( tabName = "analysis",
             fluidPage(
               tags$iframe(src = './rapport.html', 
                           width = '100%', height = '1500px', 
                           frameborder = 0, scrolling = 'auto'
               )
             )    
    ),
    
    ########################
    # First tab content
    ########################
    tabItem(
      tabName = "data_viz",
      
      box(
        title = "A little taste of the dataset",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        withSpinner(DT::dataTableOutput("myTable"))
      ),
      
      
      fluidRow(
        
        # CONTROLS
        box(
    
          title = "A little taste of the dataset",
          status = "primary",
          solidHeader = TRUE,
          
          # Choose a column
          selectInput(
            "columnChoice",
            "Choose a column:",
            choices = colnames(df),
            selected = "n"),
          
          sliderInput("slider", "Number of observations:", 1, 1000, 150),
          
          sliderInput("bins", "Number of breaks:", 1, 50, 10),
          
          # Create an eventReactive element
          actionButton(
            inputId = "submit",
            label = "Submit column")
          
        ),
        # PLOT THE THTINGS
        box( status = "warning",
             solidHeader = TRUE,
             checkboxInput("show_plot","Show plot",value=TRUE),
             withSpinner(plotOutput("histPlot")) )
      ),
      
      fluidRow(
        box(title = "Some descriptive statistics: ",
            status = "primary",
            solidHeader = TRUE,
            height = "150px",
            selectInput(
              "var",
              "Choose a variable:",
              choices = colnames(df),
              selected = "Time")),
            
            box(title = "Summary of the variable: ",
                status = "warning",
                solidHeader = TRUE,
                height = "150px",
                verbatimTextOutput("var"))

      ),
      fluidRow(
        
        
        box(title = "Correlations ",
            status = "primary",
            solidHeader = TRUE,
            height = "250px",
            pickerInput(
              "corr_var_1",
              "Choose the first variable you want to get the correlation with:  ",
              choices = lapply(split(my_data$cat2, my_data$cat1), as.list),
              selected = "Time",
              options = list(`actions-box`= TRUE)),
            pickerInput(
              "corr_var_2",
              "Choose the other one: ",
              choices = lapply(split(my_data$cat2, my_data$cat1), as.list),
              selected = "V3",
              options = list(`actions-box`= TRUE))),
        
        
        
        tabBox(
               tabPanel("Correlation Matrix", withSpinner(plotOutput("cor"))),
               tabPanel("Correlation between two variables", verbatimTextOutput("corr_btw_var")) ) )
      
      
    ),

    
    ########################
    # Second tab content
    ########################
    tabItem(
      tabName = "svm",
      
      fluidRow(  
        
        box( title = "SVM Model Specification",
             status = "primary",
             solidHeader = TRUE,
             height = "550px",
             width = 6,
             

                selectInput("kernel",
                             "Choose the type of kernel: ",
                             choices=c(Linear = "linear",Polynomial = "polynomial",Radial = "radial", Sigmoid = "sigmoid" ),
                             selected = "linear"),
                
                # Only show this panel if the kernel is linear
                conditionalPanel(
                  condition = "input.kernel == 'linear'",
                 selectInput("classweights_linear", "Do you want to put class weights in? :", choices=c("Yes, please", "No thanks"), selected = "No thanks"),
                 sliderInput("cross_linear", "Number of k-fold:", 1, 10, 3),
                 numericInput("cost_linear", "Cost: (min = 1 max = 100)", min=1, max = 100, step = 1, value = 1)
                ),
                # Only show this panel if the kernel is polynomial
                conditionalPanel(
                  condition = "input.kernel == 'polynomial'",
                  selectInput("poly_poly", "How many degree? :", choices=c(3,4,5), selected = 3),
                  selectInput("classweights_poly", "Do you want to put class weights in? :", choices=c("Yes, please", "No thanks"), selected = "No thanks"),
                  sliderInput("cross_poly", "Number of k-fold:", 1, 10, 3),
                  numericInput("cost_poly", "Cost: (min = 1 max = 100)", min=1, max = 100, step = 1, value = 1)
                  ),
                # Only show this panel if the kernel is polynomial
                conditionalPanel(
                  condition = "input.kernel == 'radial'",
                  selectInput("classweights_radial", "Do you want to put class weights in? :", choices=c("Yes, please", "No thanks"), selected = "No thanks"),
                  sliderInput("cross_radial", "Number of k-fold", 1, 10, 3),
                  numericInput("cost_radial", "Cost: (min = 1 max = 100)", min=1, max = 100, step = 1, value = 1),
                  sliderInput("gamma_radial", "Gamma: (min = 0.1 max = 1)", 0.1, 1, 0.1)
                ),
             conditionalPanel(
               condition = "input.kernel == 'sigmoid'",
               selectInput("classweights_sigmoid", "Do you want to put class weights in? :", choices=c("Yes, please", "No thanks"), selected = "No thanks"),
               sliderInput("cross_sigmoid", "Number of k-fold", 1, 10, 3),
               numericInput("cost_sigmoid", "Cost: (min = 1 max = 100)", min=1, max = 100, step = 1, value = 1)
             ),
                actionButton("go", "Go") ),
        
        tabBox(title = "Summary",
               tabPanel("Summary of the SVM",withSpinner(verbatimTextOutput("svm"))),
               tabPanel("Confusion Matrix", withSpinner(plotOutput("conf_svm"))),
               tabPanel("ROC", withSpinner(plotOutput("plot_ROC_svm"))),
               tabPanel("PR-ROC", withSpinner(plotOutput("plot_PRC_svm")))   ) )
      ),
    
    ########################
    #  XGBoost
    ########################
    
    tabItem(
      tabName = "xgb",
      
      fluidRow(  
        
        box( title = "XGBoost Model Specification",
             status = "primary",
             solidHeader = TRUE,
             height = "825px",
             width = 6,
             
            
             sliderInput("eta", "Choose the Learning Rate ", 0, 1, step = 0.1, value = 0.1),
             numericInput("max_depth", "Deepness of the tree :", min=1, max = 20, step = 1, value = 10),
             numericInput("min_child_weight", "Minimum sum of instance weight needed in a child: (min = 1 max = 1000)", min=1, max = 200, step = 5, value = 100),
             numericInput("colsample_bytree", "Subsample ratio of columns when constructing each tree: (min = 0.1 max = 1) ", min=0.1, max = 1, step = 0.1, value = 0.7),
             selectInput("eval_metric", "Which evaluation metrics would you use ?  :", choices=c("auc", "aucpr"), selected = "aucpr"),
             numericInput("scale_pos_weight", "Scaling: (min = 1 max = 1000)", min=1, max = 1000, step = 1, value = 577),
             sliderInput("nrounds", "Max number of boosting iterations: ", 1, 1000, step = 10, value = 500),
             sliderInput("early_stopping_rounds", "Early stopping rounds: ", 10, 100, step = 10, value = 40),
             actionButton("go2", "Go") ),


        tabBox(title = "Summary",
               tabPanel("Summary of the XGBoost",withSpinner(verbatimTextOutput("xgb"))),
               tabPanel("Confusion Matrix", withSpinner(plotOutput("conf_xgb"))),
               tabPanel("Features Importance", withSpinner(plotOutput("Imp_xgb"))),
               tabPanel("ROC", withSpinner(plotOutput("plot_ROC_xgb"))),
               tabPanel("PR-ROC", withSpinner(plotOutput("plot_PRC_xgb")))   ) )
    ),
    
    ########################
    #  KNN
    ########################
    
    tabItem(
      tabName = "knn",
      
      fluidRow(  
        
        box( title = "KNN Model Specification",
             status = "primary",
             solidHeader = TRUE,
             height = "200px",
             width = 6,
             
             
             sliderInput("k", "Choose the number of nearest neighbors: ", 1, 20, step = 1, value = 3),
             actionButton("go4", "Go") ),
        
        
        tabBox(title = "Summary",
               tabPanel("Summary of the KNN",withSpinner(verbatimTextOutput("knn"))),
               tabPanel("Confusion Matrix", withSpinner(plotOutput("conf_knn"))),
               tabPanel("ROC", withSpinner(plotOutput("plot_ROC_knn"))),
               tabPanel("PR-ROC", withSpinner(plotOutput("plot_PRC_knn")))   ) )
    ),
    
    ########################
    #  Logistic Regression
    ########################
    
    tabItem(
      tabName = "logreg",
      
      fluidRow(  
        
        box( title = "Logistic Regression Model Specification",
             status = "primary",
             solidHeader = TRUE,
             height = "210px",
             width = 6,
             pickerInput(
               "all_var",
               "Choose the variables you want to have for the model: ",
               choices = lapply(split(my_data$cat2, my_data$cat1), as.list),
               selected = c("Class"),
               options = list(`actions-box`= TRUE),
               multiple = T),
             actionButton("go3", "Go") ),
        
        tabBox(title = "Summary",
               tabPanel("Summary of the LogReg",withSpinner(verbatimTextOutput("logreg"))),
               tabPanel("Confusion Matrix", withSpinner(plotOutput("conf_logreg"))),
               tabPanel("ROC", withSpinner(plotOutput("plot_ROC_logreg"))),
               tabPanel("PR-ROC", withSpinner(plotOutput("plot_PRC_logreg")))   ) )
    ),
    
    tabItem(
      tabName = "comparison",
      
      fluidRow(  
        box( status = "warning",
             solidHeader = TRUE,
             width = 6,
             height = "450px",
             actionButton("go5", "Compare the ROCs"),
             withSpinner(plotOutput("plot_all_ROC")) ),
        
        box( status = "warning",
             solidHeader = TRUE,
             width = 6,
             height = "450px",
             actionButton("go6", "Compare the PRCs"),
             withSpinner(plotOutput("plot_all_PRC")) )
        )
    )
    
    
    
  )
)






server <- function(input, output, session) {
  
  observeEvent(input$home, {
    updateTabItems(session, "sidebar", "foa")
  })
  
  output$downloadData <- downloadHandler(
    filename = "Consignes_SVM_2019.pdf",
    content = function(file) {
      file.copy("www/Consignes_SVM_2019.pdf", file)
    })
  
  
  histPlot_df <- eventReactive(
    input$submit,
    {
      df[[ input$columnChoice ]]
    })
  
  output$histPlot <- renderPlot({
    validate(need(input$show_plot, "Show plot is unchecked. Check to see plot."))
    Sys.sleep(0.2)
    data <- histPlot_df()[ seq_len(input$slider) ]
   h =  hist(data, main = "Frequency per variable", breaks = input$bins, xlab = input$columnChoice)
    text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5))

  })
  
  output$myTable = DT::renderDataTable({
    DT::datatable(
      round(df, 2),
      rownames = TRUE,
      extensions = 'Buttons',
      options = list(
        autoWidth = FALSE, scrollX = TRUE,
        columnDefs = list(list(
          width = "125px", targets = "_all"
        )),
        dom = 'tpB',
        lengthMenu = list(c(5, 15,-1), c('5', '15', 'All')),
        pageLength = 15,
        buttons = list(
          list(
            extend = "collection",
            text = 'Show Less',
            action = DT::JS(
              "function ( e, dt, node, config ) {
                              dt.page.len(10);
                              dt.ajax.reload();}"
            )
          ),
          list(
            extend = "collection",
            text = 'Show More',
            action = DT::JS(
              "function ( e, dt, node, config ) {
                              dt.page.len(30);
                              dt.ajax.reload();}"
            )
            
          )
        )
      )
    )
  })

  output$var <- renderPrint({
   summary(df[[input$var]])
  })
  
  output$all_var <- renderPrint({
    colnames(df[,c(input$all_var)])
    
  })
  
  output$miss <- renderPrint({
    if (input$miss == "Yes, please"){
    nmiss <- function(x) {
      return(sum(is.na(x)))
    }
    apply(df[,c(input$all_var)] ,2,nmiss)
    }
    else{
      print("You don't want to see if there are any missing values")
    }
    
  })
  

  output$train <- renderPrint({
  
    set.seed(1337) 
    train.test.split <- sample(2
                               , nrow(df)
                               , replace = TRUE
                               , prob = c(as.numeric(input$prop), as.numeric(1-input$prop)))
    train = df[,c(input$all_var)][train.test.split == 1,]
    print(nrow(train))
    
  })
  
  output$test <- renderPrint({
    
    set.seed(1337) 
    train.test.split <- sample(2
                               , nrow(df)
                               , replace = TRUE
                               , prob = c(as.numeric(input$prop), as.numeric(1-input$prop)))
    test = df[,c(input$all_var)][train.test.split == 2,]
    print(nrow(test))
    
  })
  
  
  ##### SVM #####
  
  svm_classifier<- eventReactive(input$go, {
    
    set.seed(1337)
    
    if (input$kernel == "linear"){
      
      
      if(input$classweights_linear == "Yes, please"){
        
        classifier = svm(formula = Class ~ ., 
                         data = train_smote_maison, 
                         type = 'C-classification', 
                         kernel = input$kernel,
                         probability = TRUE,
                         cross = input$cross_linear,
                         class.weights = costs,
                         cost = input$cost_linear) 
        
        
        
      }else{
        
        classifier = svm(formula = Class ~ ., 
                         data = train_smote_maison, 
                         type = 'C-classification', 
                         kernel = input$kernel,
                         probability = TRUE,
                         cross = input$cross_linear,
                         cost = input$cost_linear) 
        
        
      }  }
    
    else if (input$kernel == "polynomial"){
      
      
      if(input$classweights_poly == "Yes, please"){
        
        classifier = svm(formula = Class ~ ., 
                         data = train_smote_maison, 
                         type = 'C-classification', 
                         kernel = input$kernel,
                         degree = input$poly_poly,
                         probability = TRUE,
                         cross = input$cross_poly,
                         class.weights = costs,
                         cost = input$cost_poly) 
        
        
      }else{
        
        classifier = svm(formula = Class ~ ., 
                         data = train_smote_maison, 
                         type = 'C-classification', 
                         kernel = input$kernel,
                         degree = input$poly_poly,
                         probability = TRUE,
                         cross = input$cross_poly,
                         cost = input$cost_poly) 
        
        
      }  }
    
    else  if (input$kernel == "radial"){
      
      if(input$classweights_radial == "Yes, please"){
        
        classifier = svm(formula = Class ~ ., 
                         data = train_smote_maison, 
                         type = 'C-classification', 
                         kernel = input$kernel,
                         probability = TRUE,
                         cross = input$cross_radial,
                         class.weights = costs,
                         cost = input$cost_radial,
                         gamma = input$gamma_radial) 
        
        
      }else{
        
        classifier = svm(formula = Class ~ ., 
                         data = train_smote_maison, 
                         type = 'C-classification', 
                         kernel = input$kernel,
                         probability = TRUE,
                         cross = input$cross_radial,
                         cost = input$cost_radial,
                         gamma = input$gamma_radial) 
        
        
      } }
    
    else  if (input$kernel == "sigmoid"){
      
      if(input$classweights_sigmoid == "Yes, please"){
        
        classifier = svm(formula = Class ~ ., 
                         data = train_smote_maison, 
                         type = 'C-classification', 
                         kernel = input$kernel,
                         probability = TRUE,
                         cross = input$cross_sigmoid,
                         class.weights = costs,
                         cost = input$cost_sigmoid) 
        
        
      }else{
        
        classifier = svm(formula = Class ~ ., 
                         data = train_smote_maison, 
                         type = 'C-classification', 
                         kernel = input$kernel,
                         probability = TRUE,
                         cross = input$cross_sigmoid,
                         cost = input$cost_sigmoid) 
        
        
      } }
    


  })
  

  output$svm <- renderPrint({
    
    set.seed(1337)
  summary(svm_classifier())


  })
  
  output$conf_svm <- renderPlot({
    
    set.seed(1337)
    test_svm = df[train.test.split == 2,]
    test_svm$predicted= predict(svm_classifier()
                                , newdata = as.matrix(test_svm[, colnames(test_svm) != "Class"]),probability=TRUE)
    predictions_svm2 <- predict(svm_classifier(),newdata = test)
    
    proba = as.data.frame(attr(test_svm$predicted, "probabilities"))
    test_svm = cbind(test_svm,proba)
    test_svm = test_svm[,-c(33,32)]
    colnames(test_svm)[colnames(test_svm)=="1"] <- "predicted"
    
    plot_confusion_matrix(test_svm, "SVM")
    
    
    
  })
  
  output$plot_ROC_svm <- renderPlot({
    
    set.seed(1337)
    test_svm_plot = df[train.test.split == 2,]
    predictions_svm2 <- predict(svm_classifier(),newdata = test_svm_plot, probability=T)
    svm2_predict_obj <- mmdata(as.numeric(predictions_svm2),test_svm_plot$Class)
    svm2_perfromance <- evalmod(svm2_predict_obj)
    
    curves.svm <- evalmod(svm2_predict_obj)
    curves.svm.df <- as.data.frame(curves.svm)
    
    plot(svm2_perfromance,"ROC")
    
    
  })
  
  output$plot_PRC_svm <- renderPlot({
    
    set.seed(1337)
    test_svm_plot = df[train.test.split == 2,]
    predictions_svm2 <- predict(svm_classifier(),newdata = test_svm_plot, probability=T)
    svm2_predict_obj <- mmdata(as.numeric(predictions_svm2),test_svm_plot$Class)
    svm2_perfromance <- evalmod(svm2_predict_obj)
    plot(svm2_perfromance,"PRC")
    
    
  })
  
  
  
  output$cor <- renderPlot({
  correlations <- cor(df,method="pearson") 
  corrplot(correlations, number.cex = .9, method = "circle", type = "full", tl.cex=0.8,tl.col = "black")

  
  })
  
  output$corr_btw_var <- renderPrint({
    correlations <- cor(df,method="pearson") 
    print(correlations[input$corr_var_1,input$corr_var_2])
    
  })
  
  
  
  
  ######### XG BOOST ########
  
  
  xgb_classifier<- eventReactive(input$go2, {
    
    set.seed(1337)
    
      
      xgb.model <- xgb.train(data = xgb.data.train
                             , params = list(objective = "binary:logistic"
                                             , eta = as.numeric(input$eta)
                                             , max.depth = as.numeric(input$max_depth)
                                             , min_child_weight = as.numeric(input$min_child_weight)
                                             , subsample = 0.5
                                             , colsample_bytree = as.numeric(input$colsample_bytree)
                                             , nthread = 3
                                             , scale_pos_weight = as.numeric(input$scale_pos_weight)
                                             , eval_metric = input$eval_metric
                             )
                             , watchlist = list(train = xgb.data.train,test = xgb.data.test1)
                             , nrounds = as.numeric(input$nrounds)
                             , early_stopping_rounds = as.numeric(input$early_stopping_rounds)
                             , print_every_n = 20)

  })
  
  
  output$xgb <- renderPrint({
    
    print(xgb_classifier())
    
  })
  
  output$Imp_xgb <- renderPlot({
    set.seed(1337)
    model <- xgb.dump(xgb_classifier(), with_stats=TRUE)
    names = dimnames(train_xgb)[[2]]
    importance_matrix <- xgb.importance(names, model=xgb_classifier())
    xgb.plot.importance(importance_matrix, top_n=7)
    
    

  })
  
  output$conf_xgb <- renderPlot({
    
    set.seed(1337)
    test_xgb$predicted= predict(xgb_classifier()
                            , newdata = as.matrix(test_xgb[, colnames(test_xgb) != "Class"])
                            , ntreelimit = xgb_classifier()$bestInd)
    
    plot_confusion_matrix(test_xgb, "XGBoost")

    
    
    
  })
  
  output$plot_ROC_xgb <- renderPlot({
    
    set.seed(1337)
    test_xgb_plot = df[train.test.split == 2,]
    predictions_xgb= predict(xgb_classifier()
                             , newdata = as.matrix(test_xgb_plot[, colnames(test_xgb_plot) != "Class"])
                             , ntreelimit = xgb_classifier()$bestInd)
    xgb_predict_obj <- mmdata(as.numeric(predictions_xgb),test_xgb_plot$Class)
    xgb_performance <- evalmod(xgb_predict_obj)
    plot(xgb_performance,"ROC")
    
    
  })
  
  output$plot_PRC_xgb <- renderPlot({
    
    
    
    set.seed(1337)
    test_xgb_plot = df[train.test.split == 2,]
    predictions_xgb= predict(xgb_classifier()
                             , newdata = as.matrix(test_xgb_plot[, colnames(test_xgb_plot) != "Class"])
                             , ntreelimit = xgb_classifier()$bestInd)
    xgb_predict_obj <- mmdata(as.numeric(predictions_xgb),test_xgb_plot$Class)
    xgb_performance <- evalmod(xgb_predict_obj)
    plot(xgb_performance,"PRC")
    
    
    
  })
  
  
  ######### KNN ########
  
  
  
  
 knn_classifier <- eventReactive(input$go4, {
    
   set.seed(1337)
   classifier_knn <- knn3(Class ~ .
                          , data = train_smote_maison
                          , k = input$k)
  })
  
  
  output$knn <- renderPrint({
    
    print(knn_classifier())
    
  })
  
  
  output$conf_knn <- renderPlot({
    set.seed(1337)
    test_knn = test
    predictions_knn <- predict(knn_classifier(),newdata = test_knn, type="prob")
    predictions_knn_conf <- predictions_knn[,"1"]
    conf_knn = cbind(test_knn,predictions_knn_conf)
    colnames(conf_knn)[colnames(conf_knn)=="predictions_knn_conf"] <- "predicted"
    plot_confusion_matrix(conf_knn, "K-Nearest-Neighbors")
    

  })
  
  output$plot_ROC_knn <- renderPlot({
    set.seed(1337)
    test_knn = test
    predictions_knn <- predict(knn_classifier(),newdata = test_knn, type="prob")
    predictions_knn <- predictions_knn[,"1"]
    knn_predict_obj <- mmdata(as.numeric(predictions_knn),test_knn$Class)
    knn_performance <- evalmod(knn_predict_obj)
    plot(knn_performance, "ROC")
    
    
  })
  
  output$plot_PRC_knn <- renderPlot({
    set.seed(1337)
    test_knn = test
    predictions_knn <- predict(knn_classifier(),newdata = test_knn, type="prob")
    predictions_knn <- predictions_knn[,"1"]
    knn_predict_obj <- mmdata(as.numeric(predictions_knn),test_knn$Class)
    knn_performance <- evalmod(knn_predict_obj)
    plot(knn_performance, "PRC")
    
    
    
  })
  
  
  
  ######### LogReg ########
  
  logreg_classifier <- eventReactive(input$go3, {
    
      set.seed(1337)
      classifier_logreg <- glm(data = train_smote_maison[,input$all_var], family = "binomial",
                               formula = Class ~ .)

  })
  
  
  output$logreg <- renderPrint({
    
    print(logreg_classifier())
    
  })
  
  
  output$conf_logreg <- renderPlot({
    
    set.seed(1337)
    predicted = predict(logreg_classifier(), newdata = test_glm,type="response")
    test_glm_plot = cbind(test_glm,predicted)
    plot_confusion_matrix(test_glm_plot,"Logistic Regression")
    
    
    
    
  })
  
  output$plot_ROC_logreg <- renderPlot({
    set.seed(1337)
    test_glm = test
    test_glm_plot = df[train.test.split == 2,]
    predictions_logreg <- predict(logreg_classifier(),newdata = test_glm, type = "response")
    logreg_predict_obj <- mmdata(predictions_logreg,test_glm$Class)
    logreg_performance <- evalmod(mdat = logreg_predict_obj) 
    
    plot(logreg_performance, "ROC")
    
    
  })
  
  output$plot_PRC_logreg <- renderPlot({
    
    set.seed(1337)
    test_glm_plot = df[train.test.split == 2,]
    predictions_logreg <- predict(logreg_classifier(),newdata = test_glm, type = "response")
    logreg_predict_obj <- mmdata(predictions_logreg,test_glm$Class)
    logreg_performance <- evalmod(mdat = logreg_predict_obj) 
    plot(logreg_performance, "PRC")
    

  })
  
  
  plot_all_ROCS <- eventReactive(input$go5, {
    
    set.seed(1337)
    
    
    predictions_svm2 <- predict(svm_classifier(),newdata = test_svm_plot, probability=T)
    svm2_predict_obj <- mmdata(as.numeric(predictions_svm2),test_svm_plot$Class)
    curves.svm <- evalmod(svm2_predict_obj)
    curves.svm.df <- as.data.frame(curves.svm)
    
    test_xgb_plot = df[train.test.split == 2,]
    predictions_xgb= predict(xgb_classifier()
                             , newdata = as.matrix(test_xgb_plot[, colnames(test_xgb_plot) != "Class"])
                             , ntreelimit = xgb_classifier()$bestInd)
    xgb_predict_obj <- mmdata(as.numeric(predictions_xgb),test_xgb_plot$Class)
    curves.xgb = evalmod(xgb_predict_obj)
    curves.xgb.df = as.data.frame(curves.xgb)
    
    test_knn = test
    predictions_knn <- predict(knn_classifier(),newdata = test_knn, type="prob")
    predictions_knn <- predictions_knn[,"1"]
    knn_predict_obj <- mmdata(as.numeric(predictions_knn),test_knn$Class)
    curves.knn = evalmod(knn_predict_obj)
    curves.knn.df = as.data.frame(curves.knn)
    
    
    test_glm_plot = df[train.test.split == 2,]
    predictions_logreg <- predict(logreg_classifier(),newdata = test_glm, type = "response")
    logreg_predict_obj <- mmdata(predictions_logreg,test_glm$Class)
    curves.glm = evalmod(mdat = logreg_predict_obj) 
    curves.glm.df = as.data.frame(curves.glm)
    

    
    ROC_svm = curves.svm.df %>%
      filter(type == "ROC")
    
    ROC_xgb = curves.xgb.df %>%
      filter(type == "ROC")
    
    ROC_knn = curves.knn.df %>%
      filter(type == "ROC")
    
    ROC_glm = curves.glm.df %>%
      filter(type == "ROC")
    
    
    ROC_svm$type = "SVM"
    ROC_xgb$type = "XGBoost"
    ROC_glm$type = "Logistic"
    ROC_knn$type = "KNN"
    
    ROC_ALL = rbind(ROC_svm,ROC_xgb,ROC_glm,ROC_knn)
    
    
    ggplot() +
      # green plot
      geom_line(data=ROC_ALL, aes(x=x, y=y,colour=type)) + 
      geom_abline(intercept=0, slope=1, linetype = "dashed") +
      ylab("Sensitivity") +
      xlab("1-Specificity") +
      ggtitle("ROC Comparison") +
      labs(colour = "Type of Model:")
  })
  
  
  plot_all_PRCS <- eventReactive(input$go6, {
    
    set.seed(1337)
    
    
    predictions_svm2 <- predict(svm_classifier(),newdata = test_svm_plot, probability=T)
    svm2_predict_obj <- mmdata(as.numeric(predictions_svm2),test_svm_plot$Class)
    curves.svm <- evalmod(svm2_predict_obj)
    curves.svm.df <- as.data.frame(curves.svm)
    
    test_xgb_plot = df[train.test.split == 2,]
    predictions_xgb= predict(xgb_classifier()
                             , newdata = as.matrix(test_xgb_plot[, colnames(test_xgb_plot) != "Class"])
                             , ntreelimit = xgb_classifier()$bestInd)
    xgb_predict_obj <- mmdata(as.numeric(predictions_xgb),test_xgb_plot$Class)
    curves.xgb = evalmod(xgb_predict_obj)
    curves.xgb.df = as.data.frame(curves.xgb)
    
    test_knn = test
    predictions_knn <- predict(knn_classifier(),newdata = test_knn, type="prob")
    predictions_knn <- predictions_knn[,"1"]
    knn_predict_obj <- mmdata(as.numeric(predictions_knn),test_knn$Class)
    curves.knn = evalmod(knn_predict_obj)
    curves.knn.df = as.data.frame(curves.knn)
    
    
    test_glm_plot = df[train.test.split == 2,]
    predictions_logreg <- predict(logreg_classifier(),newdata = test_glm, type = "response")
    logreg_predict_obj <- mmdata(predictions_logreg,test_glm$Class)
    curves.glm = evalmod(mdat = logreg_predict_obj) 
    curves.glm.df = as.data.frame(curves.glm)
    
 
    
    PRC_svm = curves.svm.df %>%
      filter(type == "PRC")
    
    PRC_xgb = curves.xgb.df %>%
      filter(type == "PRC")
    
    PRC_knn = curves.knn.df %>%
      filter(type == "PRC")
    
    PRC_glm = curves.glm.df %>%
      filter(type == "PRC")
    
    PRC_svm$type = "SVM"
    PRC_xgb$type = "XGBoost"
    PRC_glm$type = "Logistic"
    PRC_knn$type = "KNN"
    
    PRC_ALL = rbind(PRC_svm,PRC_xgb,PRC_glm,PRC_knn)
    
    ggplot() +
      # green plot
      geom_line(data=PRC_ALL, aes(x=x, y=y,colour=type)) + 
      ylab("Sensitivity") +
      xlab("1-Specificity") +
      ggtitle("PRC Comparison") +
      labs(colour = "Type of Model:")
    
  })
  
  output$plot_all_ROC <- renderPlot({
    
    plot_all_ROCS()
    
  })
  
  
  
  output$plot_all_PRC <- renderPlot({
    
    plot_all_PRCS()
    
  })
  
  
  
  
}

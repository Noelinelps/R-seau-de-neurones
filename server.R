#setwd("D:/Documents/FAC/M2/semestre 10/- Projets/reseau de neurones/FINAL")

server = function(input, output) {
  
  observeEvent(input$pdf, {file.show(file.path("www/Dictionnaire_credit.pdf")) })
  
  observeEvent(input$pdf1, {file.show(file.path("www/Dictionnaire_kaggle.pdf")) })
  
  #show intro modal
  observeEvent("", {
    showModal(modalDialog(
      includeHTML("intro.Rhtml"),
      easyClose = TRUE)
    )
  })
  
  output$tab1 <- renderTable({head(base)})
  
  output$tab <- renderTable({head(data)})
  
  output$summary <- renderPrint({summary(data)})
  
  output$summary1 <- renderPrint({summary(base)})
  
  output$graph1 <- renderPlot({graph})
  
  output$graph3 <- renderPlot({graph3})
  
  output$gra <- renderPlot({gra})
  
  output$gra1 <- renderPlot({gra1})
  
  output$gra2 <- renderPlot({gra2})
  
  output$gra3 <- renderPlot({gra3})
  
  output$gra4 <- renderPlot({gra4})
  

  #pmc sur crédit
  model_pmc1 <- reactive({ 
    if(input$scenario=="1"){
      neuralnet(Y~., data=train2_b, hidden=c(4), linear.output=F, stepmax=100000)
    }
    else if(input$scenario=="2"){
      neuralnet(Y~., data=train2_b, hidden=c(4,2), linear.output=F, stepmax=100000)
    }
    else if (input$scenario=="3"){
      neuralnet(Y~., data=train2_b, hidden=c(5,4,3), linear.output=F, stepmax=100000)
    }
    })
    
  output$graph_pmc <- renderPlot({plot(model_pmc1())})
  
  pred_pmc1 <- reactive({ifelse(predict(model_pmc1(), xb_test)<0.5, 0,1)})
  
  output$matconfpmc_credit <- renderTable({
    c=table(test_b[,1], as.factor(pred_pmc1()))
    tab.pmc=rbind(c(c[1],c[3]),c(c[2],c[4]))
    name=c("réponse 0", "réponse 1")
    tab.pmc=data.frame(cbind(name,tab.pmc))
    colnames(tab.pmc)=c(" ","Prédiction 0", "Prédiction 1")
    print(tab.pmc)})
  
  
  output$accpmc_credit <- renderValueBox({
    valueBox(
      subtitle="Précision",
      mapply(round, mean(pred_pmc1()==test_b[,1]),4),
      icon = icon("check-double"),
      color="navy")   })
  
  output$aucpmc_credit <- renderValueBox({
    valueBox(
      subtitle="AUC", width=8,
      mapply(round, auc(roc(test_b[,1], as.numeric(pred_pmc1()))),4),
      icon = icon("check-double"),
      color="navy")  })
  
  
  
  #RBF sur crédit
  model_rbf1 <- reactive({rbf(xb, yb, size=input$nh, linOut=F)})
  
  pred_rbf1 <- reactive({ifelse(predict(model_rbf1(), xb_test)<input$cutoff1, 0,1)})
  
  output$matconfrbf_credit <- renderTable({
    c=table(test_b[,1], as.factor(pred_rbf1()))
    tab.rbf=rbind(c(c[1],c[3]),c(c[2],c[4]))
    name=c("réponse 0", "réponse 1")
    tab.rbf=data.frame(cbind(name,tab.rbf))
    colnames(tab.rbf)=c(" ","Prédiction 0", "Prédiction 1")
    print(tab.rbf)})
  
  
  output$accrbf_credit <- renderValueBox({
    valueBox(
      subtitle="Précision",
      mapply(round, mean(pred_rbf1()==test_b[,1]),4),
      icon = icon("check-double"),
      color="navy")   })
  
  output$aucrbf_credit <- renderValueBox({
    valueBox(
      subtitle="AUC", width=8,
      mapply(round, auc(roc(test_b[,1], as.numeric(pred_rbf1()))),4),
      icon = icon("check-double"),
      color="navy")  })
 
  
  
  
  
  #regression logistique sur Kaggle
  model_reg <- reactive({glm(as.factor(ser_delinquency)~.,data=train, family=binomial(link = logit))})
  
  pred_reg <- reactive({ifelse(predict(model_reg(), data.frame(x_test), type="response")<input$cutoff2, 0,1)})

  output$matconfreg_kaggle <- renderTable({
    reg=table(test[,1], as.factor(pred_reg()))
    
    #correction de la matrice
    sp.reg=reg[1]/(reg[1]+reg[3])
    se.reg=reg[4]/(reg[2]+reg[4])
    
    name=c("réponse 0", "réponse 1")
    matreg1=c(round(freq0*sp.reg*length(test[,1])),round(freq0*(1-sp.reg)*length(test[,1])))
    matreg2=c(round(freq1*(1-se.reg)*length(test[,1])),round(freq1*se.reg*length(test[,1])))
    tab2.reg=data.frame(name, matreg1, matreg2)
    colnames(tab2.reg)=c(" ","Prédiction 0", "Prédiction 1")
    print(as.matrix(tab2.reg))})
  
  output$accreg_kaggle <- renderValueBox({
    valueBox(
      subtitle="Précision",
      mapply(round, mean(pred_reg()==test[,1]),4),
      icon = icon("check-double"),
      color="navy")   })
  
  output$aucreg_kaggle <- renderValueBox({
    valueBox(
      subtitle="AUC", width=8,
      mapply(round, auc(roc(test[,1], as.numeric(pred_reg()))),4),
      icon = icon("check-double"),
      color="navy")  })
 
  
  
  #RBF sur kaggle
  model_rbf2 <- reactive({rbf(x, as.numeric(y), size=input$nh2, linOut=F)})
  
  pred_rbf2 <- reactive({ifelse(predict(model_rbf2(), x_test)<input$cutoff4, 0,1)})
  
  output$matconfrbf_kaggle <- renderTable({
    rbf=table(test[,1], as.factor(pred_rbf2()))
    
    #correction de la matrice
    sp.rbf=rbf[1]/(rbf[1]+rbf[3])
    se.rbf=rbf[4]/(rbf[2]+rbf[4])
    
    name=c("réponse 0", "réponse 1")
    matrbf1=c(round(freq0*sp.rbf*length(test[,1])),round(freq0*(1-sp.rbf)*length(test[,1])))
    matrbf2=c(round(freq1*(1-se.rbf)*length(test[,1])),round(freq1*se.rbf*length(test[,1])))
    tab2.rbf=data.frame(name, matrbf1, matrbf2)
    colnames(tab2.rbf)=c(" ","Prédiction 0", "Prédiction 1")
    print(as.matrix(tab2.rbf))})
  
  
  output$accrbf_kaggle <- renderValueBox({
    valueBox(
      subtitle="Précision",
      mapply(round, mean(as.numeric(pred_rbf2())==test[,1]),4),
      icon = icon("check-double"),
      color="navy")   })
  
  output$aucrbf_kaggle <- renderValueBox({
    valueBox(
      subtitle="AUC", width=8,
      mapply(round, auc(roc(test[,1], as.numeric(pred_rbf2()))),4),
      icon = icon("check-double"),
      color="navy")  })
  
  
  
  
  #arbre de décision sur kaggle
  arbre <- reactive({rpart(ser_delinquency~.,train, method="class", control = rpart.control(cp = input$cp))})
  
  output$graph_arbre <- renderPlot({plot(arbre(), uniform = TRUE, branch = 0.4, margin = 0.01)
                                    text(arbre())})
  
  pred.arbre <- reactive({predict(arbre(), data.frame(x_test), type="class")})
  
  
  output$matconfarbre_kaggle <- renderTable({ 
    tab.arbre=as.matrix(table(test[,1], pred.arbre()))
    
    #correction de la matrice
    sp.arbre=tab.arbre[1]/(tab.arbre[1]+tab.arbre[3])
    se.arbre=tab.arbre[4]/(tab.arbre[2]+tab.arbre[4])
    
    name=c("Réponse 0", "Réponse 1")
    arbre1=c(round(freq0*sp.arbre*length(test[,1])),round(freq0*(1-sp.arbre)*length(test[,1])))
    arbre2=c(round(freq1*(1-se.arbre)*length(test[,1])),round(freq1*se.arbre*length(test[,1])))
    tab2.arbre=data.frame(name,arbre1,arbre2)
    colnames(tab2.arbre)=c(" ","Prédiction 0", "Prédiction 1")
    matarbre=as.matrix(tab2.arbre)
    print(matarbre)})
  
  output$accarbre_kaggle <- renderValueBox({
    valueBox(
      subtitle="Précision",
      mapply(round, mean(pred.arbre()==test[,1]),4),
      icon = icon("check-double"),
      color="navy")})
  
  output$aucarbre_kaggle <- renderValueBox({
    valueBox(
      subtitle="AUC",
      mapply(round, auc(roc(test[,1],as.numeric(pred.arbre()))),4),
      icon = icon("check-double"),
      color="navy")})
  
  
  
  
  
  #random forest sur kaggle
  y.rf1 <- reactive({
    predict(randomForest(train$ser_delinquency~. , data=train, ntree=input$ntree, mtry=round(sqrt(ncol(x))), type=classification), x_test, type="class",s=c(0))
  })
  
  output$matconfurf <- renderTable({ 
    tab.Rforest=as.matrix(table(test[,1], y.rf1()))
    
    #correction de la matrice
    sp.Rforest=tab.Rforest[1]/(tab.Rforest[1]+tab.Rforest[3])
    se.Rforest=tab.Rforest[4]/(tab.Rforest[2]+tab.Rforest[4])
    
    name=c("Réponse 0", "Réponse 1")
    Rforest1=c(round(freq0*sp.Rforest*length(test[,1])),round(freq0*(1-sp.Rforest)*length(test[,1])))
    Rforest2=c(round(freq1*(1-se.Rforest)*length(test[,1])),round(freq1*se.Rforest*length(test[,1])))
    tab2.Rforest=data.frame(name,Rforest1,Rforest2)
    colnames(tab2.Rforest)=c(" ","Prédiction 0", "Prédiction 1")
    matrf=as.matrix(tab2.Rforest)
    print(matrf)})
  
  output$acc.rf <- renderValueBox({
    valueBox(
      subtitle="Précision",
      mapply(round, mean(y.rf1()==test[,1]),4),
      icon = icon("check-double"),
      color="navy")})
  
  output$auc.rf <- renderValueBox({
    valueBox(
      subtitle="AUC",
      mapply(round, auc(roc(test[,1],as.numeric(y.rf1()))),4),
      icon = icon("check-double"),
      color="navy")})
  
  
  
  #boosting sur Kaggle
  y.boosting1 <- reactive({
    predict(boosting(ser_delinquency~., data=train, boos=FALSE, mfinal=input$mfinal, coeflearn="Breiman"), test[,-1])$class})
  
  output$matconfuboost <-renderTable ({
    #matrice de confusion
    tab.boosting=as.matrix(table(test[,1], y.boosting1()))
    
    #correction de la matrice
    sp.boosting=tab.boosting[1]/(tab.boosting[1]+tab.boosting[3])
    se.boosting=tab.boosting[4]/(tab.boosting[2]+tab.boosting[4])
    
    name=c("Réponse 0", "Réponse 1")
    boosting1=c(round(freq0*sp.boosting*length(test[,1])),round(freq0*(1-sp.boosting)*length(test[,1])))
    boosting2=c(round(freq1*(1-se.boosting)*length(test[,1])),round(freq1*se.boosting*length(test[,1])))
    tab2.boosting=data.frame(name, boosting1, boosting2)
    colnames(tab2.boosting)=c(" ","Prédiction 0", "Prédiction 1")
    matb=as.matrix(tab2.boosting)
    print(matb)})
  
  output$acc.boost <- renderValueBox({
    valueBox(
      subtitle="Précision",
      mapply(round, mean(y.boosting1()==test[,1]),4),
      icon = icon("check-double"),
      color="navy")})
  
  output$auc.boost <- renderValueBox({
    valueBox(
      subtitle="AUC",
      mapply(round, auc(roc(test[,1],as.numeric(y.boosting1()))),4),
      icon = icon("check-double"),
      color="navy")})
  
  
  
  
  #SVM sur Kaggle
  output$matconfsvm_kaggle <- renderTable({
    svm=table(test[,1], as.factor(pred_svm))
    
    #correction de la matrice
    sp.svm=svm[1]/(svm[1]+svm[3])
    se.svm=svm[4]/(svm[2]+svm[4])
    
    name=c("réponse 0", "réponse 1")
    matsvm1=c(round(freq0*sp.svm*length(test[,1])),round(freq0*(1-sp.svm)*length(test[,1])))
    matsvm2=c(round(freq1*(1-se.svm)*length(test[,1])),round(freq1*se.svm*length(test[,1])))
    tab2.svm=data.frame(name, matsvm1, matsvm2)
    colnames(tab2.svm)=c(" ","Prédiction 0", "Prédiction 1")
    print(as.matrix(tab2.svm))})
  
  output$accsvm_kaggle <- renderValueBox({
    valueBox(
      subtitle="Précision",
      mapply(round, mean(pred_svm==test[,1]),4),
      icon = icon("check-double"),
      color="navy")   })
  
  output$aucsvm_kaggle <- renderValueBox({
    valueBox(
      subtitle="AUC", width=8,
      mapply(round, auc(roc(test[,1], as.numeric(pred_svm))),4),
      icon = icon("check-double"),
      color="navy")  })
  

  
  output$comparaison2 <- renderTable({(compare2)})
  
}
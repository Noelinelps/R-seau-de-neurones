source('dependencies.R')

#setwd("D:/Documents/FAC/M2/semestre 10/- Projets/reseau de neurones/FINAL")


#données credit
base=read_excel("data/credit.xls")
base=base[,-1]


#traitement des doublons
base=base[-which(duplicated(base)),]

#traitement des valeurs manquantes
vmanq=is.na(base)
#pas de valeur manquante

attach(base)
#traitement des valeurs étranges
base$X3 <- ifelse(X3 == 1, 1, ifelse(X3==2, 2, ifelse(X3==3, 3, 4)))
base$X4 <- ifelse(X4 == 1, 1, ifelse(X4==2, 2, 3))

# graphiques variable cible
g0=ggplot(base, aes(x=Y)) +
  geom_bar(col = "black", fill="#000035") +
  #ggtitle("Répartition de la variable cible") +
  xlab("Défaut") +
  ylab("Effectifs") + theme(panel.background = element_blank())

g1= ggplot(base, aes(X1)) + 
  geom_histogram(breaks=c(0,50000,100000,150000,200000,250000,300000,350000,400000,450000,500000), col="white") + 
  xlab("Montant du crédit") +
  ylab("Effectifs") + theme(panel.background = element_blank())

g2= ggplot(base, aes(x=X2)) +
  geom_bar(col = "white") +
  xlab("Sexe") +
  ylab("Effectifs") + theme(panel.background = element_blank())

g3= ggplot(base, aes(x=X3)) +
  geom_bar(col = "white") +
  xlab("Education") +
  ylab("Effectifs") + theme(panel.background = element_blank())

g4= ggplot(base, aes(x=X4)) +
  geom_bar(col = "white") +
  xlab("Statut marital") +
  ylab("Effectifs") + theme(panel.background = element_blank())

g5= ggplot(base, aes(X5)) + 
  geom_histogram(breaks=c(20,25,30,35,40,45,50,55,60,65,70), col="white") + 
  xlab("Age") +
  ylab("Effectifs") + theme(panel.background = element_blank())

g6= ggplot(base, aes(X6)) + 
  geom_bar( col="white") + 
  xlab("Septembre") +
  ylab("Effectifs") + theme(panel.background = element_blank())

g7= ggplot(base, aes(X7)) + 
  geom_bar( col="white") + 
  xlab("Août") +
  ylab("Effectifs") + theme(panel.background = element_blank())

g8= ggplot(base, aes(X8)) + 
  geom_bar( col="white") + 
  xlab("Juillet") +
  ylab("Effectifs") + theme(panel.background = element_blank())

g9= ggplot(base, aes(X9)) + 
  geom_bar( col="white") + 
  xlab("Juin") +
  ylab("Effectifs") + theme(panel.background = element_blank())

g10= ggplot(base, aes(X10)) + 
  geom_bar( col="white") + 
  xlab("Mai") +
  ylab("Effectifs") + theme(panel.background = element_blank())

g11= ggplot(base, aes(X11)) + 
  geom_bar( col="white") + 
  xlab("Avril") +
  ylab("Effectifs") + theme(panel.background = element_blank())

g12= ggplot(base, aes(X12)) + 
  geom_histogram(breaks=c(-100000,-50000,0,50000,100000,150000,200000,250000,300000,350000,400000,450000), col="white") + 
  xlab("Septembre") +
  ylab("Effectifs") + theme(panel.background = element_blank())

g13= ggplot(base, aes(X13)) + 
  geom_histogram(breaks=c(-100000,-50000,0,50000,100000,150000,200000,250000,300000,350000,400000,450000), col="white") + 
  xlab("Août") +
  ylab("Effectifs") + theme(panel.background = element_blank())

g14= ggplot(base, aes(X14)) + 
  geom_histogram(breaks=c(-100000,-50000,0,50000,100000,150000,200000,250000,300000,350000,400000,450000), col="white") + 
  xlab("Juillet") +
  ylab("Effectifs") + theme(panel.background = element_blank())

g15= ggplot(base, aes(X15)) + 
  geom_histogram(breaks=c(-100000,-50000,0,50000,100000,150000,200000,250000,300000,350000,400000,450000), col="white") + 
  xlab("Juin") +
  ylab("Effectifs") + theme(panel.background = element_blank())

g16= ggplot(base, aes(X16)) + 
  geom_histogram(breaks=c(-100000,-50000,0,50000,100000,150000,200000,250000,300000,350000,400000,450000), col="white") + 
  xlab("Mai") +
  ylab("Effectifs") + theme(panel.background = element_blank())

g17= ggplot(base, aes(X17)) + 
  geom_histogram(breaks=c(-10000,-50000,0,50000,100000,150000,200000,250000,300000,350000,400000,450000), col="white") + 
  xlab("Avril") +
  ylab("Effectifs") + theme(panel.background = element_blank())

g18= ggplot(base, aes(X18)) + 
  geom_histogram(breaks=c(0,1000,2000,3000,4000,5000,6000,7000,8000,9000,10000),col="white") + 
  xlab("Septembre") +
  ylab("Effectifs") + theme(panel.background = element_blank())

g19= ggplot(base, aes(X19)) + 
  geom_histogram(breaks=c(0,1000,2000,3000,4000,5000,6000,7000,8000,9000,10000),col="white") + 
  xlab("Août") +
  ylab("Effectifs") + theme(panel.background = element_blank())

g20= ggplot(base, aes(X20)) + 
  geom_histogram(breaks=c(0,1000,2000,3000,4000,5000,6000,7000,8000,9000,10000),col="white") + 
  xlab("Juillet") +
  ylab("Effectifs") + theme(panel.background = element_blank())

g21= ggplot(base, aes(X21)) + 
  geom_histogram(breaks=c(0,1000,2000,3000,4000,5000,6000,7000,8000,9000,10000),col="white") + 
  xlab("Juin") +
  ylab("Effectifs") + theme(panel.background = element_blank())

g22= ggplot(base, aes(X22)) + 
  geom_histogram(breaks=c(0,1000,2000,3000,4000,5000,6000,7000,8000,9000,10000),col="white") + 
  xlab("Mai") +
  ylab("Effectifs") + theme(panel.background = element_blank())

g23= ggplot(base, aes(X23)) + 
  geom_histogram(breaks=c(0,1000,2000,3000,4000,5000,6000,7000,8000,9000,10000),col="white") + 
  xlab("Avril") +
  ylab("Effectifs") + theme(panel.background = element_blank())

gra=plot_grid(g0, ncol=1, nrow=1)
gra1=plot_grid(g1,g2,g3,g4,g5, ncol = 5, nrow = 1)
#title="Historique des paiements passés"
gra2=plot_grid(g6,g7,g8,g9,g10,g11, ncol = 3, nrow = 2)
#title="Montant des factures"
gra3=plot_grid(g12,g13,g14,g15,g16,g17, ncol = 3, nrow = 2)
#title="Montant des factures"
gra4=plot_grid(g18,g19,g20,g21,g22,g23, ncol = 3, nrow = 2)


#standardisation
dataf=as.data.frame(scale(base[,-24]))
base2=cbind(base[,24],dataf)

set.seed(3456)

#construction des échantillons apprentissage et test
detach(base)
attach(base2)

indice = sort(createDataPartition(Y, p = .7,list = FALSE,times = 1))

train_b=base2[indice,]
test_b=base2[-indice,]


#préparation des données
xb <- as.matrix(train_b[,-1])
yb <- as.matrix(train_b[, 1])
xb_test <- as.matrix(test_b[,-1])





#données kaggle
data=read.table(file = "data/kaggle.txt" , header=F, sep=" ",col.names=c("id", "ser_delinquency", "RU_unsecuredlines", "age", "nb_3059days", "debt_ratio", "income_month", "nb_creditloan", "nb_90days", "nb_realEloanlines", "nb_6089days", "nb_dependents"))

data=data[,-1]

#traitement des doublons
data=data[-which(duplicated(data)),]


#traitement des valeurs manquantes
# remplacement des valeurs manquantes par la moyenne
data$income_month[is.na(data$income_month)] = round(median(data$income_month, na.rm = TRUE))
data$nb_dependents[is.na(data$nb_dependents)] = round(median(data$nb_dependents, na.rm = TRUE))

attach(data)
data$ser_delinquency=as.factor(ser_delinquency)
# graphiques variable cible
graph1=ggplot(data, aes(x=ser_delinquency)) +
  geom_bar(col = "black", fill="#000035") +
  #ggtitle("Répartition de la variable cible") +
  xlab("Défaut") +
  ylab("Effectifs") + theme(panel.background = element_blank())
#evenementt rare

#rééchantillonnage
data1 <- SmoteClassif(ser_delinquency~. ,data, list("0" = 0.6, "1" = 4) )

graph2=ggplot(data1, aes(x=ser_delinquency)) +
  geom_bar(col = "black", fill="#000035") +
  #ggtitle("Répartition de la variable cible") +
  xlab("Défaut") +
  ylab("Effectifs") + theme(panel.background = element_blank())
#rééchantillonage fait

# graphiques sur les autres variables
p1= ggplot(data1, aes(age)) + 
  geom_histogram(breaks=c(20,25,30,35,40,45,50,55,60,65,70,75,80,85,90), col="white") + 
  xlab("Age") +
  ylab("Effectifs") + theme(panel.background = element_blank())

p2= ggplot(data1, aes(debt_ratio)) + 
  geom_histogram(breaks=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1), col="white") + 
  xlab("Dette") +
  ylab("Effectifs") + theme(panel.background = element_blank())

p3= ggplot(data1, aes(income_month)) + 
  geom_histogram(breaks=c(0,2000,4000,6000,8000,10000,12000,14000,16000,18000,20000), col="white") + 
  xlab("Revenu") +
  ylab("Effectifs") + theme(panel.background = element_blank())

p4= ggplot(data1, aes(nb_creditloan)) + 
  geom_histogram(breaks=c(0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30), col='white') + 
  xlab("Nb crédits") +
  ylab("Effectifs") + theme(panel.background = element_blank())

p5= ggplot(data1, aes(nb_dependents)) + 
  geom_histogram(breaks=c(0,1,2,3,4,5), col='white') + 
  xlab("Nb dépendances") +
  ylab("Effectifs") + theme(panel.background = element_blank())

p6= ggplot(data1, aes(nb_realEloanlines)) + 
  geom_histogram(breaks=c(0,1,2,3,4,5), col='white') + 
  xlab("Nb crédits ouverts") +
  ylab("Effectifs") + theme(panel.background = element_blank())

p7= ggplot(data1, aes(RU_unsecuredlines)) + 
  geom_histogram(breaks=c(0,0.2,0.4,0.6,0.8,1), col='white') + 
  xlab("Pourcentage") +
  ylab("Effectifs") + theme(panel.background = element_blank())

p8= ggplot(data1, aes(nb_3059days)) + 
  geom_histogram(breaks=c(0,1,2,3,4,5), col="white") + 
  xlab("30-59 jours") +
  ylab("Effectifs") + theme(panel.background = element_blank())

p9= ggplot(data1, aes(nb_6089days)) + 
  geom_histogram(breaks=c(0,1,2,3,4), col="white") + 
  xlab("60-89 jours") +
  ylab("Effectifs") + theme(panel.background = element_blank())

p10= ggplot(data1, aes(nb_90days)) + 
  geom_histogram(breaks=c(0,1,2,3,4), col="white") + 
  xlab("Plus de 90 jours") +
  ylab("Effectifs") + theme(panel.background = element_blank())


graph=plot_grid(graph1,graph2, ncol=2, nrow=1)
graph3=plot_grid(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10, ncol = 5, nrow = 2)


#standardisation des données
dataframe=as.data.frame(scale(data1[,-1]))
data2=cbind(data1[,1],dataframe)



set.seed(3456)

#construction des échantillons apprentissage et test
names(data2)[1]="ser_delinquency"
detach(data)
attach(data2)

trainIndex = sort(createDataPartition(ser_delinquency, p = .7,
                                      list = FALSE,
                                      times = 1))

train=data2[trainIndex,]
test=data2[-trainIndex,]


#préparation des données
x <- as.matrix(train[,-1])
y <- as.matrix(train[, 1])
x_test <- as.matrix(test[,-1])

#fréquence nécessaire pour modifier les matrices à cause du réechnatillonage
freq1= sum(ser_delinquency==1)/(sum(ser_delinquency==1)+sum(ser_delinquency==0))
freq0=1-freq1


#pour le pmc - crédit
redu = sort(createDataPartition(train_b$Y, p = .1,
                                list = FALSE,
                                times = 1))

train2_b=train_b[redu,]


#pour le svm - réduction de train 
reduce = sort(createDataPartition(train$ser_delinquency, p = .3,
                                      list = FALSE,
                                      times = 1))

train2=train[reduce,]
model_svm = svm(ser_delinquency~., data=data.frame(train2), type="C-classification")
pred_svm= predict(model_svm, test[,-1])




# comparaison kaggle
m_regk=glm(as.factor(ser_delinquency)~.,data=train, family=binomial(link = logit))
y_regk=ifelse(predict(m_regk, data.frame(x_test), type="response")<0.3, 0,1)
y_regkt=ifelse(predict(m_regk, data.frame(x), type="response")<0.3, 0,1)
acc.reg=round(mean(y_regk==test[,1]),4)
auc.reg=round(auc(roc(test[,1], as.numeric(y_regk))),4)
acc.regt=round(mean(y_regkt==train[,1]),4)
auc.regt=round(auc(roc(train[,1], as.numeric(y_regkt))),4)

m_arbrek=rpart(ser_delinquency~.,train, method="class", control = rpart.control(cp = 0.001))
y_arbrek=predict(m_arbrek, data.frame(x_test), type="class")
y_arbrekt=predict(m_arbrek, data.frame(x), type="class")
acc.arbre=round(mean(y_arbrek==test[,1]),4)
auc.arbre=round(auc(roc(test[,1], as.numeric(y_arbrek))),4)
acc.arbret=round(mean(y_arbrekt==train[,1]),4)
auc.arbret=round(auc(roc(train[,1], as.numeric(y_arbrekt))),4)

m_randfk=randomForest(ser_delinquency~. , data=train, ntree=10, mtry=round(sqrt(ncol(x))), type=classification)
y_randfk=predict(m_randfk, x_test, type="class",s=c(0))
y_randfkt=predict(m_randfk, x, type="class",s=c(0))
acc.randf=round(mean(y_randfk==test[,1]),4)
auc.randf=round(auc(roc(test[,1], as.numeric(y_randfk))),4)
acc.randft=round(mean(y_randfkt==train[,1]),4)
auc.randft=round(auc(roc(train[,1], as.numeric(y_randfkt))),4)

m_book=boosting(ser_delinquency~., data=train, boos=FALSE, mfinal=10, coeflearn="Breiman")
y_book=predict(m_book, test[,-1])$class
y_bookt=predict(m_book, train[,-1])$class
acc.boo=round(mean(y_book==test[,1]),4)
auc.boo=round(auc(roc(test[,1], as.numeric(y_book))),4)
acc.boot=round(mean(y_bookt==train[,1]),4)
auc.boot=round(auc(roc(train[,1], as.numeric(y_bookt))),4)

pred_svm2= predict(model_svm, train[,-1])
acc.svm=round(mean(pred_svm==test[,1]),4)
auc.svm=round(auc(roc(test[,1], as.numeric(pred_svm))),4)
acc.svmt=round(mean(pred_svm2==train[,1]),4)
auc.svmt=round(auc(roc(train[,1], as.numeric(pred_svm2))),4)

m_rbfk=rbf(x, as.numeric(y), size=11, linOut=F)
y_rbfk=ifelse(predict(m_rbfk, x_test)<0.3, 0,1)
y_rbfkt=ifelse(predict(m_rbfk, x)<0.3, 0,1)
acc.rbf=round(mean(y_rbfk==test[,1]),4)
auc.rbf=round(auc(roc(test[,1], as.numeric(y_rbfk))),4)
acc.rbft=round(mean(y_rbfkt==train[,1]),4)
auc.rbft=round(auc(roc(train[,1], as.numeric(y_rbfkt))),4)


compare2=data.frame(
  Méthodes = c("Regression logisitique",
               "RBF - 11 neurones cachés",
               "Arbre de décision - 0.001",
               "Random Forest - 10",
               "Boosting - 10",
               "SVM"),
  AUC.test = as.character(c(auc.reg,
                            auc.rbf,
                            auc.arbre,
                            auc.randf,
                            auc.boo,
                            auc.svm)),
  AUC.train = as.character(c(auc.regt,
                             auc.rbft,
                             auc.arbret,
                             auc.randft,
                             auc.boot,
                             auc.svmt)),
  ACCURACY.test = as.character(c(acc.reg,
                                 acc.rbf,
                                 acc.arbre,
                                 acc.randf,
                                 acc.boo,
                                 acc.svm)),
  ACCURACY.train = as.character(c(acc.regt,
                                  acc.rbft,
                                  acc.arbret,
                                  acc.randft,
                                  acc.boot,
                                  acc.svmt)),
  stringsAsFactors = FALSE)

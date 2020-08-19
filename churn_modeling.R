#1. citirea si analiza setului de date
churn=read.csv("Churn Modeling.csv")
dim(churn)
head(churn)
summary(churn)
str(churn)
attach(churn)

mytable=table(Geography)
levels(Geography)=c(1,2,3)
churn['Geography']=as.numeric(Geography)
str(Gender)
Gender=factor(Gender, levels=c("Female","Male"),labels=c(0,1))
Gender=as.integer(as.character(Gender))
churn['Gender']=Gender
churn['Gender']=as.numeric(Gender)
str(churn)
#SELECTAM DOAR ATRIBUTELE NUMERICE UTILE
churn=churn[,c(4,5,6,7,8,9,10,11,12,13,14)]
write.csv(churn,"retea_full.csv")

cor(churn)

cor()
#verificarea valorilor lipsa
any(is.na(churn))


#pie char variabila Geography?? trebuie sa arate procentul, nu valoarea
library(plotrix)
mytable=table(churn$Geography)
x=c(5014,2509,2477)
labels=c("France","Germany","Spain")
pct=round(x/sum(x)*100)
lbls=paste(pct)
lbls=paste(lbls,"%",sep="")
pie3D(x,labels=lbls, main="Pie Chart of Geography")
legend("topright",c("France","Germany","Spain"),cex=0.7, fill=rainbow(length(x)))



#pie chart variabila gender
mytable=table(churn$Gender)
x=c(4543,5457)
pct=round(x/sum(x)*100)
lbls=paste(pct)
lbls=paste(lbls,"%",sep="")
pie3D(x,labels=lbls, main="Pie Chart of Gender")
legend("topright",c("Female","Male"),cex=0.7, fill=rainbow(length(x)))

#pie chart of exited

mytable=table(churn$Exited)
x=c(7963,2037)
pct=round(x/sum(x)*100)
lbls=paste(pct)
lbls=paste(lbls,"%",sep="")
pie3D(x,labels=lbls, main="Pie Chart of Exited")
legend("topright",c("Non-Churn","Churn"),cex=0.7, fill=rainbow(length(x)))

#pie chart of Is Active Member
mytable=table(churn$IsActiveMember)
x=c(4849,5151)
pct=round(x/sum(x)*100)
lbls=paste(pct)
lbls=paste(lbls,"%",sep="")
pie3D(x,labels=lbls, main="Pie Chart of IsActiveMember")
legend("topright",c("Inactive","Active"),cex=0.7, fill=rainbow(length(x)))

#transformarea variabilelor Gender si Geography in numeric

mytable=table(Geography)
levels(Geography)=c(1,2,3)
churn['Geography']=as.numeric(Geography)
str(Gender)
Gender=factor(Gender, levels=c("Female","Male"),labels=c(0,1))
Gender=as.integer(as.character(Gender))
churn['Gender']=Gender
churn['Gender']=as.numeric(Gender)
str(churn)
#SELECTAM DOAR ATRIBUTELE NUMERICE UTILE
churn=churn[,c(4,5,6,7,8,9,10,11,12,13,14)]
cor(churn)

#balansarea setului de date-ENN
library(unbalanced)
sqrt(1000)
#metoda ENN
data_ENN=ubENN(churn,Exited,k=3, verbose = TRUE)
data_ENN=cbind(data_ENN$X,data_ENN$Y)
mytable=table(data_ENN$Exited)
lbls=paste(names(mytable),"\n", mytable,sep="")
pie3D(mytable,labels=lbls, main="Pie Chart of Exited after ENN\n (with sample sizes)")

#APLICAM METODA enn de cate ori este nevoie-2 ORI SI SE STERG 77+10
data_ENN=ubENN(data_ENN,data_ENN$`data_ENN$Y`, k=3, verbose=TRUE)
data_ENN=cbind(data_ENN$X,data_ENN$Y)
mytable=table(data_ENN$Exited)
lbls=paste(names(mytable),"\n", mytable,sep="")
pie3D(mytable,labels=lbls, main="Pie Chart of Exited after ENN\n (with sample sizes)")

x=c(7876,2037)
pct=round(x/sum(x)*100)
lbls=paste(pct)
lbls=paste(lbls,"%",sep="")
pie3D(x,labels=lbls, main="Pie Chart of Exited after ENN")
legend("topright",c("Non-Churn","Churn"),cex=0.7, fill=rainbow(length(x)))

#metoda NCL
data_NCL=ubNCL(churn,Exited,k=3, verbose = TRUE)
data_NCL=cbind(data_NCL$X,data_NCL$Y)
mytable=table(data_NCL$Exited)
lbls=paste(names(mytable),"\n", mytable,sep="")
pie3D(mytable,labels=lbls, main="Pie Chart of Exited after NCL\n (with sample sizes)")


x=c(3041,2037)
pct=round(x/sum(x)*100)
lbls=paste(pct)
lbls=paste(lbls,"%",sep="")
pie3D(x,labels=lbls, main="Pie Chart of Exited after NCL")
legend("topright",c("Non-Churn","Churn"),cex=0.7, fill=rainbow(length(x)))

#NCL+tomek
data_ncl_t=ubTomek(data_NCL,data_NCL$`data_NCL$Y`,verbose = TRUE)
data_ncl_t=cbind(data_ncl_t$X,data_ncl_t$Y)
mytable=table(data_ncl_t$Exited)
lbls=paste(names(mytable),"\n", mytable,sep="")
pie3D(mytable,labels=lbls, main="Pie Chart of Exited after NCL+Tomek\n (with sample sizes)")

x=c(2359,2037)
pct=round(x/sum(x)*100)
lbls=paste(pct)
lbls=paste(lbls,"%",sep="")
pie3D(x,labels=lbls, main="Pie Chart of Exited after NCL and Tomek Links")
legend("topright",c("Non-Churn","Churn"),cex=0.7, fill=rainbow(length(x)))

#salvam si scriem intr-un document setul de date rezultat dupa balansare
str(data_ncl_t)
churn_final=data.frame(data_ncl_t$CreditScore,data_ncl_t$Geography,data_ncl_t$Gender,data_ncl_t$Age,data_ncl_t$Tenure,data_ncl_t$Balance,data_ncl_t$NumOfProducts,data_ncl_t$HasCrCard,data_ncl_t$IsActiveMember,data_ncl_t$EstimatedSalary,data_ncl_t$Exited)
names(churn_final)=c("CreditScore","Geography","Gender","Age","Tenure","Balance","NumOfProducts","HasCrCard","IsActiveMember","EstimatedSalary","Exited")
write.csv(churn_final,'churn_final.csv')
data=read.csv("churn_final.csv")
cor(data)
attach(data)
model=lm(Exited~Age+Gender+NumOfProducts+Tenure+Balance+CreditScore+IsActiveMember+Geography+HasCrCard)



#normalizarea varibilelor CreditScore, balance, estimatedSalary
min_max_norm=function(x){
  (x-min(x))/(max(x)-min(x))
}
atribute_n=as.data.frame(lapply(data[,c(2,3,4,5,6,7,8,11)],min_max_norm))
data=data.frame(atribute_n, data[,c(9,10,12)])
#scriem baza de date in format final, dupa scalarizare
write.csv(data,"churn_final.csv")

#-----------------------------------------------------------------------------------------------------------------------------------------------

#ANALIZA REGRESIEI LOGISTICE
#instalarea pachetelor
library(dplyr)
library(mlbench)
library(broom)
library(visreg)
library(margins)
library(rcompanion)
library(ROCR)
#IMPARTIM SETUL DE DATE IN TRAIN SI TEST
n=nrow(data)
n_train=round(0.70*n)
set.seed(123)
train_indices=sample(1:n,n_train)
train=data[train_indices,]
test=data[-train_indices,]

#verificam dimensiunile pt train si test
dim(train)
dim(test)

#modelul de regresie logistica
model_logistic=glm(Exited~.,data=train,family="binomial")
summary(model_logistic)

coef(model_logistic)
# efecte marginale
efect_logit=margins(model_logistic)
summary(efect_logit)


#indicatori de potrivire
rcompanion::nagelkerke(model_logistic)

#dominance analysis
library(dominanceanalysis)
dapres=dominanceAnalysis(model_logistic)

plot(dapres,which.graph="general",fit.function="r2.m")


#probability plot
visreg(model_logistic,"Age", scale="response",rug=2,xlab="Age",ylab="Probability of Churn")


visreg(model_logistic,"CreditScore", scale="response",rug=2,xlab="CreditScore",ylab="Probability of Churn")


#calculam marginal efects
library(margins)
effects_logit_dia=margins(model_logistic)
summary(effects_logit_dia)
plot(effects_logit_dia)

#matricea de confuzie
pred=predict(model_logistic,test,type="response")
predicted=round(pred)
tab=table(Predicted=predicted, Reference=test$Exited)
#acuratetea modelului-0.69

#cut values vs Accuracy

pred.rocr=ROCR::prediction(pred,test$Exited)
eval=performance(pred.rocr,"acc")
plot(eval)


#
max=which.max(slot(eval,"y.values")[[1]])
acc=slot(eval,"y.values")[[1]][max]
cut=slot(eval,"x.values")[[1]][max]
print(c(Accuracy=acc),Cutoff=cut)

#curba ROC
perf_rocr=performance(pred.rocr,measure="auc",x.measure="cutoff")
perf_rocr@y.values[[1]]=round(perf_rocr@y.values[[1]],digits=4)
perf.tpr.fpr.rocr=performance(pred.rocr,"tpr","fpr")
plot(perf.tpr.fpr.rocr,colorize=T, main=paste("AUC:", (perf_rocr@y.values)))

abline(a=0,b=1)


#-------------------------------------------------------------------------------------------

#RETELE NEURONALE ARTIFICIALE
#importarea pachetelor
library(neuralnet)
library(nnet)
library(plyr)
library(ROCR)
library(dplyr)
library(mlbench)
library(broom)
library(visreg)
library(margins)
library(rcompanion)
library(ROCR)
data=read.csv("churn_final.csv")


data=data[,-1]

#impartirea setului in date de testare si date de antrenament
set.seed(222)
ind=sample(2,nrow(data),replace=TRUE,prob=c(0.7,0.3))
train=data[ind==1,]
test=data[ind==2,]

#modelul
set.seed(333)
library(neuralnet)
fit<-neuralnet(Exited~Age+NumOfProducts+Balance+Geography+IsActiveMember+Gender+CreditScore,
               data=train,
               hidden=3,
               err.fct="sse",
               algorithm = "rprop+",
               linear.output = FALSE,
               threshold = 0.3)
#print(fit)
plot(fit, intercept = FALSE,show.weights = FALSE)
#attributes(fit)
summary(fit)
#fit$result.matrix
plot(fit, intercept = TRUE, show.weights=TRUE)



#prediction
output=compute(fit,test[,-11])
head(output$net.result)
#confuse matrix
p1=output$net.result
pred1=ifelse(p1>0.5,1,0)
#DATAFRAME CU VALORILE OBSERVATE 
confusion_matrix=data.frame(pred1,train$Exited)
write.csv(confusion_matrix,"confusion.csv")
tab1=table(pred1,test$Exited, dnn=c("Predicted","Observed"))
tab1
#acuratetea modelului
sum(diag(tab1))/sum(tab1)
#eroarea de casificare
1-sum(diag(tab1))/sum(tab1)
#

pred=predict(fit,test,type="response")
predicted=round(pred)
tab=table(Predicted=predicted, Reference=test$Exited)
#acuratetea modelului-0.69

#cut values vs Accuracy

pred.rocr=ROCR::prediction(pred,test$Exited)
eval=performance(pred.rocr,"acc")
plot(eval)


#
max=which.max(slot(eval,"y.values")[[1]])
acc=slot(eval,"y.values")[[1]][max]
cut=slot(eval,"x.values")[[1]][max]
print(c(Accuracy=acc),Cutoff=cut)

#curba ROC
perf_rocr=performance(pred.rocr,measure="auc",x.measure="cutoff")
perf_rocr@y.values[[1]]=round(perf_rocr@y.values[[1]],digits=4)
perf.tpr.fpr.rocr=performance(pred.rocr,"tpr","fpr")
plot(perf.tpr.fpr.rocr,colorize=T, main=paste("AUC:", (perf_rocr@y.values)))

abline(a=0,b=1)

#importanta factorilor
library(NeuralNetTools)
garson(fit, bar_plot=FALSE)




#-------------------------------------------------------------------------------------------
#ARBORI DE DECIZIE

library(tree)
data=read.csv("arbori_churn.csv")
data$Gender=factor(data$Gender, labels = c("Female","Male"))
data$Geography=factor(data$Geography, labels=c("France","Germany","Spain"))
data$Exited=factor(data$Exited)
data$IsActiveMember=factor(data$IsActiveMember)
data$HasCrCard=factor(data$HasCrCard)

data=data[,-1]
str(data)

tree_churn=tree(Exited~.,data=data)
summary(tree_churn)

plot(tree_churn)
text(tree_churn,pretty=0)
tree_churn

#impartim setul de date in train si test
set.seed(101)
train=sample(1:nrow(data),3500)
test=data[-train,]
tree_train=tree(Exited~., data, subset=train)
plot(tree_train)
tree_train
summary(tree_train)
text(tree_train,pretty=0)
str(train)

tree.pred=predict(tree_train,data[-train, ],type="class")
tree.pred
#matricea de confuzie
with(data[-train,],table(tree.pred, Exited))


#acuratetea arborelui-0.74
(320+335)/(330+80+161+335)

#folosim cv.tree pentru erorile de clasificare
cv=cv.tree(tree_train,FUN=prune.misclass)
cv
plot(cv)

prune.tree=prune.misclass(tree_train,best=6)
plot(prune.tree)
text(prune.tree)
summary(prune.tree)
library(pROC)
tree.pred=predict(tree_train,data[-train,], type='prob')
with(data[-train,],table(tree.pred, Exited))
#acuratetea pt best=6-0.75
(392+286)/(392+286+89+129)

#curba ROC

predicted=round(tree.pred)


pred.rocr=ROCR::prediction(tree.pred,test$Exited)
eval=performance(pred.rocr,"acc")
plot(eval)


#
max=which.max(slot(eval,"y.values")[[1]])
acc=slot(eval,"y.values")[[1]][max]
cut=slot(eval,"x.values")[[1]][max]
print(c(Accuracy=acc),Cutoff=cut)

#curba ROC
perf_rocr=performance(pred.rocr,measure="auc",x.measure="cutoff")
perf_rocr@y.values[[1]]=round(perf_rocr@y.values[[1]],digits=4)
perf.tpr.fpr.rocr=performance(pred.rocr,"tpr","fpr")
plot(perf.tpr.fpr.rocr,colorize=T, main=paste("AUC:", (perf_rocr@y.values)))

abline(a=0,b=1)
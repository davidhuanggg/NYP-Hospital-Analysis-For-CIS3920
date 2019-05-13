library(ISLR)
library(class)
library(MASS)
View(nyp_data)
attach(nyp_data)
summary(nyp_data)
severity_of_illness1 = rep(1,length(`APR_Severity_of_Illness_Code`))
severity_of_illness1[`APR_Severity_of_Illness_Code`>median(`APR_Severity_of_Illness_Code`)]=4
boxplot(Mean_Charge~severity_of_illness1, data=nyp_data, main="Mean Charge vs Severity of Illness")

boxplot(Median_Cost~severity_of_illness1, data=nyp_data, main = "Median Cost vs Severity of Illness")

boxplot(Mean_Cost~severity_of_illness1, data = nyp_data, main = "Mean Cost vs Severity of Illness")

boxplot(Median_Charge~severity_of_illness1, data = nyp_data, main = "Median Charge vs Severity of Illness")

train = (Median_Cost>Mean_Cost)
test = !train
nyp_data.train = nyp_data[train, ]
nyp_data.test = nyp_data[test, ]
severity_of_illness1.test = severity_of_illness1[test]
lda.fit = lda(severity_of_illness1~Mean_Charge + Median_Cost + Mean_Cost + Median_Charge,data = nyp_data,subset=train)
lda.pred=predict(lda.fit,nyp_data.test)
mean(lda.pred$class == severity_of_illness1.test)

qda.fit = qda(severity_of_illness1~Mean_Charge + Median_Cost + Mean_Cost + Median_Charge,data = nyp_data,subset=train)
qda.pred=predict(qda.fit,nyp_data.test)
mean(qda.pred$class == severity_of_illness1.test)

train.B = cbind(Mean_Charge, Median_Cost, Mean_Cost, Median_Charge)[train,]
test.B = cbind(Mean_Charge, Median_Cost, Mean_Cost, Median_Charge)[test, ]
train.severity_of_illness1 = severity_of_illness1[train]
set.seed(1)
pred.knn = knn(train.B, test.B, train.severity_of_illness1, k =4)
table(pred.knn,severity_of_illness1.test)
mean(pred.knn == severity_of_illness1.test)

pred.knn = knn(train.B, test.B, train.severity_of_illness1, k =10)
table(pred.knn,severity_of_illness1.test)
mean(pred.knn == severity_of_illness1.test)

pred.knn = knn(train.B, test.B, train.severity_of_illness1, k =100)
table(pred.knn,severity_of_illness1.test)
mean(pred.knn == severity_of_illness1.test)

detach(nyp_data)


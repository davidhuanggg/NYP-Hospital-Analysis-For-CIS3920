library(tree)
library(ISLR)
attach(nyp_data)
view(nyp_data)
High=ifelse(Discharges<=400,"No","Yes")
nyp_data=data.frame(nyp_data,High)
tree.hospitals=tree(High~.-Discharges,nyp_data)
summary(tree.hospitals)
plot(tree.hospitals)
text(tree.hospitals,pretty=0)
set.seed(2)
train=sample(1:nrow(nyp_data),1822)
nyp_data.test=nyp_data[-train,]
High.test=High[-train]
tree.hospitals=tree(High~.-Discharges,nyp_data,subset=train)
tree.pred=predict(tree.hospitals,nyp_data.test,type="class") 
table(tree.pred,High.test)
(1149+3)/1822

set.seed(3)
cv.nyp_data=cv.tree(tree.hospitals,FUN=prune.misclass)
names(cv.nyp_data)
cv.nyp_data

par(mfrow=c(1,2))
plot(cv.nyp_data$size,cv.nyp_data$dev,type="b")
plot(cv.nyp_data$k,cv.nyp_data$dev,type="b")

prune.nyp_data=prune.misclass(tree.hospitals,best=5)
plot(prune.nyp_data)
text(prune.nyp_data,pretty=0)
tree.pred=predict(prune.nyp_data,nyp_data.test,type="class")
table(tree.pred,High.test)
(1152+2)/1822

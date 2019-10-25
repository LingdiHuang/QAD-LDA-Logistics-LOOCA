library(ISLR)
library(ggplot2)
library(corrplot)
library(dplyr)
library(MASS)
library(class)
library(boot)
data("Weekly")
#10.(a) The year and volumn have relatively high correlation
summary(Weekly)
corrplot(cor(Weekly[,1:8]))

#10.(b) Lag2 is statistically significant
glm_fit1 = glm(Weekly$Direction~Weekly$Lag1+Weekly$Lag2+Weekly$Lag3+Weekly$Lag4+Weekly$Lag5+Weekly$Volume,family = binomial)
summary(glm_fit1)

#10.(c) The confusion matrix tells us the prediction accuracy rate is 56.1%
glm_fit1.prob = predict(glm_fit1,type = 'response')
glm_fit1.pred = rep("Down",length(glm_fit1.prob))
glm_fit1.pred[glm_fit1.prob>0.5] = "Up"
table(glm_fit1.pred,Weekly$Direction)

#10(d) Now, the new predict accuracy rate is 62.5%, which is better than last model.
train = Weekly%>%filter(.,Year>=1990&Year<=2008)
test = Weekly%>%filter(Year>2008)
glm_fit2 = glm(data = train, Direction~Lag2,family = "binomial")
summary(glm_fit2)
new = data.frame(Lag2 = test$Lag2)
glm_fit2.prob = predict(glm_fit2,newdata = new,type = 'response')
glm_fit2.pred = rep("Down",length(glm_fit2.prob))
glm_fit2.pred[glm_fit2.prob>0.5] = "Up"
table(glm_fit2.pred,test$Direction)

#10(e) Using linear discriminant analysis generates similar result with logistic regression
lda.fit<-lda(data = train, Direction~Lag2,family=binomial)
lda.pred = predict(lda.fit,newdata = new,type = 'response')
table(lda.pred$class,test$Direction)
mean(lda.pred$class==test$Direction)


#10(f) The accuracy from QDA is 58.65%, which is lower than previous methods. Also, this model only predict the upward trends.
qda.fit<-qda(data = train, Direction~Lag2,family=binomial)
qda.pred = predict(qda.fit,newdata = new,type = 'response')
table(qad.pred$class,test$Direction)
mean(qda.pred$class==test$Direction)

#10(g) The accuracy of KNN is 50%, which is the lowest so far. 
set.seed(1)
knn.train = data.frame(Lag2 = train$Lag2)
knn.test = data.frame(Lag2 = test$Lag2)
knn.pred = knn(train = knn.train,test = knn.test,cl = train$Direction,k=1)
table(knn.pred,test$Direction)
mean(knn.pred == test$Direction)

#10(h) logistics regression and LAD provide the best result on this data

#10(i) We set k from 2 to 100 to find the best k that has the best result. The result shows when k = 47, the accuracy is 61.54%, which is the best among them.
k = seq(2,100)
result = data.frame(k=numeric(),accuracy=numeric())
for (i in k){
  knn.pred = knn(train = knn.train,test = knn.test,cl = train$Direction,k=i)
  result[i-1,1] = i
  result[i-1,2] = mean(knn.pred == test$Direction)
}
result[result$accuracy==max(result$accuracy),]

#We add Lag4 as a new predictor in the orginal LDA model, the accuracy doesn't change
lda.fit_new<-lda(data = train, Direction~Lag2+Lag4,family='binomial')
lda_new = data.frame(Lag2 = test$Lag2, Lag4 = test$Lag4)
lda.pred_new = predict(lda.fit_new,newdata = lda_new,type = 'response')
table(lda.pred_new$class,test$Direction)
mean(lda.pred_new$class==test$Direction)

#We did the same thing with QDA model, the accuracy becomes 53.85%, which is lower than before.
qda.fit_new<-qda(data = train, Direction~Lag2+Lag4,family=binomial)
qda_new = data.frame(Lag2 = test$Lag2, Lag4 = test$Lag4)
qda.pred_new = predict(qda.fit_new,newdata = qda_new,type = 'response')
table(qda.pred_new$class,test$Direction)
mean(qda.pred_new$class==test$Direction)


#11
data(Auto)
#(a)
mpg01 <- rep(0, length(Auto$mpg))
mpg01[Auto$mpg > median(Auto$mpg)] <- 1
Auto = data.frame(Auto, mpg01)

#(b) cylinders, displacement, horsepower and weight seem to be the useful features.
corrplot(cor(Auto[,-9]), method="square")

#(c) We choose 70% of data to be train data set, and 30% of it to be test data set.
set.seed(11)
train_sample = sample(392,274,replace = FALSE)
auto_train = Auto[train_sample,]
auto_test = Auto[-train_sample,]

#(d) Using LDA model gives us the test error rate 12.71%
lda.fit_auto<-lda(data = auto_train, mpg01~cylinders+displacement+horsepower+weight,family='binomial')
lda_new_auto = data.frame(cylinders = auto_test$cylinders,displacement = auto_test$displacement,horsepower = auto_test$horsepower,weight = auto_test$weight)
lda.pred_auto = predict(lda.fit_auto,newdata = lda_new_auto,type = 'response')
table(lda.pred_auto$class,auto_test$mpg01)
mean(lda.pred_auto$class!=auto_test$mpg01)


#(e) Using QDA gives us the same error rate.
qda.fit_auto<-qda(data = auto_train, mpg01~cylinders+displacement+horsepower+weight,family=binomial)
qda.pred_auto = predict(qda.fit_auto,newdata = lda_new_auto,type = 'response')
table(qda.pred_auto$class,auto_test$mpg01)
mean(qda.pred_auto$class!=auto_test$mpg01)

#(f) Using logistic regression, we get 13.56% error rate.
glm.fit_auto = glm(data = auto_train,mpg01~cylinders+displacement+horsepower+weight,family=binomial)
glm_fit_auto.prob = predict(glm.fit_auto,newdata = lda_new_auto,type = 'response')
glm_fit_auto.pred = rep(0,length(glm_fit_auto.prob))
glm_fit_auto.pred[glm_fit_auto.prob>0.5] = 1
table(glm_fit_auto.pred,auto_test$mpg01)
mean(glm_fit_auto.pred!=auto_test$mpg01)


#(g) Based on the result, when k = 6 or 12, the error rate is the lowest which is 11.86%
auto_knn_test = data.frame(cylinders = auto_test$cylinders,displacement = auto_test$displacement,horsepower = auto_test$horsepower,weight = auto_test$weight)
auto_knn_train = data.frame(cylinders = auto_train$cylinders,displacement = auto_train$displacement,horsepower = auto_train$horsepower,weight = auto_train$weight)
k = seq(1,100)
result_auto = data.frame(k=numeric(),error_rate=numeric())
for (i in k){
  knn.pred_auto = knn(train = auto_knn_train,test = auto_knn_test,cl = auto_train$mpg01,k=i)
  result_auto[i,1] = i
  result_auto[i,2] = mean(knn.pred_auto != auto_test$mpg01)
}
result_auto[result_auto$error_rate==min(result_auto$error_rate),]

#Chpater 5
#2
#(a) 1-(1/n) because bootstrap is sample with replacement, 1/n represents jth observation is the first bootstrap then 1-1/n means it's not.

#(b) It's still 1-(1/n)

#(c) The probability of jth sample is not the first, or second bootstrap is 1-1/n. So the result would be:
#(1-1/n)(1-1/n).....(1-1/n) = (1-1/n)^n

#(d) 1 - (1-1/5)^5 = 0.67232

#(e) 1 - (1-1/100)^100 = 0.6339677

#(f) 1 - (1-1/10000)^10000 = 0.632139

#(g) The probability that the jth observation is not in the bootstrap converges to 0.632.
x<-1:100000
plot(x = x, y = 1-(1-1/x)^x, xlab = 'bootstrap number', ylab = 'probability', main = 'jth observation not in bootstrap sample')

#(h) That means when n becomes infinite, 1+(x/n)^n converges to e^x
store=rep(NA, 10000)
for(i in 1:10000){
  store[i]=sum(sample(1:100, rep=TRUE)==4)>0 }
mean(store)

#5
#(a)
set.seed(5)
data("Default")
glm_fit_default = glm(Default$default~Default$income+Default$balance,family = binomial)
summary(glm_fit_default)

#(b) We have a 2.63% test error rate
train_sample_default = sample(10000,7000,replace = FALSE)
default_train = Default[train_sample_default,]
default_test = Default[-train_sample_default,]

glm_fit2_default = glm(data = default_train,default~income+balance,family = binomial)
summary(glm_fit2_default)

newdata_default = default_test[,-1]
default_probs = predict(glm_fit2_default, newdata = newdata_default, type = "response")
pred_glm_default <- rep("No", length(default_probs))
pred_glm_default[default_probs > 0.5] <- "Yes"

mean(pred_glm_default != default_test$default)

#(c) The test error rate is from 2.34% to 2.78%
#The first sample
train_sample_default = sample(10000,7000,replace = FALSE)
default_train = Default[train_sample_default,]
default_test = Default[-train_sample_default,]
glm_fit2_default = glm(data = default_train,default~income+balance,family = binomial)
newdata_default = default_test[,-1]
default_probs = predict(glm_fit2_default, newdata = newdata_default, type = "response")
pred_glm_default = rep("No", length(default_probs))
pred_glm_default[default_probs > 0.5] <- "Yes"
mean(pred_glm_default != default_test$default)

#The second sample
train_sample_default = sample(10000,7000,replace = FALSE)
default_train = Default[train_sample_default,]
default_test = Default[-train_sample_default,]
glm_fit2_default = glm(data = default_train,default~income+balance,family = binomial)
newdata_default = default_test[,-1]
default_probs = predict(glm_fit2_default, newdata = newdata_default, type = "response")
pred_glm_default = rep("No", length(default_probs))
pred_glm_default[default_probs > 0.5] <- "Yes"
mean(pred_glm_default != default_test$default)

#The third sample
train_sample_default = sample(10000,7000,replace = FALSE)
default_train = Default[train_sample_default,]
default_test = Default[-train_sample_default,]
glm_fit2_default = glm(data = default_train,default~income+balance,family = binomial)
newdata_default = default_test[,-1]
default_probs = predict(glm_fit2_default, newdata = newdata_default, type = "response")
pred_glm_default <- rep("No", length(default_probs))
pred_glm_default[default_probs > 0.5] <- "Yes"
mean(pred_glm_default != default_test$default)

#(d) The test error rate is 2.53%. It seems like dummy variable doesn't reduce the test error rate.
glm_fit3_default = glm(data = default_train,default~income+balance+student,family = binomial)
newdata_default = default_test[,-1]
default_probs_full = predict(glm_fit3_default, newdata = newdata_default, type = "response")
pred_glm_default_full = rep("No", length(default_probs_full))
pred_glm_default_full[default_probs_full > 0.5] = "Yes"
mean(pred_glm_default_full != default_test$default)


#6(a) The standard errors of the two variables are 0.000004985 and 0.0002274
set.seed(6)
glm_fit6_default = glm(data= Default,default~income+balance,family = binomial)
summary(glm_fit6_default)

#(b)
boot.fn <- function(data, index) {
  fit <- glm(default ~ income + balance, data = data, family = "binomial", subset = index)
  return (coef(fit))
}

#(c)
boot(Default, boot.fn, 1000)

#(d) The result is similar.

#7
#(a)
glm_fit_weekly = glm(data = Weekly, Direction~Lag1+Lag2,family = binomial)
summary(glm_fit_weekly)

#(b)
glm_fit_weekly_b = glm(data = Weekly[-1,], Direction~Lag1+Lag2,family = binomial)
summary(glm_fit_weekly_b)

#(c) This observation was not correctly classified
glm_fit7_weekly.prob = predict(glm_fit_weekly_b,newdata = Weekly[1,-9],type = 'response')
glm_fit7_weekly.pred = rep("Down",length(glm_fit7_weekly.prob))
glm_fit7_weekly.pred[glm_fit7_weekly.prob>0.5] = "Up"
table(glm_fit7_weekly.pred,Weekly[1,9])

#(d)
predict_result = data.frame(result = numeric())
for (i in seq(1,1089)){
  fit<-glm(data = Weekly[-i,], Direction~Lag1+Lag2,family = binomial)
  glm.prob = predict(fit,newdata = Weekly[i,-9],type = 'response')
  glm.pred = rep("Down",length(glm.prob))
  glm.pred[glm.prob>0.5] = "Up"
  if (glm.pred == Weekly[i,9])
    predict_result[i,] = 0
  else
    predict_result[i,] = 1
}
head(predict_result)


#(e) The LOOCV estimate test error rate is 45%
mean(predict_result$result)









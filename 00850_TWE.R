###SVR+DE using 00850
rm(list=ls())
library(e1071)
library(DEoptim) #for svr

library(forecast)
library(TSA)
library(tseries)
library(data.table) #for ARIMA

library(ggplot2) #for plot

library(caret)

oo85o <- read.csv("C:/Users/leoko/OneDrive/Desktop/IM_PROJECT/main/da.csv")[ ,1:10]
attach(oo85o)
as.ts(oo85o) #the main data, tranform as time series data

#the first part for svr: show the original constant input represent
train <- data.frame(opening[1:300],max[1:300],min[1:300],s_p500[1:300],TP2330[1:300],TP2308[1:300],TP2454[1:300],TP2414[1:300])
in0 <- svm(opening~max+min+s_p500+TP2330+TP2308+TP2454+TP2414,data=train,scale=TRUE,kernel="radial")
out0 <- predict(in0,train,decision.values = TRUE)
price <- opening[1:300]
out0 <- unname(out0)
# names(price) <- c(1:300)
MAPE0 <- mean(abs(price-out0)/price)*100
MAPE0 < 0.1 #浪喷家,MAPE < 10%家钡
#MAPE0
out0

# u <- union(out0, price)
# t <- table(factor(out0, u), factor(price, u))
# cm <- confusionMatrix(t)

table(factor(out0, levels=min(test):max(test)), factor(test, levels=min(test):max(test)))

cm <- confusionMatrix(factor(out0, levels=1:2), factor(price, levels=1:2))
cm
cm<- confusionMatrix(out0, price) #<ask>

index <- c(1:300)
y_pred_1 <- as.numeric(out0)
original <- as.numeric(price)
ggplot()+
  geom_line(aes(x = index, y = original, colour = 'Original'), size=0.8)+
  geom_line(aes(x = index, y = y_pred_1, colour = 'Prediction'), size=0.8)+
  labs(x = 'Date', y = 'Price', title = 'prediction comparison')+
  scale_colour_manual(values = c('black','blue'))

t <- table(out0, price) 
t
 #TODO:璸衡タ絋瞯

#the second part, compute the three constants
al=c(0.01,1,0.001)
au=c(1,3000,0.2) #DEoptim畉だ┮惠
SVR=function(a){
  m=svm(opening~max+min+s_p500+TP2330+TP2308+TP2454+TP2414,
        data=train,scale=TRUE,
        kernel="radial",
        gamma=a[1],
        cost=a[2],
        tolerance=0.0001,
        epsilon=a[3])
  new0=predict(m, train)
  MAPE=mean(abs(opening[1:300]-new0[1:300])/opening[1:300])*100
} #硂娩そΑ琌璶ノ50Ω畉だㄓ琵程gamma,cost,epsilon把计Μ滥,眔程ㄎ把计,莱ノsvrそΑい
set.seed(12345)
out1=DEoptim(SVR, lower=al, upper=au, DEoptim.control(NP=30, itermax = 50, F = 0.8, CR = 0.5))
gamma=out1$optim$bestmem[1]
cost=out1$optim$bestmem[2]
epsilon=out1$optim$bestmem[3]
plot(out1) #50Ω畉だ筁祘,把计簍て筁祘 

cost =2642.832
gamma =0.689565
epsilon =0.001000764
#璶琩把计癸svr家

###
#眔程瞶稱svr把计盿svr modelい暗箇代
new=read.csv("C:/Users/leoko/OneDrive/Desktop/IM_PROJECT/main/da.csv")[ ,1:10]
attach(new)
as.ts(new)
#<拜>ぐ或硂娩代刚临琌盿场戈秈莱赣璶蛤training8:2だ盾?
test=data.frame(opening[1:300],max[1:300],min[1:300],s_p500[1:300],TP2330[1:300],TP2308[1:300],TP2454[1:300],TP2414[1:300])
mnew=svm(opening~max+min+s_p500+TP2330+TP2308+TP2454+TP2414,data=train,scale=TRUE,kernel="radial",
         gamma=gamma,cost=cost,tolerance=0.0001,epsilon=epsilon)

m2 <- predict(mnew, test)
p <- opening[1:300]
T_MAPE=mean(abs(p[1:300]-m2[1:300])/p[1:300])*100
T_MAPE<0.1
T_MAPE
m2
#<拜>璶或箇代钡ㄓ30ぱ戈

index <- c(1:300)
y_pred_2 = as.numeric(m2)
original = as.numeric(price)
ggplot()+
  geom_line(aes(x = index, y = original, colour = 'Original'), size=1.2)+
  geom_line(aes(x = index, y = y_pred_2, colour = 'Prediction'), size=0.8)+
  labs(x = 'Date', y = 'Price', title = 'prediction comparison')+
  scale_colour_manual(values = c('black','red'))
#穝箇代瓜


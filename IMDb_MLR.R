Data<-read.csv("C:\\Users\\user\\Downloads\\Movie+Assignment+Data.csv")
View(Data)
data<-Data[,-1]
View(data)
data<-data[,-3]


RNGkind(sample.kind="Rounding")
set.seed(111)
#install.packages("caret")
library(caret)

#Split the data into train and test dataset
training.samples <- createDataPartition(data$Profit, p = 0.7, list = FALSE, groups=2)

train.data  <- data[training.samples, ]

test.data <- data[-training.samples, ]

plot(train.data)

#Correlation analysis on train dataset

attach(train.data)

pairs(train.data) #gives same result as plot(train.data)

install.packages("corrplot")

library(corrplot)

c=cor(train.data)

c

par(mfrow=c(2,2))

corrplot(c) #wherever you see white, basically indicates very low correlation

corrplot(c, method = "square")

corrplot.mixed(c)

corrplot.mixed(c, lower.col = "black")

#Regression analysis on train dataset

model1 <- lm(Profit ~ budget + title_year + Average.IMDb.and.Metacritic + Runtime + VotesU18+ Votes1829+Votes3044+Votes45A, data = train.data)

model1=lm(Profit~., data=train.data) #This is just diff way of doing the above command. This is recommended

summary(model1)

model2=lm(Profit~ budget, data=train.data)
summary(model2)

summary(model2)$coef

predictions <- predict(object=model2, newdata=test.data)  #predict for test dataset

pred=data.frame(predictions,test.data$Profit)


#RMSE (prediction error) (test dataset)
library(caret)
RMSE(predictions, test.data$Profit)

mean(test.data$Profit)

RMSE(predictions, test.data$Profit)/mean(test.data$Profit)
R2(predictions, test.data$Profit)

#Checking model adequacy (optional)

plot(model2)
install.packages("lmtest")
library(lmtest)

bptest(model2)
#heteroscdesticity

shapiro.test(model2$residuals)
#but data doesn't follow normality


View(test.data)



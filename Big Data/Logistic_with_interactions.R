library(tidyverse)
library(imputeTS)
library(car)
library(zoo)
library(data.table)


ferry <- read.csv("train.csv")
Datetime <- paste(ferry$Full.Date,ferry$Scheduled.Departure,sep = ' ')
ferry$p.datetime <- as.POSIXct(Datetime, format = '%d %B %Y %I:%M %p')

test <- read.csv("test.csv")
Datetime2 <- paste(test$Full.Date,test$Scheduled.Departure,sep = ' ')
test$p.datetime <- as.POSIXct(Datetime2, format = '%d %B %Y %I:%M %p')

traffic <- read.csv('traffic.csv')
Datetime3 <- paste(traffic$Day,traffic$Month, traffic$Year, traffic$Hour, 
                   traffic$Minute,sep = ' ')
traffic$p.datetime <- as.POSIXct(Datetime3, format = '%d %m %Y %H %M')
#delete duplicate rows
traffic <- distinct(traffic, p.datetime, .keep_all = TRUE)

vancouver <- read.csv('vancouver.csv')
vancouver$p.datetime <- as.POSIXct(vancouver$Date.Time, format = '%Y-%m-%d %H:%M:%S')

victoria <- read.csv('victoria.csv')
victoria$p.datetime <- as.POSIXct(victoria$Date.Time, format = '%Y-%m-%d %H:%M:%S')


######################################################################

#Fill missing info on Trip Duration
Trip_Duration_NA <- filter(ferry, is.na(ferry$Trip.Duration)==T)
Trip_Duration_NA$Trip <- droplevels(Trip_Duration_NA$Trip)
table(Trip_Duration_NA$Trip)

#Trip to Southern Gulf Islands, destination is unsure but most likely around or within 65mins
ferry$Trip.Duration <- ferry$Trip.Duration %>% replace_na(65)
table(is.na(ferry$Trip.Duration))


#Assign season based on date
year_qtr <- as.yearqtr(as.yearmon(ferry$p.datetime, "%m/%d/%Y") + 1/12)
ferry$Season <- factor(format(year_qtr, "%q"), levels = 1:4, 
                       labels = c("winter", "spring", "summer", "fall"))

######################################################################

library(pracma)
plot(vancouver$Temperature.in.Celsius, type="l", 
     ylab="Temperature(in Celsius)", 
     main="Vancouver Temperature from \n August 1 2016 to March 31 2018")
y <- movavg(vancouver$Temperature.in.Celsius, 100, "w"); lines(y, col = 2)
legend("topright", lty=c(1,1), col=c(1,2), 
       c("Temperature", "Weighted Moving Average"), cex=0.75)


######################################################################

ferry_traffic <- left_join(ferry, traffic, by="p.datetime") %>% 
      select(-c(Day.y, Month.y, Year.y, Hour, Minute, Second, Status, 
                Full.Date)) %>% 
      rename(Day = Day.x, Month = Month.x, Year = Year.x) %>% arrange(p.datetime)


# Merge Van Weather Data
Temp <- full_join(ferry, vancouver, by="p.datetime") %>% 
      select(-c(Day.y, Month.y, Year.y, Hour, Date.Time, Time)) %>% 
      arrange(p.datetime) %>%
      rename(Temperature = Temperature.in.Celsius, 
             Dew.Point.Temperature = Dew.Point.Temperature.in.Celsius, 
             Relative.Humidity = Relative.Humidity.in.Percent, 
             Humidex = Humidex.in.Celsius)
Temp$Temperature <- na_ma(Temp$Temperature, weighting = "linear")
Temp$Dew.Point.Temperature <- na_ma(Temp$Dew.Point.Temperature, weighting = "linear")
Temp$Relative.Humidity <- na_ma(Temp$Relative.Humidity, weighting = "linear")

Merged_train <- filter(Temp, is.na(Scheduled.Departure)==FALSE) 

#Calculate Humidex
for (i in 1:nrow(Merged_train)) {
      if(is.na(Merged_train$Humidex[i])==T){
            Merged_train$Humidex[i] = Merged_train$Temperature[i] + (5/9) * 
                  (6.11 * exp(5417.7530*( 
                        (1/273.16) - 1/(273.15+Merged_train$Dew.Point.Temperature[i]) ) 
                  ) -10 )
      }
}

Merged_train <- Merged_train %>% mutate(Traffic.Ordinal = ferry_traffic$Traffic.Ordinal)
Merged_train$Traffic.Ordinal <- na_ma(Merged_train$Traffic.Ordinal, 
                                      weighting = "linear")


#Merge Vic Weather Data
Temp1 <- full_join(Merged_train, victoria, by="p.datetime") %>% 
      select(-c(Day, Month, Year, Hour, Date.Time, Time, Weather)) %>% 
      arrange(p.datetime) %>% 
      rename(Temperature.Vic = Temperature.in.Celsius, 
             Dew.Point.Temperature.Vic = Dew.Point.Temperature.in.Celsius, 
             Relative.Humidity.Vic = Relative.Humidity.in.Percent, 
             Wind.Direction = Wind.Direction.in.Degrees, 
             Wind.Speed = Wind.Speed.km.per.h, 
             Visibility = Visibility.in.km, 
             Station.Pressure = Station.Pressure.in.kPa)

Temp1$Temperature.Vic <- na_ma(Temp1$Temperature.Vic, weighting = "linear")
Temp1$Dew.Point.Temperature.Vic <- na_ma(Temp1$Dew.Point.Temperature.Vic, 
                                         weighting = "linear")
Temp1$Relative.Humidity.Vic<- na_ma(Temp1$Relative.Humidity.Vic, 
                                    weighting = "linear")
Temp1$Wind.Direction <- na_ma(Temp1$Wind.Direction, weighting = "linear")
Temp1$Wind.Speed <- na_ma(Temp1$Wind.Speed, weighting = "linear")
Temp1$Visibility <- na_ma(Temp1$Visibility, weighting = "linear")
Temp1$Station.Pressure <- na_ma(Temp1$Station.Pressure, weighting = "linear")


Temp1 <- filter(Temp1, is.na(Scheduled.Departure)==FALSE)

Merged_train <- Temp1 %>% select(-Status, -Full.Date) %>% 
      rename(Day = Day.x, Month = Month.x, Year = Year.x)

Merged_train <- Merged_train[, c(1:8, 10:23, 9)]

Merged_train$Traffic.Ordinal <- as.factor(round(Merged_train$Traffic.Ordinal))
Merged_train$Delay.Indicator <- as.factor(Merged_train$Delay.Indicator)


str(Merged_train)


######################################################################

ferry_traffic_test <- left_join(test, traffic, by="p.datetime") %>% 
      select(-c(Day.y, Month.y, Year.y, Hour, Minute, Second, Full.Date)) %>% 
      rename(Day = Day.x, Month = Month.x, Year = Year.x) %>% arrange(p.datetime)


#Full join to fill NAs
Temp2 <- full_join(test, vancouver, by="p.datetime") %>% 
      select(-c(Day.y, Month.y, Year.y, Hour, Date.Time, Time)) %>% arrange(p.datetime) %>%
      rename(Temperature = Temperature.in.Celsius, 
             Dew.Point.Temperature = Dew.Point.Temperature.in.Celsius, 
             Relative.Humidity = Relative.Humidity.in.Percent, 
             Humidex = Humidex.in.Celsius)
Temp2$Temperature <- na_ma(Temp2$Temperature, weighting = "linear")
Temp2$Dew.Point.Temperature <- na_ma(Temp2$Dew.Point.Temperature, weighting = "linear")
Temp2$Relative.Humidity <- na_ma(Temp2$Relative.Humidity, weighting = "linear")

Merged_test <- filter(Temp2, is.na(Scheduled.Departure)==FALSE) 

for (i in 1:nrow(Merged_test)) {
      if(is.na(Merged_test$Humidex[i])==T){
            Merged_test$Humidex[i] = Merged_test$Temperature[i] + (5/9) * 
                  (6.11 * exp(5417.7530*( 
                        (1/273.16) - 1/(273.15+Merged_test$Dew.Point.Temperature[i]) ) 
                  ) -10 )
      }
}


Merged_test <- Merged_test %>% mutate(Traffic.Ordinal = ferry_traffic_test$Traffic.Ordinal)
Merged_test$Traffic.Ordinal <- na_ma(Merged_test$Traffic.Ordinal, weighting = "linear")


Temp3 <- full_join(Merged_test, victoria, by="p.datetime") %>% 
      select(-c(Day, Month, Year, Hour, Date.Time, Time, Weather)) %>% 
      arrange(p.datetime) %>% 
      rename(Temperature.Vic = Temperature.in.Celsius, 
             Dew.Point.Temperature.Vic = Dew.Point.Temperature.in.Celsius, 
             Relative.Humidity.Vic = Relative.Humidity.in.Percent, 
             Wind.Direction = Wind.Direction.in.Degrees, 
             Wind.Speed = Wind.Speed.km.per.h, 
             Visibility = Visibility.in.km, 
             Station.Pressure = Station.Pressure.in.kPa)

Temp3$Temperature.Vic <- na_ma(Temp3$Temperature.Vic, weighting = "linear")
Temp3$Dew.Point.Temperature.Vic <- na_ma(Temp3$Dew.Point.Temperature.Vic, weighting = "linear")
Temp3$Relative.Humidity.Vic<- na_ma(Temp3$Relative.Humidity.Vic, weighting = "linear")
Temp3$Wind.Direction <- na_ma(Temp3$Wind.Direction, weighting = "linear")
Temp3$Wind.Speed <- na_ma(Temp3$Wind.Speed, weighting = "linear")
Temp3$Visibility <- na_ma(Temp3$Visibility, weighting = "linear")
Temp3$Station.Pressure <- na_ma(Temp3$Station.Pressure, weighting = "linear")

Temp3 <- filter(Temp3, is.na(Scheduled.Departure)==FALSE)

Merged_test <- Temp3 %>% select(-Full.Date) %>% rename(Day = Day.x, 
                                                       Month = Month.x, 
                                                       Year = Year.x)

Merged_test$Traffic.Ordinal <- as.factor(round(Merged_test$Traffic.Ordinal))

#Assign season based on date
year_qtr <- as.yearqtr(as.yearmon(Merged_test$p.datetime, "%m/%d/%Y") + 1/12)
Merged_test$Season <- factor(format(year_qtr, "%q"), levels = 1:4, 
                             labels = c("winter", "spring", "summer", "fall"))

Merged_test$Trip.Duration <- Merged_train$Trip.Duration[match(Merged_test$Trip, 
                                                              Merged_train$Trip)]


str(Merged_test)


######################################################################

#Replace Scheduled.Departure time with AM or PM
Merged_train$Scheduled.Departure <- as.factor(gsub("^.*\\s", "", 
                                                   Merged_train$Scheduled.Departure))

#Replace datetime with a range 
Merged_train$p.datetime <- gsub("^.*\\s", "", Merged_train$p.datetime)
#Get rid of :xx:xx
Merged_train$p.datetime <- gsub("\\:+.+$", "", Merged_train$p.datetime)
Merged_train$p.datetime <- as.numeric(Merged_train$p.datetime)
table(Merged_train$p.datetime)

#Turn datetime into a range of time
Merged_train <- Merged_train %>% 
      mutate(time_range = cut(p.datetime, breaks=c(4,7,10,13,16,19,23)))
Merged_train <- Merged_train %>% 
      mutate(time_range = recode_factor(time_range, "(4,7]" = "5-7", 
                                        "(7,10]" = "8-10", "(10,13]" = "11-13", 
                                        "(13,16]" = "14-16", "(16,19]" = "17-19", 
                                        "(19,23]" = "20-23"))


#Replace Scheduled.Departure time with AM or PM
Merged_test$Scheduled.Departure <- as.factor(gsub("^.*\\s", "", 
                                                  Merged_test$Scheduled.Departure))

#Replace datetime with a range 
Merged_test$p.datetime <- gsub("^.*\\s", "", Merged_test$p.datetime)
#Get rid of :xx:xx
Merged_test$p.datetime <- gsub("\\:+.+$", "", Merged_test$p.datetime)
Merged_test$p.datetime <- as.numeric(Merged_test$p.datetime)
table(Merged_test$p.datetime)

#Turn datetime into a range of time
Merged_test <- Merged_test %>% 
      mutate(time_range = cut(p.datetime, breaks=c(4,7,10,13,16,19,23)))
Merged_test <- Merged_test %>% 
      mutate(time_range = recode_factor(time_range, "(4,7]" = "5-7", 
                                        "(7,10]" = "8-10", "(10,13]" = "11-13", 
                                        "(13,16]" = "14-16", "(16,19]" = "17-19", 
                                        "(19,23]" = "20-23"))

Merged_train <- select(Merged_train, -p.datetime)
Merged_test <- select(Merged_test, -p.datetime)


######################################################################
str(Merged_train)
str(Merged_test)
setdiff(Merged_train, Merged_test)

Merged_train$Trip.Duration <- as.integer(Merged_train$Trip.Duration)
Merged_test$Trip.Duration <- Merged_train$Trip.Duration[match(Merged_test$Trip, 
                                                              Merged_train$Trip)]

Merged_test$Trip.Duration <- as.integer(Merged_test$Trip.Duration)


#First try full model with all parameters
logi <- glm(Delay.Indicator~., data=Merged_train, family=binomial(link="logit"))
Anova(logi)

#Then we can exclude variables which are not contribute to the model
Merged_train <- Merged_train %>% select(-c(Humidex, Wind.Direction, Year,
                                           Traffic.Ordinal, Temperature.Vic, 
                                           Dew.Point.Temperature.Vic, 
                                           Relative.Humidity.Vic))
Merged_test <- Merged_test %>% select(-c(Humidex, Wind.Direction, Year,
                                         Traffic.Ordinal, Temperature.Vic, 
                                         Dew.Point.Temperature.Vic, 
                                         Relative.Humidity.Vic))


######################################################################

#By trying any interactions between variables one at a time and run Anova() to check their
#contribution to the model. Our final model is obtained below.

# 0.7187
#This model is big, takes at least 5 mins to run
logi <- glm(Delay.Indicator ~ Vessel.Name + Trip + Day + Month +
                    Day.of.Month + Temperature + I(Temperature^2) + 
                  Dew.Point.Temperature + Relative.Humidity + Wind.Speed + 
                  Visibility + Station.Pressure + time_range + 
                  Visibility:Wind.Speed:Station.Pressure + Temperature:time_range + 
                  Trip:time_range + Trip:Day + 
                  Vessel.Name:time_range + Trip:time_range:Day,
            data=Merged_train, family=binomial(link="logit"))

#This takes long time to run (~15+ mins)
# Anova(logi)


prediction <- predict(logi, newdata = Merged_test, type = "response")

range(prediction)
hist(prediction, breaks=50)

table(prediction>0.5)
prediction[prediction>0.5]

final_result <- data.frame("ID" = Merged_test$ID, "Delay.Indicator" = prediction)
final_result <- final_result %>% arrange(ID)


write.csv(final_result, file = "final_result.csv", row.names = FALSE)




######################################################################


library(rpart)
library(rpart.plot)
library(randomForest)

RF <- randomForest(Delay.Indicator~., data=Merged_train, nodesize=49)

#From OOB matrix, we can see that classification error for 1(Delay) is pretty high. 
#Heavily misclassify the chance of being delay. 
RF

#Plot indicates after around 50 trees they all become a straight line, 
#OOB errors stay constantly. Usually, we need further investigation on number of trees 
#after 500 if the error rate is unsteady throughout the graph. In our case, it is probably
#because the method itself does not work well for the data. 
plot(RF)

#mtry=6 provides the lowest OOB error rate. But changing this does not improve the model, 
#prediction is still poor. 
tuneRF(Merged_train[,-16], Merged_train[,16], stepFactor=1.5, plot=TRUE)

RF <- randomForest(Delay.Indicator~., data=Merged_train, nodesize=49, 
                   ntree=100, mtry=6)
RF



######################################################################


library(pROC)
# Compute AUC for predicting Class with the variable CreditHistory.Critical
f1 = roc(Class ~ CreditHistory.Critical, data=training) 
plot(f1, col="red")
## 
## Call:
## roc.formula(formula = Class ~ CreditHistory.Critical, data = training)
## 
## Data: CreditHistory.Critical in 180 controls (Class Bad) < 420 cases (Class Good).
## Area under the curve: 0.5944
library(ROCR)
# Compute AUC for predicting Class with the model
prob <- predict(mod_fit_one, newdata=testing, type="response")
pred <- prediction(prob, testing$Class)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf)
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]






      

setwd("E:/SFU/2019Fall/STAT440/Module2 Weather Data")

rm(list = ls())

library(tidyverse)
library(sqldf)
library(ggplot2)
library(imputeTS)
library(zoo)
library(data.table)
library(car)
train <- read.csv("train.csv", sep=",", na.strings="NaN")
test <- read.csv("test.csv")

#For filtering
train$index <- 1:nrow(train)


train$YEAR <- as.factor(train$YEAR)

ggplot(train, aes(YEAR, TEMPERATURE, color=YEAR)) + geom_boxplot() + ylim(-10, 5)


ggplot(train, aes(YEAR, WINDDIR, color=YEAR)) + geom_boxplot()
ggplot(train, aes(YEAR, WINDSPEED, color=YEAR)) + geom_boxplot()
ggplot(train, aes(YEAR, TEMPERATURE, color=YEAR)) + geom_boxplot()
ggplot(train, aes(YEAR, DEWPOINT, color=YEAR)) + geom_boxplot()
ggplot(train, aes(YEAR, PRESSURE, color=YEAR)) + geom_boxplot()


table(is.na(train$WINDDIR)); table(is.na(train$WINDSPEED)); table(is.na(train$TEMPERATURE))
table(is.na(train$DEWPOINT)); table(is.na(train$PRESSURE))


##############################################################################
#Push Forward (MAE: ~0.258)

weather <- train[7:11]
str(weather)
plot(weather$WINDDIR[1:500], type="l")

weather <- weather %>% fill(WINDDIR, WINDSPEED, TEMPERATURE, DEWPOINT, PRESSURE)
weather$WINDDIR[1] <- weather$WINDDIR[2]

#Result
Result <- data.frame(ID=test$ID, Value=NA)

#Detect test set ID
row <- as.numeric(gsub("\\-{1}.*", "", Result$ID))
col <- as.numeric(gsub("^.*\\-{1}", "", Result$ID))

for(j in 1:nrow(Result)){
    Result$Value[j] <- weather[row[j], col[j]]
}

table(is.na(Result$Value))
range(Result$Value)

write.csv(Result, file = "Result.csv", row.names = FALSE)


##############################################################################
#Mean Interpolation(Not completed) (MAE: 0.23454)

#Result
Result <- data.frame(ID=test$ID, Value=NA)

#Divide test set ID into row and column
row <- as.numeric(gsub("\\-{1}.*", "", Result$ID))
col <- as.numeric(gsub("^.*\\-{1}", "", Result$ID))


position <- as.data.frame(cbind(row, col))
position <- position %>% arrange(row)
position$row <- as.integer(position$row)

#
length(unique(position$row))

#208 duplicates in row, we'll fill those missings later 
pos <- position %>% distinct(row, .keep_all = TRUE)


weather <- train %>% select(WINDDIR, WINDSPEED, TEMPERATURE, DEWPOINT, PRESSURE, index)


indicator <- as.numeric() #indicator for testing
obs <- as.numeric() #for testing
which.col <- as.numeric()
j <- 0

#This loop probably takes ~8mins to run
for(i in 1:nrow(weather)){
    #Whenever i is matching w/ our interested rows
    if(i %in% pos$row){
        j <- j + 1    #j indicates the rows of position  
        Bindex <- 0 #Backward index
        Findex <- 0 #Forward index
        
        # we know "weather[i, position$col[j]]" is missing, 
        #then try to find whether the numbers above and below are missing
        
        if(is.na(weather[i-1, pos$col[j]])==FALSE){
            if(is.na(weather[i+1, pos$col[j]])==FALSE){
                #fill NA w/ an average value by the numbers above and below
                weather[i, pos$col[j]] <-
                    mean(c(weather[i-1, pos$col[j]], 
                           weather[i+1, pos$col[j]]))
                obs[j] <- i
                indicator[j] <- 1       #1 means both above and below are nonmissing
                which.col[j] <- pos$col[j]
            }else if(is.na(weather[i+1, pos$col[j]])==TRUE){
                Findex <- i + 1
                while(is.na(weather[Findex, pos$col[j]])==TRUE){
                    Findex <- Findex + 1 
                }
                weather[i, pos$col[j]] <- mean(c(weather[i-1, pos$col[j]], 
                                                 weather[Findex, pos$col[j]]))
                obs[j] <- i
                indicator[j] <- 2       #2 means one of above or below is missing
                which.col[j] <- pos$col[j]
            }
        }
        if(is.na(weather[i-1, pos$col[j]])==TRUE){
            if(is.na(weather[i+1, pos$col[j]])==FALSE){
                Bindex <- i - 1
                while(is.na(weather[Bindex, pos$col[j]])==TRUE){
                    Bindex <- Bindex - 1 
                }
                weather[i, pos$col[j]] <- mean(c(weather[Bindex, pos$col[j]], 
                                                 weather[i+1, pos$col[j]]))
                obs[j] <- i
                indicator[j] <- 2
                which.col[j] <- pos$col[j]
            }
        }
        if(is.na(weather[i-1, pos$col[j]])==TRUE & 
           is.na(weather[i+1, pos$col[j]])==TRUE){
            Bindex <- i - 1
            Findex <- i + 1
            while(is.na(weather[Bindex, pos$col[j]])==TRUE){
                # +/- 5 width
                if(Bindex==(i-6)){
                    break
                }
                Bindex <- Bindex - 1 
            }
            while(is.na(weather[Findex, pos$col[j]])==TRUE){
                if(Findex==(i+6)){
                    break
                }
                Findex <- Findex + 1 
            }
            weather[i, pos$col[j]] <- mean(c(weather[Bindex, pos$col[j]], 
                                             weather[Findex, pos$col[j]]))
            obs[j] <- i
            if(is.na(mean(c(weather[Bindex, pos$col[j]], 
                            weather[Findex, pos$col[j]])))==TRUE){
                indicator[j] <- 3       #3 means missing is not filled
            }else{
                indicator[j] <- 4       #4 means missing is filled  
            }
            which.col[j] <- pos$col[j]
        }
    }
}


test_obs1 <- as.data.frame(cbind(obs, which.col, indicator))

table(test_obs1$indicator)


test_obs <- test_obs1 %>% rename(row=obs, col=which.col) %>% select(-indicator)

diff_obs <- sqldf('SELECT * FROM position EXCEPT SELECT * FROM pos')


#Run this again to fill the rest missings
indicator <- as.numeric() 
obs <- as.numeric() 
which.col <- as.numeric()
j <- 0

for(i in 1:nrow(weather)){
    #Whenever i is matching w/ our interested rows
    if(i %in% diff_obs$row){
        j <- j + 1    #j indicates the rows of position  
        Bindex <- 0 #Backward index
        Findex <- 0 #Forward index
        
        if(is.na(weather[i-1, diff_obs$col[j]])==FALSE){
            if(is.na(weather[i+1, diff_obs$col[j]])==FALSE){
                #fill NA w/ an average value by the numbers above and below
                weather[i, diff_obs$col[j]] <-
                    mean(c(weather[i-1, diff_obs$col[j]], 
                           weather[i+1, diff_obs$col[j]]))
                obs[j] <- i
                indicator[j] <- 1       #1 means both above and below are nonmissing
                which.col[j] <- diff_obs$col[j]
            }else if(is.na(weather[i+1, diff_obs$col[j]])==TRUE){
                Findex <- i + 1
                while(is.na(weather[Findex, diff_obs$col[j]])==TRUE){
                    Findex <- Findex + 1 
                }
                weather[i, diff_obs$col[j]] <- mean(c(weather[i-1, diff_obs$col[j]], 
                                                      weather[Findex, diff_obs$col[j]]))
                obs[j] <- i
                indicator[j] <- 2       #2 means one of above or below is missing
                which.col[j] <- diff_obs$col[j]
            }
        }
        if(is.na(weather[i-1, diff_obs$col[j]])==TRUE){
            if(is.na(weather[i+1, diff_obs$col[j]])==FALSE){
                Bindex <- i - 1
                while(is.na(weather[Bindex, diff_obs$col[j]])==TRUE){
                    Bindex <- Bindex - 1 
                }
                weather[i, diff_obs$col[j]] <- mean(c(weather[Bindex, diff_obs$col[j]], 
                                                      weather[i+1, diff_obs$col[j]]))
                obs[j] <- i
                indicator[j] <- 2
                which.col[j] <- diff_obs$col[j]
            }
        }
        if(is.na(weather[i-1, diff_obs$col[j]])==TRUE & 
           is.na(weather[i+1, diff_obs$col[j]])==TRUE){
            Bindex <- i - 1
            Findex <- i + 1
            while(is.na(weather[Bindex, diff_obs$col[j]])==TRUE){
                # +/- 5 width
                if(Bindex==(i-6)){
                    break
                }
                Bindex <- Bindex - 1 
            }
            while(is.na(weather[Findex, diff_obs$col[j]])==TRUE){
                if(Findex==(i+6)){
                    break
                }
                Findex <- Findex + 1 
            }
            weather[i, diff_obs$col[j]] <- mean(c(weather[Bindex, diff_obs$col[j]], 
                                                  weather[Findex, diff_obs$col[j]]))
            obs[j] <- i
            if(is.na(mean(c(weather[Bindex, diff_obs$col[j]], 
                            weather[Findex, diff_obs$col[j]])))==TRUE){
                indicator[j] <- 3       #3 means missing is not filled
            }else{
                indicator[j] <- 4       #4 means missing is filled  
            }
            which.col[j] <- diff_obs$col[j]
        }
    }
}

test_obs2 <- as.data.frame(cbind(obs, which.col, indicator))
table(test_obs2$indicator)

#Push forward on the rest of the missings 
weather <- weather %>% select(-index) %>% fill(WINDDIR, WINDSPEED, TEMPERATURE, 
                                               DEWPOINT, PRESSURE)
weather$WINDDIR[1] <- weather$WINDDIR[2]

for(j in 1:nrow(Result)){
    Result$Value[j] <- weather[row[j], col[j]]
}

table(is.na(Result$Value))
range(Result$Value)

hist(Result$Value, xlim=c(-5,5), breaks=60)

write.csv(Result, file = "Result.csv", row.names = FALSE)



##############################################################################

#Moving Averages and Mean within A Range (MAE:0.19)

weather <- train %>% select(WINDDIR, WINDSPEED, TEMPERATURE, DEWPOINT, PRESSURE, 
                            USAF, index)

#Result
Result <- data.frame(ID=test$ID, Value=NA)

#Divide test set ID into row and column
row <- as.numeric(gsub("\\-{1}.*", "", Result$ID))
col <- as.numeric(gsub("^.*\\-{1}", "", Result$ID))


position <- as.data.frame(cbind(row, col))
position <- position %>% arrange(row)
position$row <- as.integer(position$row)

#
length(unique(position$row))

#208 duplicates in row, we'll fill those missings later 
pos <- position %>% distinct(row, .keep_all = TRUE)


indicator <- as.numeric() #indicator for testing
obs <- as.numeric() #for testing
which.col <- as.numeric()
j <- 0

for(i in 1:nrow(weather)){
    #Whenever i is matching w/ our interested rows
    if(i %in% pos$row){
        j <- j + 1 
        Bindex <- i #Backward index
        Findex <- i #Forward index
        
        # +/- 3 width
        while(Bindex > (i-6)){
            Bindex <- Bindex - 1 
        }
        while (Findex < (i+6)) {
            Findex <- Findex + 1
        }
        
        #Nbd not the same station
        if(length(unique(weather[Bindex:Findex, "USAF"])) > 1){
            indicator[j] <- 0
            obs[j] <- i
            which.col[j] <- pos$col[j]
        }else{
            if(is.na(as.numeric(table(is.na(weather[Bindex:Findex, pos$col[j]]))["FALSE"]))){
                indicator[j] <- 2
                obs[j] <- i
                which.col[j] <- pos$col[j]
            }else if(as.numeric(table(is.na(weather[Bindex:Findex, pos$col[j]]))["FALSE"]) >= 2){
                temp <- weather[Bindex:Findex, pos$col[j]]
                temp <- na_ma(temp, weighting="exponential", k=12)
                
                weather[Bindex:Findex, pos$col[j]] <- temp
                
                indicator[j] <- 1
                obs[j] <- i
                which.col[j] <- pos$col[j]
            }else{
                indicator[j] <- 3
                obs[j] <- i
                which.col[j] <- pos$col[j]
            }
        }
    }
}

# unique(weather[7950:7960, "USAF"])

table(indicator)

test_obs1 <- as.data.frame(cbind(obs, which.col, indicator))


test_obs <- test_obs1 %>% rename(row=obs, col=which.col) %>% select(-indicator)

diff_obs <- sqldf('SELECT * FROM position EXCEPT SELECT * FROM pos')


#Run this again for rest of the rows
indicator <- as.numeric() #indicator for testing
obs <- as.numeric() #for testing
which.col <- as.numeric()
j <- 0
for(i in 1:nrow(weather)){
    #Whenever i is matching w/ our interested rows
    if(i %in% diff_obs$row){
        j <- j + 1 
        Bindex <- i #Backward index
        Findex <- i #Forward index
        
        # +/- 3 width
        while(Bindex > (i-6)){
            Bindex <- Bindex - 1 
        }
        while (Findex < (i+6)) {
            Findex <- Findex + 1
        }
        
        #Nbd not the same station
        if(length(unique(weather[Bindex:Findex, "USAF"])) > 1){
            indicator[j] <- 0
            obs[j] <- i
            which.col[j] <- diff_obs$col[j]
        }else{
            if(is.na(as.numeric(table(is.na(weather[Bindex:Findex, diff_obs$col[j]]))["FALSE"]))){
                indicator[j] <- 2
                obs[j] <- i
                which.col[j] <- diff_obs$col[j]
            }else if(as.numeric(table(is.na(weather[Bindex:Findex, diff_obs$col[j]]))["FALSE"]) >= 2){
                temp <- weather[Bindex:Findex, diff_obs$col[j]]
                temp <- na_ma(temp, weighting="exponential", k=12)
                
                weather[Bindex:Findex, diff_obs$col[j]] <- temp
                
                indicator[j] <- 1
                obs[j] <- i
                which.col[j] <- diff_obs$col[j]
            }else{
                # temp <- weather[Bindex:Findex, diff_obs$col[j]]
                # temp_noNA <- which(is.na(temp)=="FALSE") #index
                # weather[i, diff_obs$col[j]] <- temp[temp_noNA]
                indicator[j] <- 3
                obs[j] <- i
                which.col[j] <- diff_obs$col[j]
            }
        }
    }
}

test_obs2 <- as.data.frame(cbind(obs, which.col, indicator))
table(test_obs2$indicator)


test_tot <- rbind(test_obs1, test_obs2)
test_tot <- test_tot %>% arrange(obs)

# indicator0 <- filter(test_tot, indicator==0)



#rest of the missings 
X <- weather %>% select(-index, -USAF)


for(j in 1:nrow(Result)){
    Result$Value[j] <- weather[row[j], col[j]]
}

X1=na.locf(X, fromLast=TRUE)
X2=na.locf(X, na.rm=FALSE)
X2$WINDDIR[1] <- X2$WINDDIR[2] 

for(k in 1:nrow(Result)){
    if(is.na(Result$Value[k])==T){
        #This is wrong
        Result$Value[k] <- (X1[row[k], col[k]] + X1[row[k+2], col[k]] +
                                X1[row[k+3], col[k]] + X1[row[k+4], col[k]] +
                                X1[row[k+5], col[k]] + X1[row[k+6], col[k]] +
                                X2[row[k], col[k]] + X2[row[k-2], col[k]] +
                                X2[row[k-3], col[k]] + X2[row[k-4], col[k]] +
                                X2[row[k-5], col[k]] + X2[row[k-6], col[k]])/12
    }
}

table(is.na(Result$Value))

range(Result$Value)

write.csv(Result, file = "Result.csv", row.names = FALSE)


##############################################################################

#Regression 

weather_noNA <- na.omit(train)
weather_noNA <- weather_noNA %>% select(WINDDIR, WINDSPEED, TEMPERATURE, DEWPOINT, PRESSURE, 
                                        YEAR, MONTH)

var <- weather_noNA %>% select(WINDDIR, WINDSPEED, TEMPERATURE, DEWPOINT, PRESSURE)
cor(var)

str(weather_noNA)
weather_noNA$YEAR <- as.factor(weather_noNA$YEAR)

set.seed(1)
split <- sample(1:nrow(weather_noNA), .7*nrow(weather_noNA))
X.train <- weather_noNA[split,]
X.test <- weather_noNA[-split,]


fit <- lm(WINDSPEED~TEMPERATURE+PRESSURE+YEAR+MONTH, data=X.train)
summary(fit)

vif(fit)

set.seed(1)
split <- sample(1:nrow(weather), .7*nrow(weather))
X.train <- weather[split,]
X.test <- weather[-split,]

pred <- predict(fit, newdata=X.test)
mean(abs(pred-X.test$WINDDIR))

#################################################
library(glmnet)
X <- data.matrix(weather_noNA[, c("WINDSPEED", "TEMPERATURE", "DEWPOINT", "PRESSURE")])

Y <- weather_noNA[, "WINDDIR"]

set.seed(1)
split <- sample(1:nrow(X), .7*nrow(X))
X.train <- X[split,]
X.test <- X[-split,]

Y.train <- Y[split]
Y.test <- Y[-split]


lasso.fit <- glmnet(X, Y, alpha=1)
plot(lasso.fit, xvar="lambda",label=TRUE)
cv.fit <- cv.glmnet(X.train, Y.train, type.measure="mse", alpha=1)
# plot(cv.fit)
best.lambda <- cv.fit$lambda.min
lasso.fit <- glmnet(X, Y, alpha=1, lambda = best.lambda)
coef(lasso.fit)
pred <- predict(lasso.fit, s=best.lambda, newx=X.test)
mean(abs(pred-Y.test))


grid = 10^seq(10, -2, length = 100)
ridge.fit <- glmnet(X, Y, alpha=0, lambda = grid)

plot(ridge.fit, xvar="lambda",label=TRUE)

cv.fit <- cv.glmnet(X.train, Y.train, type.measure="mse", alpha=0)
plot(cv.fit)
cv.fit$lambda.min

ridge.fit <- glmnet(X, Y, alpha=0, lambda = 0.007759567)
coef(ridge.fit)

pred <- predict(ridge.fit, s=0.007759567, newx=X.test)

mean(abs(pred-Y.test))


#################################################

#Regression b/w TEMP and DEW
weather_noNA <- na.omit(weather)
temp <- weather_noNA %>% select(-USAF, -YEAR, -index)
cor(temp)

plot(weather_noNA$DEWPOINT[1:30000], weather_noNA$TEMPERATURE[1:30000])


set.seed(1)
split <- sample(1:nrow(weather_noNA), .7*nrow(weather_noNA))
X.train <- weather_noNA[split,]
X.test <- weather_noNA[-split,]

lm.fit <- lm(WINDSPEED~PRESSURE, data=X.train)
summary(lm.fit)

lm.fit <- lm(TEMPERATURE~DEWPOINT, data=X.train)
summary(lm.fit)

pred <- predict(lm.fit, newdata=X.test)
mean(abs(pred-X.test$TEMPERATURE))

#################################################

lm.fit <- lm(WINDDIR~WINDSPEED, data=X.train)
pred <- predict(lm.fit, newdata=X.test)
mean(abs(pred-X.test$TEMPERATURE))


##############################################################################

#By station 

Result <- data.frame(ID=test$ID, Value=NA)

#Divide test set ID into row and column
row <- as.numeric(gsub("\\-{1}.*", "", Result$ID))
col <- as.numeric(gsub("^.*\\-{1}", "", Result$ID))


position <- as.data.frame(cbind(row, col))
position <- position %>% arrange(row)
position$row <- as.integer(position$row)

#
length(unique(position$row))

#208 duplicates in row, we'll fill those missings later 
pos <- position %>% distinct(row, .keep_all = TRUE)



#76 Stations
station <- unique(train$USAF)


weather <- train %>% select(WINDDIR, WINDSPEED, TEMPERATURE, DEWPOINT, PRESSURE, 
                            USAF, YEAR, index)

#Create empty list
data_bystation <- list()

data_bystation <- split(weather, list(weather$USAF, weather$YEAR), drop=TRUE)


indicator <- as.numeric() #indicator for testing
obs <- as.numeric() #for testing
which.col <- as.numeric()
k <- 1

for(i in 1:length(data_bystation)){
    for(j in 1:nrow(data_bystation[[i]])){
        if(data_bystation[[i]]$index[j] %in% pos$row[k]){
            
            Bindex <- j #Backward index
            Findex <- j #Forward index
            
            if(j > 10 & j < nrow(data_bystation[[i]])-10){
                # +/- 10 width
                while(Bindex > (j-10)){
                    Bindex <- Bindex - 1 
                }
                while (Findex < (j+10)) {
                    Findex <- Findex + 1
                }
                
                #Values are all missings within the range
                if(is.na(as.numeric(table(is.na(data_bystation[[i]][Bindex:Findex, pos$col[k]]))["FALSE"]))){
                    indicator[k] <- 2
                    obs[k] <- data_bystation[[i]]$index[j]
                    which.col[k] <- pos$col[k]
                }
                #There are at least two nonmissing values, then we can use the function
                else if(as.numeric(table(is.na(data_bystation[[i]][Bindex:Findex, pos$col[k]]))["FALSE"]) >= 2){
                    temp <- data_bystation[[i]][Bindex:Findex, pos$col[k]]
                    temp <- na_ma(temp, weighting="exponential", k=20)
                    
                    data_bystation[[i]][Bindex:Findex, pos$col[k]] <- temp
                    
                    indicator[k] <- 1
                    obs[k] <- data_bystation[[i]]$index[j]
                    which.col[k] <- pos$col[k]
                }else{
                    indicator[k] <- 3
                    obs[k] <- data_bystation[[i]]$index[j]
                    which.col[k] <- pos$col[k]
                }
            }else if(j < 10){
                if(is.na(as.numeric(table(is.na(data_bystation[[i]][1:j+10, pos$col[k]]))["FALSE"]))){
                    indicator[k] <- 2
                    obs[k] <- data_bystation[[i]]$index[j]
                    which.col[k] <- pos$col[k]
                }else if(as.numeric(table(is.na(data_bystation[[i]][1:j+10, pos$col[k]]))["FALSE"]) >= 2){
                    temp <- data_bystation[[i]][1:j+10, pos$col[k]]
                    temp <- na_ma(temp, weighting="exponential", k=20)
                    
                    data_bystation[[i]][1:j, pos$col[k]] <- temp
                    
                    indicator[k] <- 1
                    obs[k] <- data_bystation[[i]]$index[j]
                    which.col[k] <- pos$col[k]
                }else{
                    indicator[k] <- 3
                    obs[k] <- data_bystation[[i]]$index[j]
                    which.col[k] <- pos$col[k]
                }
            }else if(j > nrow(data_bystation[[i]])-10){
                if(is.na(as.numeric(table(is.na(data_bystation[[i]][(j-10):nrow(data_bystation[[i]]), 
                                                                    pos$col[k]]))["FALSE"]))){
                    indicator[k] <- 2
                    obs[k] <- data_bystation[[i]]$index[j]
                    which.col[k] <- pos$col[k]
                }else if(as.numeric(table(is.na(data_bystation[[i]][(j-10):nrow(data_bystation[[i]]), 
                                                                    pos$col[k]]))["FALSE"]) >= 2){
                    temp <- data_bystation[[i]][(j-10):nrow(data_bystation[[i]]), pos$col[k]]
                    temp <- na_ma(temp, weighting="exponential", k=20)
                    
                    data_bystation[[i]][(j-10):nrow(data_bystation[[i]]), pos$col[k]] <- temp
                    
                    indicator[k] <- 1
                    obs[k] <- data_bystation[[i]]$index[j]
                    which.col[k] <- pos$col[k]
                }else{
                    indicator[k] <- 3
                    obs[k] <- data_bystation[[i]]$index[j]
                    which.col[k] <- pos$col[k]
                }
            }
            
            k <- k + 1
        }
    }
}


test_obs1 <- as.data.frame(cbind(obs, which.col, indicator))
#1 indicates we can fill NA using above 10 and below 10 values; 2 indicates above and below 10s 
#are all NAs; 3 indicates only 1 value from above or below 10s is nonmissing.
table(test_obs1$indicator)

test_obs <- test_obs1 %>% rename(row=obs, col=which.col) %>% select(-indicator)

diff_obs <- sqldf('SELECT * FROM position EXCEPT SELECT * FROM pos')


#Run this again for rest of the rows
indicator <- as.numeric() #indicator for testing
obs <- as.numeric() #for testing
which.col <- as.numeric()
k <- 1

for(i in 1:length(data_bystation)){
    for(j in 1:nrow(data_bystation[[i]])){
        if(data_bystation[[i]]$index[j] %in% diff_obs$row[k]){
            
            Bindex <- j #Backward index
            Findex <- j #Forward index
            
            if(j > 10 & j < nrow(data_bystation[[i]])-10){
                # +/- 10 width
                while(Bindex > (j-10)){
                    Bindex <- Bindex - 1 
                }
                while (Findex < (j+10)) {
                    Findex <- Findex + 1
                }
                
                if(is.na(as.numeric(table(is.na(data_bystation[[i]][Bindex:Findex, diff_obs$col[k]]))["FALSE"]))){
                    indicator[k] <- 2
                    obs[k] <- data_bystation[[i]]$index[j]
                    which.col[k] <- diff_obs$col[k]
                }else if(as.numeric(table(is.na(data_bystation[[i]][Bindex:Findex, diff_obs$col[k]]))["FALSE"]) >= 2){
                    temp <- data_bystation[[i]][Bindex:Findex, diff_obs$col[k]]
                    temp <- na_ma(temp, weighting="exponential", k=20)
                    
                    data_bystation[[i]][Bindex:Findex, diff_obs$col[k]] <- temp
                    
                    indicator[k] <- 1
                    obs[k] <- data_bystation[[i]]$index[j]
                    which.col[k] <- diff_obs$col[k]
                }else{
                    indicator[k] <- 3
                    obs[k] <- data_bystation[[i]]$index[j]
                    which.col[k] <- diff_obs$col[k]
                }
            }else if(j < 10){
                if(is.na(as.numeric(table(is.na(data_bystation[[i]][1:j+10, diff_obs$col[k]]))["FALSE"]))){
                    indicator[k] <- 2
                    obs[k] <- data_bystation[[i]]$index[j]
                    which.col[k] <- diff_obs$col[k]
                }else if(as.numeric(table(is.na(data_bystation[[i]][1:j+10, diff_obs$col[k]]))["FALSE"]) >= 2){
                    temp <- data_bystation[[i]][1:j+10, diff_obs$col[k]]
                    temp <- na_ma(temp, weighting="exponential", k=20)
                    
                    data_bystation[[i]][1:j, diff_obs$col[k]] <- temp
                    
                    indicator[k] <- 1
                    obs[k] <- data_bystation[[i]]$index[j]
                    which.col[k] <- diff_obs$col[k]
                }else{
                    indicator[k] <- 3
                    obs[k] <- data_bystation[[i]]$index[j]
                    which.col[k] <- diff_obs$col[k]
                }
            }else if(j > nrow(data_bystation[[i]])-10){
                if(is.na(as.numeric(table(is.na(data_bystation[[i]][(j-10):nrow(data_bystation[[i]]), 
                                                                    diff_obs$col[k]]))["FALSE"]))){
                    indicator[k] <- 2
                    obs[k] <- data_bystation[[i]]$index[j]
                    which.col[k] <- diff_obs$col[k]
                }else if(as.numeric(table(is.na(data_bystation[[i]][(j-10):nrow(data_bystation[[i]]), 
                                                                    diff_obs$col[k]]))["FALSE"]) >= 2){
                    temp <- data_bystation[[i]][(j-10):nrow(data_bystation[[i]]), diff_obs$col[k]]
                    temp <- na_ma(temp, weighting="exponential", k=20)
                    
                    data_bystation[[i]][(j-10):nrow(data_bystation[[i]]), diff_obs$col[k]] <- temp
                    
                    indicator[k] <- 1
                    obs[k] <- data_bystation[[i]]$index[j]
                    which.col[k] <- diff_obs$col[k]
                }else{
                    indicator[k] <- 3
                    obs[k] <- data_bystation[[i]]$index[j]
                    which.col[k] <- diff_obs$col[k]
                }
            }
            
            k <- k + 1
        }
    }
}

test_obs2 <- as.data.frame(cbind(obs, which.col, indicator))
table(test_obs2$indicator)

test_tot <- rbind(test_obs1, test_obs2)
test_tot <- test_tot %>% arrange(obs)


#8 obs missing (not being recorded from the for loop)
pos %>% filter(!test_obs1$obs %in% pos$row)

NAs <- test_tot %>% filter(indicator==2 | indicator==3)
table(NAs$which.col)


NAs_tot <- NAs %>% rename(row=obs, col=which.col) %>% select(-indicator)
moreNA <- pos %>% filter(!test_obs1$obs %in% pos$row)

#We want to fill these missings
NAs_tot <- rbind(NAs_tot, moreNA) %>% arrange(row)


###################################

X <- rbindlist(data_bystation)
X <- X %>% arrange(index)

###################################

#4 duplicates in row
length(unique(NAs_tot$row))

NAs_tot1 <- NAs_tot %>% distinct(row, .keep_all = TRUE)
NAs_tot2 <- sqldf('SELECT * FROM NAs_tot EXCEPT SELECT * FROM NAs_tot1')

temp <- filter(X, index %in% NAs_tot1$row)
temp <- cbind(temp, NAs_tot1)

#moreNA

table(temp$col)

table(temp$YEAR)

###############################

weather_noNA <- na.omit(X)

TEMPDEW.fit <- lm(TEMPERATURE~DEWPOINT, data=weather_noNA)
# summary(TEMPDEW.fit)

indicator <- as.numeric()
for(i in 1:nrow(temp)){
    if(temp$col[i]==3){
        if(is.na(temp$DEWPOINT[i])==F){
            temp$TEMPERATURE[i] <- predict(TEMPDEW.fit, newdata=temp[i,])
            indicator[i] <- 1 
        }
        else{
            indicator[i] <- 2
        }
    }
}
#496 temperature missings filled
table(indicator)

DEWTEMP.fit <- lm(DEWPOINT~TEMPERATURE, data=weather_noNA)
# summary(DEWTEMP.fit)

indicator <- as.numeric()
for(i in 1:nrow(temp)){
    if(temp$col[i]==4){
        if(is.na(temp$TEMPERATURE[i])==F){
            temp$DEWPOINT[i] <- predict(DEWTEMP.fit, newdata=temp[i,])
            indicator[i] <- 1 
        }
        else{
            indicator[i] <- 2
        }
    }
}
#262 dewpoint missings filled
table(indicator)


SPEEDPRESS.fit <- lm(WINDSPEED~PRESSURE, data=weather_noNA)
summary(SPEEDPRESS.fit)
PRESSSPEED.fit <- lm(PRESSURE~WINDSPEED, data=weather_noNA)

indicator <- as.numeric()
for(i in 1:nrow(temp)){
    if(temp$col[i]==2){
        if(is.na(temp$PRESSURE[i])==F){
            temp$WINDSPEED[i] <- predict(SPEEDPRESS.fit, newdata=temp[i,])
            indicator[i] <- 1 
        }
        else{
            indicator[i] <- 2
        }
    }
}
#635 windspeed missings filled
table(indicator)

indicator <- as.numeric()
for(i in 1:nrow(temp)){
    if(temp$col[i]==5){
        if(is.na(temp$WINDSPEED[i])==F){
            temp$PRESSURE[i] <- predict(PRESSSPEED.fit, newdata=temp[i,])
            indicator[i] <- 1 
        }
        else{
            indicator[i] <- 2
        }
    }
}
#619 pressure missings filled
table(indicator)


WINDDIR.fit <- lm(WINDDIR~WINDSPEED, data=weather_noNA)
indicator <- as.numeric()
for(i in 1:nrow(temp)){
    if(temp$col[i]==1){
        if(is.na(temp$WINDSPEED[i])==F){
            temp$WINDDIR[i] <- predict(WINDDIR.fit, newdata=temp[i,])
            indicator[i] <- 1 
        }
        else{
            indicator[i] <- 2
        }
    }
}
#216 winddir missings filled
table(indicator)


plot(weather_noNA$PRESSURE[1:2000], weather_noNA$WINDSPEED[1:2000])


##############################################################################


# X <- rbindlist(data_bystation)
# X <- X %>% arrange(index)
Result <- data.frame(ID=test$ID, Value=NA)


for(j in 1:nrow(Result)){
    Result$Value[j] <- X[row[j], col[j]]
}

table(is.na(Result$Value))


for(k in 1:nrow(Result)){
    if(row[k] %in% temp$index){
        if(is.na(as.numeric(temp[temp$index==row[k],][col[k]]))==F){
            Result$Value[k] <- as.numeric(temp[temp$index==row[k],][col[k]])
        }
    }
}

table(is.na(Result$Value))

temp$WINDDIR <- na_ma(temp$WINDDIR, k=20)
temp$WINDSPEED <- na_ma(temp$WINDSPEED, k=20)
temp$TEMPERATURE<- na_ma(temp$TEMPERATURE, k=20)
temp$DEWPOINT <- na_ma(temp$DEWPOINT, k=20)
temp$PRESSURE <- na_ma(temp$PRESSURE, k=20)

for(k in 1:nrow(Result)){
    if(row[k] %in% temp$index){
        if(is.na(as.numeric(temp[temp$index==row[k],][col[k]]))==F){
            Result$Value[k] <- as.numeric(temp[temp$index==row[k],][col[k]])
        }
    }
}

table(is.na(Result$Value))



X1=na.locf(X, fromLast=TRUE)
X2=na.locf(X, na.rm=FALSE)
X2$WINDDIR[1] <- X2$WINDDIR[2]

Result1 <- data.frame(ID=test$ID, Value=NA)
for(k in 1:nrow(Result1)){
    if(is.na(Result1$Value[k])==T){
        Result1$Value[k] <- (X1[row[k], col[k]] + X2[row[k], col[k]])/2
    }
}



range(Result$Value)

write.csv(Result, file = "Result.csv", quote = T, row.names = FALSE)




##############################################################################

#Cold deck imputation
#A systematically chosen value from an individual who has similar values on other variables.



##############################################################################

auxfile <- read.csv("ish-history.csv")

station.data <- auxfile %>% filter(USAF %in% station)

plot(station.data$LON, station.data$LAT)

coordinate <- station.data %>% select(LAT, LON)

library(RANN)

closest <- nn2(coordinate, k=3)[[1]]
closest


stations.list <- list()

station <- unique(X$USAF) 
station <- sort(station)


for(i in 1:length(station)){
    stations.list[[i]] <- filter(X, USAF==station[i]) 
}




missing <- as.numeric()
nonmissing <- as.numeric()
station <- as.numeric()
from <- as.numeric()
to <- as.numeric()
for(i in 1:length(data_bystation)){
    if(is.na(as.numeric(table(is.na(data_bystation[[i]]$WINDDIR))["FALSE"]))){
        nonmissing[i] <- 0
    }else{
        nonmissing[i] <- as.numeric(table(is.na(data_bystation[[i]]$WINDDIR))["FALSE"])
    }
    if(is.na(as.numeric(table(is.na(data_bystation[[i]]$WINDDIR))["TRUE"]))){
        missing[i] <- 0
    }else{
        missing[i] <- as.numeric(table(is.na(data_bystation[[i]]$WINDDIR))["TRUE"])
    }
    
    station[i] <- data_bystation[[i]]$USAF[1]
    from[i] <- range(data_bystation[[i]]$index)[1]
    to[i] <- range(data_bystation[[i]]$index)[2]
}



total <- as.data.frame(cbind(nonmissing, missing, station, from, to))


var1 <- temp %>% filter(temp$col==1)


    # if(var1$index[i]>= total$from )



##############################################################################

temp[is.na(temp)] <- 0

# Load libraries
library(mlbench)
library(caret)
library(caretEnsemble)


weather_noNA <- sample_n(weather_noNA, 12000)

set.seed(123)
split <- sample(1:nrow(weather_noNA), .7*nrow(weather_noNA))
X.train <- weather_noNA[split,]
X.test <- weather_noNA[-split,]



control <- trainControl(method = "cv", number=10,
                        index = createFolds(X.train$WINDDIR, 5),
                        savePredictions = "final")
# algorithmList <- c('rpart', 'glm', 'knn')
set.seed(123)
models <- caretList(WINDDIR~WINDSPEED+TEMPERATURE+PRESSURE, data=X.train, 
                    trControl=control, methodList=c("lm", "rpart", "svmRadial", "glm"))

results <- resamples(models)
summary(results)
dotplot(results)

set.seed(123)
models <- caretList(TEMPERATURE~DEWPOINT, data=X.train, 
                    trControl=control, methodList=c("lm", "svmRadial", "knn"))

set.seed(123)
stack.fit <- caretStack(models, method="rpart", metric="MAE", trControl=control)

pred <- predict(stack.fit, newdata=X.test)

mean(abs(as.numeric(pred) - X.test$WINDDIR))

##############################################################################



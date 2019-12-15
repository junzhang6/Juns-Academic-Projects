setwd("E:/SFU/2019Fall/STAT440/Module2 Weather Data")

rm(list = ls())

library(tidyverse)
library(sqldf)
library(ggplot2)
library(imputeTS)
library(zoo)
library(data.table)
library(car)
library(mlbench)
library(caret)
library(caretEnsemble)
train <- read.csv("train.csv", sep=",", na.strings="NaN")
test <- read.csv("test.csv")

#For filtering
train$index <- 1:nrow(train)


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

moreNA$indicator <- 2
moreNA <- moreNA %>% rename(obs=row, which.col=col)

test_tot <- rbind(test_tot, moreNA)
test_tot <- test_tot %>% arrange(obs)


############################
table(test_tot$indicator)
############################



#We want to fill these missings
moreNA <- pos %>% filter(!test_obs1$obs %in% pos$row)
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


###############################

weather_noNA <- na.omit(X)


X <- rbindlist(data_bystation)
X <- X %>% arrange(index)
temp <- filter(X, index %in% NAs_tot1$row)
temp <- cbind(temp, NAs_tot1)


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

###############################

#Reduce the sizes of training and testing to run
weather_reduce <- sample_n(weather_noNA, 12000)

#Ensemble
set.seed(123)
split <- sample(1:nrow(weather_reduce), .7*nrow(weather_reduce))
X.train <- weather_reduce[split,]
X.test <- weather_reduce[-split,]


control <- trainControl(method = "cv", index = createFolds(X.train$WINDSPEED, 5),
                        savePredictions = "final")
set.seed(123)
models <- caretList(WINDSPEED~WINDDIR+TEMPERATURE+PRESSURE, data=X.train, 
                    trControl=control, methodList=c("lm", "rpart", "svmRadial", "knn"))
results <- resamples(models)
summary(results)
dotplot(results)

dev.copy(png, file="EnsembleMethods.png", width=800, height=600)
dev.off()

set.seed(123)
stack.fit.WINDSPEED <- caretStack(models, method="svmRadial", metric="MAE", trControl=control)
pred <- predict(stack.fit.WINDSPEED, newdata=X.test)
mean(abs(as.numeric(pred) - X.test$WINDSPEED))

###############################

table(temp$WINDDIR==0)
table(temp$WINDSPEED==0)
table(temp$TEMPERATURE==0)
table(temp$DEWPOINT==0)
table(temp$PRESSURE==0)
temp[is.na(temp)] <- 0


table(temp$col)
indicator <- as.numeric()
for(i in 1:nrow(temp)) {
    if(temp$col[i]==2){
        temp$WINDSPEED[i] <- predict(stack.fit.WINDSPEED, newdata=temp[i,])
        indicator[i] <- 1
    }else{
        indicator[i] <- 2
    }
}
table(indicator)


control1 <- trainControl(method = "cv", index = createFolds(X.train$PRESSURE, 5),
                        savePredictions = "final")
set.seed(123)
models1 <- caretList(PRESSURE~WINDSPEED+WINDDIR+TEMPERATURE, data=X.train, 
                    trControl=control1, methodList=c("lm", "rpart", "svmRadial"))
results1 <- resamples(models1)
# summary(results1)
# dotplot(results1)
set.seed(123)
stack.fit.PRESSURE <- caretStack(models1, method="svmRadial", metric="MAE", trControl=control1)
# pred <- predict(stack.fit.WINDSPEED, newdata=X.test)
# mean(abs(as.numeric(pred) - X.test$WINDSPEED))


indicator <- as.numeric()
for(i in 1:nrow(temp)){
    if(temp$col[i]==5){
        temp$PRESSURE[i] <- predict(stack.fit.PRESSURE, newdata=temp[i,])
        indicator[i] <- 1 
    }else{
        indicator[i] <- 2
    }
}
table(indicator)




range(weather_noNA$WINDSPEED)
hist(weather_noNA$WINDSPEED, breaks=50, xlim=c(-2, 4))

range(weather_noNA$PRESSURE)
hist(weather_noNA$PRESSURE, breaks=50, xlim=c(-4, 3))

#####################################

length(weather_noNA[weather_noNA$WINDDIR>2,]$WINDDIR)
length(weather_noNA[weather_noNA$WINDDIR<2,]$WINDDIR)

hist(weather_noNA$WINDDIR, xlim=c(-2, 2), breaks=50)
sd(weather_noNA[weather_noNA$WINDDIR<2,]$WINDDIR)


#Randomly Generate WINDDIR values 
nrow(filter(temp, col==1))
indicator <- as.numeric()
for(i in 1:nrow(temp)){
    if(temp$col[i]==1){
        set.seed(123)
        temp$WINDDIR[i] <- rnorm(1, mean=0, sd=1.03)
        indicator[i] <- 1 
    }else{
        indicator[i] <- 2
    }
}
table(indicator)

######################################

#Now, most of the missings in temp dataframe has been filled
#However, some TEMPERATURE and DEWPOINT are still missing

temp[temp$WINDDIR==0, ]$WINDDIR <- NA
temp[temp$WINDSPEED==0, ]$WINDSPEED <- NA
temp[temp$TEMPERATURE==0, ]$TEMPERATURE <- NA
temp[temp$DEWPOINT==0, ]$DEWPOINT <- NA
temp[temp$PRESSURE==0, ]$PRESSURE <- NA

TEMP <- filter(temp, col==3)
plot(TEMP$TEMPERATURE, type="l", ylab="Temperature")

dev.copy(png, file="TemperatureMissing.png", width=800, height=600)
dev.off()

##############################################################################

tempp <- temp


TEMP <- filter(temp, col==3)


# X <- rbindlist(data_bystation)
# X <- X %>% arrange(index)
Result <- data.frame(ID=test$ID, Value=NA)


for(j in 1:nrow(Result)){
    Result$Value[j] <- X[row[j], col[j]]
}

table(is.na(Result$Value))


for(k in 1:nrow(Result)){
    if(row[k] %in% tempp$index){
        if(as.numeric(tempp[tempp$index==row[k],][col[k]])!=0){
            Result$Value[k] <- as.numeric(tempp[tempp$index==row[k],][col[k]])
        }
    }
}

table(is.na(Result$Value))



temp[temp$WINDDIR==0, ]$WINDDIR <- NA
temp[temp$WINDSPEED==0, ]$WINDSPEED <- NA
temp[temp$TEMPERATURE==0, ]$TEMPERATURE <- NA
temp[temp$DEWPOINT==0, ]$DEWPOINT <- NA
temp[temp$PRESSURE==0, ]$PRESSURE <- NA

Result1 <- Result
temp1 <- temp
Result2 <- Result
temp2 <- temp
Result3 <- Result
temp3 <- temp


#Average the rest
X1=na.locf(X, fromLast=TRUE)
X2=na.locf(X, na.rm=FALSE)
X2$WINDDIR[1] <- X2$WINDDIR[2]

for(k in 1:nrow(Result)){
    if(is.na(Result$Value[k])==T){
        Result$Value[k] <- (X1[row[k], col[k]] + X2[row[k], col[k]])/2
    }
}
table(is.na(Result$Value))


#MA the rest 
temp1$WINDDIR <- na_ma(temp1$WINDDIR, k=20, weighting = "linear")
temp1$WINDSPEED <- na_ma(temp1$WINDSPEED, k=20, weighting = "linear")
temp1$TEMPERATURE<- na_ma(temp1$TEMPERATURE, k=20, weighting = "linear")
temp1$DEWPOINT <- na_ma(temp1$DEWPOINT, k=20, weighting = "linear")
temp1$PRESSURE <- na_ma(temp1$PRESSURE, k=20, weighting = "linear")
for(k in 1:nrow(Result1)){
    if(row[k] %in% temp1$index){
        if(is.na(as.numeric(temp1[temp1$index==row[k],][col[k]]))==F){
            Result1$Value[k] <- as.numeric(temp1[temp1$index==row[k],][col[k]])
        }
    }
}
table(is.na(Result1$Value))

temp2$WINDDIR <- na_ma(temp2$WINDDIR, k=20, weighting = "simple")
temp2$WINDSPEED <- na_ma(temp2$WINDSPEED, k=20, weighting = "simple")
temp2$TEMPERATURE<- na_ma(temp2$TEMPERATURE, k=20, weighting = "simple")
temp2$DEWPOINT <- na_ma(temp2$DEWPOINT, k=20, weighting = "simple")
temp2$PRESSURE <- na_ma(temp2$PRESSURE, k=20, weighting = "simple")
for(k in 1:nrow(Result2)){
    if(row[k] %in% temp2$index){
        if(is.na(as.numeric(temp2[temp2$index==row[k],][col[k]]))==F){
            Result2$Value[k] <- as.numeric(temp2[temp2$index==row[k],][col[k]])
        }
    }
}
table(is.na(Result2$Value))

temp3$WINDDIR <- na_ma(temp3$WINDDIR, k=20, weighting = "exponential")
temp3$WINDSPEED <- na_ma(temp3$WINDSPEED, k=20, weighting = "exponential")
temp3$TEMPERATURE<- na_ma(temp3$TEMPERATURE, k=20, weighting = "exponential")
temp3$DEWPOINT <- na_ma(temp3$DEWPOINT, k=20, weighting = "exponential")
temp3$PRESSURE <- na_ma(temp3$PRESSURE, k=20, weighting = "exponential")
for(k in 1:nrow(Result3)){
    if(row[k] %in% temp3$index){
        if(is.na(as.numeric(temp3[temp3$index==row[k],][col[k]]))==F){
            Result3$Value[k] <- as.numeric(temp3[temp3$index==row[k],][col[k]])
        }
    }
}
table(is.na(Result3$Value))


Result$Value <- 0.25*(Result$Value) + 0.25*(Result1$Value) + 0.25*(Result2$Value) + 0.25*(Result3$Value)



############################

#Push forward the rest
X <- X %>% fill(WINDDIR, WINDSPEED, TEMPERATURE, DEWPOINT, PRESSURE)
X$WINDDIR[1] <- X$WINDDIR[2]

for(l in 1:nrow(Result)){
    if(is.na(Result$Value[l])){
        Result$Value[l] <- X[row[l], col[l]]
    }
}
table(is.na(Result$Value))

############################

#MA the rest
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

############################

#Average the rest
X1=na.locf(X, fromLast=TRUE)
X2=na.locf(X, na.rm=FALSE)
X2$WINDDIR[1] <- X2$WINDDIR[2]

# Result1 <- data.frame(ID=test$ID, Value=NA)
for(k in 1:nrow(Result)){
    if(is.na(Result$Value[k])==T){
        Result$Value[k] <- (X1[row[k], col[k]] + X2[row[k], col[k]])/2
    }
}
table(is.na(Result$Value))

########################################################


range(Result$Value)

write.csv(Result, file = "Result.csv", quote = T, row.names = FALSE)



########################################################
auxfile <- read.csv("ish-history.csv")

station.data <- auxfile %>% filter(USAF %in% station)

plot(station.data$LON, station.data$LAT)

coordinate <- station.data %>% select(LAT, LON)

library(RANN)

closest <- nn2(coordinate, k=3)[[1]]
closest

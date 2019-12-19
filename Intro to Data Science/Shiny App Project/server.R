#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
suppressPackageStartupMessages(library(tidyverse))
load("C02NorthernHemisphere.Rdata")
load("CanadianMeanTemp.Rdata")
load("CanadianAvgSnow.Rdata")
load("CanadianMaxTemp.Rdata")
load("CanadianMinTemp.Rdata")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  van_mean1=MeanTemp%>%filter(MeanTemp$`InfoTemp[2]`=="VANCOUVER")%>%select(Year, Annual)
  van_max=MaxTemp%>%filter(MaxTemp$`InfoTemp[2]`=="VANCOUVER")%>%select(Year, Annual)
  van_min=MinTemp%>%filter(MinTemp$`InfoTemp[2]`=="VANCOUVER")%>%select(Year, Annual)
  van_snow=AllSnow%>%filter(AllSnow$`InfoTemp[2]`=="VANCOUVER")%>%select(Year, Annual)
  van_mean1=van_mean1[-1,]
  van_max=van_max[-1,]
  van_min=van_min[-1,]
  for(i in 1:nrow(van_snow)){
    if(van_snow$Annual[i]=="-9999.9"){
      van_snow$Annual[i]=NA
    }
  }
  van_snow=na.omit(van_snow)
  

   
  output$C02_49th = renderPlot({
    #Analysis Tab
    #Avg of CO2 at 49 Northern level Over years from 1979 to 2017
    Co2North_year=Co2North%>%mutate(Year=str_extract(Co2North$YearDecimal, "^+\\d{4}"))
    Co2North_year=group_by(Co2North_year, Year)
    Co2avg_byYear=summarise(Co2North_year, Avg_49_co2=mean(Latitude49value))
    Co2avg_byYear=Co2avg_byYear[-nrow(Co2avg_byYear),]
    x=as.numeric(Co2avg_byYear$Year)
    y=Co2avg_byYear$Avg_49_co2
    
    plot(x=x[1:which(x==input$year)], y=y[1:which(x==input$year)], 
         xlab="Years", ylab="Average CO2(ppm) in a Year", 
         main="Average CO2 levels at the 49th parallel North Over Years")
    lines(x=x[1:which(x==input$year)], y=y[1:which(x==input$year)])
  })

  
  output$Annualplot = renderPlot({
    #Analysis Tab
    #Van Annual Average Temp over Years
    van_mean=MeanTemp%>%filter(MeanTemp$`InfoTemp[2]`=="VANCOUVER")%>%select(Year, Annual)
    #Set -9999.9 to NA in order to make plots
    for(i in 1:nrow(van_mean)){
      if(van_mean$Annual[i]=="-9999.9"){
        van_mean$Annual[i]=NA
      }
    }
    van_mean=na.omit(van_mean)
    #Years 1979-2017
    van_mean1979=van_mean[83:121,]
    x1=as.numeric(van_mean1979$Year)
    y1=van_mean1979$Annual

    plot(x=x1[1:which(x1==input$year)], y=y1[1:which(x1==input$year)],
           xlab="Years", ylab="Annual Mean Temperature", 
           main="Vancouver Annual Mean Temprature Over Years")
    lines(x=x1[1:which(x1==input$year)], y=y1[1:which(x1==input$year)])
    if(input$year==2017){
        abline(lm(y1~x1), col=2)
        legend("topleft", pch=c(1,NA), col=c(1,2), lty=c(NA,1), 
               c("Annual Mean Temperature", "Linear Regression Line"), bty="n")
    }
      # Slope:
      # summary(lm(van_mean$Annual~van_mean$Year))$coefficient[2,1]
  })

    
  output$Summerplot = renderPlot({
    #Analysis Tab
    ##############
    #Mean summer Temp in Vancouver Over Years
    van_summer=MeanTemp%>%filter(MeanTemp$`InfoTemp[2]`=="VANCOUVER")%>%select(Year, Summer)
    for(i in 1:nrow(van_summer)){
      if(van_summer$Summer[i]=="-9999.9"){
        van_summer$Summer[i]=NA
      }
    }
    van_summer=na.omit(van_summer)
    #Years 1979-2017
    van_summer1979=van_summer[84:122,]
    plot(van_summer1979$Year, van_summer1979$Summer, type="l", 
         xlab="Years", ylab="Average Summer Temperature", 
         main="Average Summer Temperature in Vancouver Over Years")
    points(van_summer1979$Year, van_summer1979$Summer)
    abline(lm(van_summer1979$Summer~van_summer1979$Year), col=2)
    legend("topleft", pch=c(1,NA), col=c(1,2), lty=c(NA,1),
           c("Mean Summer Temperature", "Linear Regression Line"), bty="n")
    # summary(lm(van_summer1979$Summer~van_summer1979$Year))$coefficient[2,1]
  })

  
  output$C02_49th_plot = renderPlot({
    #Data Tab
    #Avg of CO2 at 49 Northern level Over years from 1979 to 2017
    Co2North_year=Co2North%>%mutate(Year=str_extract(Co2North$YearDecimal, "^+\\d{4}"))
    Co2North_year=group_by(Co2North_year, Year)
    Co2avg_byYear=summarise(Co2North_year, Avg_49_co2=mean(Latitude49value))
    Co2avg_byYear=Co2avg_byYear[-nrow(Co2avg_byYear),]
    plot(as.numeric(Co2avg_byYear$Year), Co2avg_byYear$Avg_49_co2, 
         xlab="Years", ylab="Average CO2(ppm) in a Year", 
         main="Average CO2 levels at the 49th parallel North Over Years")
    lines(x=as.numeric(Co2avg_byYear$Year), y=Co2avg_byYear$Avg_49_co2)
  })
  
  
  output$cplot = renderPlot({
    #Data Tab
    Co2North49_all=Co2North%>%select(YearDecimal, Latitude49value)
    plot(Co2North49_all$YearDecimal, Co2North49_all$Latitude49value, 
         xlab="Years", ylab="CO2 ppm", main="CO2 at the 49th parallel North Over Years")
    
    #Avg of CO2 at 49 Northern level Over years from 1979 to 2017
    Co2North_year=Co2North%>%mutate(Year=str_extract(Co2North$YearDecimal, "^+\\d{4}"))
    Co2North_year=group_by(Co2North_year, Year)
    Co2avg_byYear=summarise(Co2North_year, Avg_49_co2=mean(Latitude49value))
    Co2avg_byYear=Co2avg_byYear[-nrow(Co2avg_byYear),]
    if(input$meanCo2_49th){
      points(Co2avg_byYear$Year, Co2avg_byYear$Avg_49_co2, col=2, pch=16, cex=1.5)
      if(input$meanCO2_plot){
        plot(Co2avg_byYear$Year, Co2avg_byYear$Avg_49_co2, type="l", 
             xlab="Years", ylab="Average CO2(ppm) in a Year", 
             main="Average CO2 levels at the 49th parallel North Over Years")
        points(Co2avg_byYear$Year, Co2avg_byYear$Avg_49_co2, col=2)
      }
    }
  })
  
  
  output$tempPlot = renderPlot({
    #Within Data Tab, show some interesting plots
    if(input$tsTrend!=TRUE){
      if(input$dataSource=="Van_meanTemp"){
        x=van_mean1$Year; y=van_mean1$Annual
        plot(x[which(x==input$Yearmin):which(x==input$Yearmax)], y[which(x==input$Yearmin):which(x==input$Yearmax)],
             xlab="Years", ylab="Annual Average Temperature", 
             main="Annually Average Temperature in Vancouver Over Years", pch=16)
      }
      else if(input$dataSource=="Van_maxTemp"){
        x=van_max$Year; y=van_max$Annual
        plot(x[which(x==input$Yearmin):which(x==input$Yearmax)], y[which(x==input$Yearmin):which(x==input$Yearmax)],
             xlab="Years", ylab="Annual Max Temperature", 
             main="Annually Maximum Temperature in Vancouver Over Years", pch=16)
      }
      else if(input$dataSource=="Van_minTemp"){
        x=van_min$Year; y=van_min$Annual
        plot(x[which(x==input$Yearmin):which(x==input$Yearmax)], y[which(x==input$Yearmin):which(x==input$Yearmax)],
             xlab="Years", ylab="Annual Min Temperature", 
             main="Annually Minimum Temperature in Vancouver Over Years", pch=16)
      }else{
        NULL
      }
    }
  })
  
  
  output$snowPlot = renderPlot({
    #Data Tab, snow plot
    if(input$tsTrend!=TRUE){
      plot(van_snow$Year, van_snow$Annual, xlab="Years", ylab="Annual Average Snow", 
           main="Annually Average Snow in Vancouver Over Years", pch=16)
    }
  })
  
  
  output$tsPlot = renderPlot({
    if(input$dataSource=="Van_meanTemp"){
      if(input$tsTrend){
        plot(van_mean1$Year, van_mean1$Annual, xlab="Years", ylab="Annual Average Temperature", 
             main="Time-series Plot of Annually Average Temperature \n in Vancouver Over Years",
             type="l")
      }
    }else if(input$dataSource=="Van_maxTemp"){
      if(input$tsTrend){
        plot(van_max$Year, van_max$Annual, xlab="Years", ylab="Annual Max Temperature", 
             main="Time-series Plot of Annually Maximum Temperature \n in Vancouver Over Years",
             type="l")
      }
    }else if(input$dataSource=="Van_minTemp"){
      if(input$tsTrend){
        plot(van_min$Year, van_min$Annual, xlab="Years", ylab="Annual Min Temperature", 
             main="Time-series Plot of Annually Minimum Temperature \n in Vancouver Over Years",
             type="l")
      }
    }else if(input$dataSource=="Van_meanSnow"){
      if(input$tsTrend){
        plot(van_snow$Year, van_snow$Annual, xlab="Years", ylab="Annual Average Snow", 
             main="Time-series Plot of Annually Average Snow \n in Vancouver Over Years",
             type="l")
      }
    }else{
      NULL
    }
  })
  
})

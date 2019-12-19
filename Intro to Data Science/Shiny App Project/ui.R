#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Vancouver Climate Change Over Years"),
  
      navbarPage(title="",
                  
                    tabPanel("About", 
                               mainPanel(
                                 HTML("<h4> Jun Zhang </h4>"),
                                 p(strong("My name is Jiajun(Jun) Zhang and I'm a fourth-year Statistics 
                                          student at SFU. I used to hate computing until I use R pretty 
                                          much everyday throughout my 3 and 4 levels' Statistics courses. 
                                          It's like I already get used to it, and knowing the importance 
                                          of computing in Statistics field makes me think I need to
                                          get better at it.")),
                                 p(strong("You can find me on:")),
                                 HTML('<p> <a href="https://www.linkedin.com/in/jun-zhang-057241174/" 
                                      target="_blank">Linkedin</a><br/> </p>'),
                                 p(HTML('&nbsp;')),
                                 p(strong("The purpose of this project is to explore how the increase of CO2 
                                   affects climate change in Vancouver from years of 1979 to 2017. The 
                                   datasets I use are the measures of CO2 at surface level over the 
                                   Northern Hemisphere, monthly snow, maximum, average, and minimum 
                                   temperatures in Canadian cities. The CO2 dataset consists of the CO2
                                   value and its uncertainty at different latitudes from 1979 to 2018, 
                                   while the rest of datasets consist the measures mainly from 1897 
                                   to 2017. In order to match up the informations, I'll focus on the 
                                   datas from year of 1979 to 2017. The analysis uses the annual average
                                   temeratures and annual summer temperatures in Vancouver to compare
                                   with the CO2 changes at 49th parallel North, since we know the 
                                   latitude of Vancouver is 49.2827 degree North."))

                               )
                    ),
                  
  
                   tabPanel("Climate Datas",  
                            sidebarLayout( 
                              sidebarPanel( 
                                radioButtons(inputId = "dataSource", label = "Choose a Dataset?", 
                                             choices = list("CO2_49th", "Van_meanTemp", "Van_maxTemp", "Van_minTemp", "Van_meanSnow"), 
                                             selected = "Van_minTemp"),
                                conditionalPanel(
                                  condition="input.dataSource == 'CO2_49th'",
                                  HTML("<p> This dataset consists of the CO2 values at 49th parallel North from 1979 to 2018 with year decimals.</p>"),
                                  checkboxInput("meanCo2_49th", strong("Add Average CO2 Over Years?"), FALSE), 
                                  conditionalPanel(
                                    condition = "input.meanCo2_49th == true", 
                                    helpText(HTML("<h5>The red points indicate the average CO2 each year at 49th parallel North</h5>")), 
                                    checkboxInput("meanCO2_plot", strong("Make a plot of average CO2 only?"), FALSE)
                                  )
                                ), 

                                conditionalPanel(
                                  condition="input.dataSource=='Van_meanSnow' | input.dataSource=='Van_meanTemp' | 
                                  input.dataSource=='Van_maxTemp' | input.dataSource=='Van_minTemp'",
                                  checkboxInput("tsTrend", strong("Make this a time-series trend plot?"), FALSE),
                                  conditionalPanel(
                                    condition="input.tsTrend!=true & (input.dataSource=='Van_meanTemp' | input.dataSource=='Van_maxTemp' | input.dataSource=='Van_minTemp')",
                                    HTML("<p> The min and max of years from dataset are set as default values.</p>"),
                                    numericInput("Yearmin", "Year From:", 1897, min=1897, max=2017),
                                    numericInput("Yearmax", "To:", 2017, min=1897, max=2017)
                                  )
                                )
                              ),   
                        
                              mainPanel( 
                                conditionalPanel(
                                  condition="input.dataSource == 'CO2_49th'",
                                  plotOutput("cplot")
                                ),
                                conditionalPanel(
                                  condition="input.tsTrend!=true & (input.dataSource=='Van_meanTemp' | input.dataSource=='Van_maxTemp' | input.dataSource=='Van_minTemp')",
                                  plotOutput("tempPlot")
                                ),
                                conditionalPanel(
                                  condition="input.Yearmin<1897 | input.Yearmax>2017",
                                  h1("Input Years must be within 1897 to 2017!!!!!")
                                ),
                                conditionalPanel(
                                  condition="input.tsTrend!=true & input.dataSource=='Van_meanSnow'",
                                  plotOutput("snowPlot")
                                ),
                                conditionalPanel(
                                  condition="input.tsTrend==true", 
                                  plotOutput("tsPlot")
                                )
                              ) 
                            )
                   ),
                   
                   tabPanel("Climate Change Analysis", 
                            sidebarLayout(
                              sidebarPanel( 
                                selectInput(inputId="Mean_summer", label="Vancouver Mean Temperature", 
                                            c("Annual Average Temp", "Summer Average Temp"), 
                                            selected="Annual Average Temp"),
                                conditionalPanel(
                                  condition="input.Mean_summer == 'Annual Average Temp'", 
                                  sliderInput(inputId="year", label="Years", min=1979, max=2017, value=1979),
                                  p("Slide the input Year to see how CO2 and annual average temperature change over years.
                                    Result is shown until it reachs the year of 2017."),
                                  conditionalPanel(
                                    condition="input.year == '2017'", 
                                    checkboxInput("slope_est", strong("Want to know the slope estimate of the regression line?"), FALSE),
                                    conditionalPanel(
                                      condition="input.slope_est == true", 
                                      HTML("<h1> Slope=0.016 </h1>"), 
                                      checkboxInput("meaning", strong("Interpretation"), FALSE), 
                                      conditionalPanel(
                                        condition="input.meaning == true",
                                        p(strong("This slope estimate suggests that for each year increases,
                                             the annual average temperature in Vancouver increases 0.016 degrees in celsius."))
                                        )
                                      )
                                    )
                                ),
                                conditionalPanel(
                                  condition="input.Mean_summer =='Summer Average Temp' ",
                                  HTML("<p> This shows the CO2 changes at 49th parallel North and the average temperature of summer in Vancouver. 
                                       Clearly, this is also a relationship between them.</p>"),
                                  checkboxInput("slope_summer_est", strong("Want to know the slope estimate of the regression line?"), FALSE),
                                  conditionalPanel(
                                    condition="input.slope_summer_est == true",
                                    HTML("<h1> Slope=0.032 </h1>"),
                                    p(strong("Previously, we have an estimated slope of 0.016 for average annual
                                          temperature regression fit. This time, we have an estimate slope of 0.032 
                                          which is twice larger. That says, the average summer temperature in Vancouver
                                          increases 0.032 degrees in celsius as one year increases."))
                                  )
                                )
                              ),
      
                              mainPanel( 
                                conditionalPanel(
                                  condition="input.Mean_summer == 'Annual Average Temp'",
                                  plotOutput("C02_49th"),
                                  plotOutput("Annualplot")
                                ),
                                conditionalPanel(
                                  condition="input.Mean_summer =='Summer Average Temp'",
                                  plotOutput("C02_49th_plot"),
                                  plotOutput("Summerplot")
                                )
                              ) 
                            )
                   ),
                 
                 tabPanel("References", 
                          mainPanel(
                            HTML("<p>Dlugokencky, E.J., K.W. Thoning, P.M. Lang, and P.P. Tans (2017), 
                                 NOAA Greenhouse Gas Reference from Atmospheric Carbon Dioxide Dry Air Mole 
                                 Fractions from the NOAA ESRL Carbon Cycle Cooperative Global Air Sampling Network.</p>"), 
                            HTML("<p>Mekis, \u00C9. and L.A. Vincent, 2011: An overview of the second generation adjusted daily 
                                 precipitation dataset for trend analysis in Canada. Atmosphere-Ocean, 49(2), 163-177</p>"),
                            HTML("<p>Vincent, L. A., X. L. Wang, E. J. Milewska, H. Wan, F. Yang, and V. Swail, 2012. A second 
                                 generation of homogenized Canadian monthly surface air temperature for climate trend analysis, 
                                 J. Geophys. Res., 117, D18110, doi:10.1029\\/2012JD017859.</p>"),
                            HTML("<p>Wan, H., X. L. Wang, V. R. Swail, 2010: Homogenization and trend analysis of Canadian 
                                 near-surface wind speeds. Journal of Climate, 23, 1209-1225.</p>"),
                            HTML("Wan, H., X. L. Wang, V. R. Swail, 2007: A quality assurance system for Canadian hourly pressure 
                                 data. J. Appl. Meteor. Climatol., 46, 1804-1817. "),
                            HTML("<p>Wang, X.L, Y. Feng, L. A. Vincent, 2013. Observed changes in one-in-20 year extremes of 
                                 Canadian surface air temperatures. Atmosphere-Ocean. Doi:10.1080\\/07055900.2013.818526.</p>")
                          )
                  )
                   
        )
    
))

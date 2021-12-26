# Define UI for application that draws a histogram
library(dplyr)
library(leaflet)
library(shiny)
library(readr)
library(ggplot2)

mydata<-read_csv("data.csv",show_col_types = FALSE)


# Define UI for application that draws a histogram
ui<-(fluidPage(
  
  navbarPage("Covid 19 cases",
             
             tabPanel("Incidence",
                      
                      # Sidebar with a slider input 
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("region", "Region",choices=c("All",unique(mydata$Region))),
                          selectInput("province","Province",choices="All"),
                          sliderInput("Topview", "Top Highest Incidence",min=1,max=nrow(mydata),100),
                          actionButton("updateview","Update View"),
                         
                          
                        ),
                        
                        mainPanel(
                          tabsetPanel(
                            id="myPanelSet",
                            # PROVIDE SUMMARY OF SELECTED REGIONS AND PROVINCES 
                            tabPanel("Data summary",  
                                     h4("Summary"),
                                     verbatimTextOutput("summary"),
                                     
                                     # DISPLAY THE TOP INCIDENCE RATE LIMITED BY NUMBER DEFINED IN SLIDERINPUT: 
                                     h4("Observations"),
                                     tableOutput("data")
                            ),
                            #   PLOT THE INCIDENCE RATE VERSUS POPULATION DENSITY AND ALLOW THE SELECTION OF ANY POINTS FOR DISPLAY
                            tabPanel("Plot",
                                     h4("Select or brush any/group points in the graphic to display the details below "),
                                     plotOutput("myreg",brush = "plot_brush",height=500,  width=750),
                                     tableOutput("data_reg")),
                            
                            # DISPLAY IN THE MAP THE CITY AND THE RELATIVE SIZE OF CONFIRMED CASES.   
                            tabPanel("MAP",
                                     h4("The  size of the cirkel is related to the number of the confirmed cases in the city"),
                                     leaflet::leafletOutput('map')
                            )
                          )
                        )
                      )
             )
  )
)
)


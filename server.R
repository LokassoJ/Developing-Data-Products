
library(shiny)
library(ggplot2)

# Define server logic required to display tables
server <- function(input, output, session) {
  # SELECTION OF THE REGION 
  
  region <- reactive( {
    if(input$region!="All"){
      filter(mydata, Region == input$region)
    } else {
             mydata
    }
    })
  # ADJUSTMENT THE LIST OF PROVINCES LINKED TO THE REGION 
  observeEvent(region(), {
    choices <- unique(region()$Province)
    if(input$region!="All"){
       choices =c("All", choices)} else {
       choices ="All" 
      }
    updateSelectInput(inputId = "province", choices =choices) 
  })
  # SELECTION OF PROVINCE AND THE LIST OF THE CITIES ASSOCIATED 
 prov<-eventReactive(input$updateview, {
   if(input$province !="All"){
   data0<-region() %>%
       filter(Province==input$province)
   } else {
     data0<-region()
   }
   arrange(data0,desc(Incidence_rate))
 }) 
 # UPDATING THE MAX in THE SliderInput BASED ON SELECTED ITEMS
 observeEvent(prov(),{
   totalrow<-nrow(prov())
   updateSliderInput(inputId="Topview",max=totalrow)
   
 })
 data1<-reactive({head(prov(),input$Topview)})
 
 # DISPLAY THE TABLE 
  output$data <- renderTable({
    req(input$province)
    data1()
  
  })
  
  # OUTPUT THE SCATTER PLOTS
  output$data_reg <- renderTable({
    brushedPoints(data1(), input$plot_brush)
    
  })
  output$summary <- renderPrint({
    dataset <- prov()[,c(2,5:7,10:11)]
    summary(dataset)
  })
 
  output$myreg<-renderPlot({
        ggplot(data1(),aes(Density,Incidence_rate,size=pop))+
        geom_point(aes(colour=Region))})
  
  # OUTPUT THE MAP WITH LEAFLET
  
  output$map<-renderLeaflet({
    data1()%>%select(city,cases,long, lat)%>%leaflet()%>%addTiles()%>%
      addMarkers(popup =data1()$city,label= data1()$city)%>% addCircles(weight=1,radius=sqrt(data1()$cases)*100)
   
  })
  
  
}
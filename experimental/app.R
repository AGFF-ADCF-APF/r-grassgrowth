#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Graswachstum"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("sliderWeek",
                        "Kalenderwoche",
                        min = 1,
                        max = 52,
                        value = week),
            numericInput("n", "n", 50),
            actionButton("Show", "Anzeigen"),
            actionLink("getData", "Daten aktualisieren")
        ),
        
        
        

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
        
        
    )
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {

   
  
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
      
      # import data 
      input$Show
      
      
      eventReactive(input$getData,{
        graswachstum <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSrcomkkwzl7-XESOTZLhk0XOCQMq5cz1kkcMif7sl8PGybv_nHK8ite3eMM_-UKLKC1hHEHVHlx_lc/pub?gid=513247593&single=true&output=csv") %>% 
          select(Standort, Erhebungsdatum, Graswachstum..kg.TS.ha.) %>% 
          rename(growth=Graswachstum..kg.TS.ha., date=Erhebungsdatum, place=Standort)
        
        daten <- merge(standorte, graswachstum, by = "place") %>% 
          mutate(Ort = factor(Ort), date=as.Date(date, format="%d.%m.%Y"), 
                 weeknum=as.integer(strftime(daten$date, format = '%V')))
      })
      
    
      # eventReactive(input$Show, {
        week <- input$sliderWeek
        #week <- as.integer(strftime(Sys.Date(), format = "%V"))
        weeks <- c(week, week-1)
        year <- strftime(maxdaten$date[1], format = "%Y")
        
        currentdaten <- daten  %>% group_by(place) %>%
          filter(weeknum %in% weeks)
        
        maxdaten <- currentdaten  %>% group_by(place) %>%
          filter(date == max(date))
        
        #Datum <- as.Date(week, format="%V")
        Datum = ""
        Untertitel <- paste("Kalenderwoche",week,Datum ) 
        
        
        ggplot() +
          geom_sf(data = sw_shp, fill="#b6d69a", color = "white") +
          #geom_point(data = daten, aes(x = lon, y = lat, color = growth), size = 5) +
          #scale_color_gradient(low = "blue", high = "red", name = "kg TS/ha/Tag") +
          #geom_text(data = maxdaten, aes(x = lon, y = lat, label = round(growth,0)), 
          #         color = "black", size = 4, nudge_y = 0.05, nudge_x = 0.07) +
          geom_point(data = maxdaten, aes(x = lon, y = lat), shape=21, 
                     color="black", stroke=1.5, size=8, fill="white") +
          geom_text(data = maxdaten, aes(x = lon, y = lat, label = round(growth,0)), fontface="bold" ) +
          geom_text(data = maxdaten, aes(x = lon, y = lat, label = Ort), 
                    fontface="bold", nudge_x = 0.35) +
          labs(title = paste("Graswachstum",year), subtitle = Untertitel ) +
          theme_void()
        
        
      # })


     
      
      
      
    
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

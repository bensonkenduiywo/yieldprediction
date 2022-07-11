library(shiny)
library(tmap)
library(raster)
path <- "D:/RCMRD/Code/yieldprediction"
Zambia <- shapefile(paste0(path,"/rwanda/rwanda_Maize_Forecasts.shp"))
#Rename the variables
names(Zambia)[2] <- "Water_Stress"
names(Zambia)[3] <- "LAI"
names(Zambia)[4] <- "Yield_kg_ha"
zmb_vars <-  c("Water_Stress", "LAI", "Yield_kg_ha", "Season")
year <- 2022
rwa <- subset(Zambia, date== year)

ui <- fluidPage(
  tmapOutput("map"),
  selectInput("var", "Variable", zmb_vars)
)

server <- function(input, output, session) {
  output$map <- renderTmap({
    tm_shape(rwa) +
      tm_polygons(zmb_vars[1], zindex = 401)
  })
  
  observe({
    var <- input$var
    tmapProxy("map", session, {
      tm_remove_layer(401) +
      tm_shape(rwa) +
            tm_polygons(var, zindex = 401)
    })
  })
}	


shinyApp(ui, server)

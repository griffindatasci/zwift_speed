library(shiny)
library(data.table)

format_seconds <- function(s){
  sprintf("%02.0f:%04.1f", s%/%60, s%%60)
}


bikes <- fread("../data/speed_data.txt")
bikes[class=="road", class:="Road"]
bikes[class=="tt", class:="TT"]



ui <- fluidPage(
  inputPanel(
    numericInput("level", "Unlock Level", value=60, min=1, max=60),
    numericInput("drops", "Drops Budget", value=6000000, min=1, max=6000000),
    checkboxInput("incl_tt", "Include TT Bikes?", value=TRUE)
  ),
  dataTableOutput("bikes")
)




server <- function(input, output) {

  output$bikes <- renderDataTable({
    
    
    bikes[level<=input$level & drops<=input$drops & 
            (class=="Road"|(class=="TT")==input$incl_tt), 
          .("Frame"=frame, "Wheels"=wheel, "Class"=class, "Level"=level, 
            "Drops"=format(drops, big.mark=","), 
            "Flat"=format_seconds(tempus), "Climb"=format_seconds(alp))]
  })
  
}



# run application 
shinyApp(ui = ui, server = server)

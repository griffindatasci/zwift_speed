library(shiny)
library(data.table)
library(DT)

format_seconds <- function(s){
  sprintf("%02.0f:%04.1f", s%/%60, s%%60)
}


format_dt <- function(x, right_cols=NA){
  datatable(x, 
            rownames=FALSE, 
            options=list(pageLength=20, 
                         dom = "tp",
                         columnDefs = list(list(className = 'dt-right', targets = right_cols))), 
            escape=FALSE) 
}

bikes <- fread("data/speed_data.txt")
bikes[class=="road", class:="Road"]
bikes[class=="tt", class:="TT"]



ui <- fluidPage(
  inputPanel(
    sliderInput("level", "Unlock Level", value=c(0,60), min=0, max=60, dragRange=FALSE),
    numericInput("drops", "Drops Budget", value=6000000, min=1, max=6000000),
    checkboxInput("incl_tt", "Include TT Frames", value=TRUE)
  ),
  tabsetPanel(
    tabPanel("Wheels", dataTableOutput("wheels")),
    tabPanel("Frames", dataTableOutput("frames")),
    tabPanel("Complete Bikes", dataTableOutput("bikes"))
  )
)




server <- function(input, output) {
  
  output$bikes <- renderDataTable({
    # - reduce to those matching user inputs
    bikes_out <- bikes[level>=input$level[1] & level<=input$level[2] & drops<=input$drops & 
                         (class=="Road"|(class=="TT")==input$incl_tt)]
    
    # - format the output dataset
    bikes_out <- bikes_out[,.("Frame"=frame, "Wheel"=wheel, "Class"=class,
                              "Level"=level, "Drops"=format(drops, big.mark=","), 
                              "Flat"=ifelse(tempus>min(tempus), 
                                            paste0(format_seconds(tempus), " (+", sprintf("%.1f", tempus-min(tempus)), ")"),
                                            format_seconds(tempus)), 
                              "Climb"=ifelse(alp>min(alp), 
                                             paste0(format_seconds(alp), " (+", sprintf("%.1f", alp-min(alp)), ")"),
                                             format_seconds(alp)))]
    
    # - format the table (DT:: table, not data.table)
    format_dt(bikes_out, 4)
  })
  
  
  output$frames <- renderDataTable({
    # - reduce to frames only (no need to adjust drops price, carbon 32mm wheel is free)
    frames_out <- bikes[wheel=="Zwift 32mm Carbon"]
    
    # - reduce to those matching user inputs
    frames_out <- frames_out[level>=input$level[1] & level<=input$level[2] & drops<=input$drops & 
                               (class=="Road"|(class=="TT")==input$incl_tt)]
    
    # - format the output dataset
    frames_out <- frames_out[,.("Frame"=frame, "Class"=class,
                                "Level"=level, "Drops"=format(drops, big.mark=","), 
                                "Flat"=ifelse(tempus>min(tempus), 
                                              paste0(format_seconds(tempus), " (+", sprintf("%.1f", tempus-min(tempus)), ")"),
                                              format_seconds(tempus)), 
                                "Climb"=ifelse(alp>min(alp), 
                                               paste0(format_seconds(alp), " (+", sprintf("%.1f", alp-min(alp)), ")"),
                                               format_seconds(alp)))]
    
    # - format the table (DT:: table, not data.table)
    format_dt(frames_out, 3)
  })
  
  
  
  
  output$wheels <- renderDataTable({
    # - reduce to wheels only, adjust drops  (price includes Zwift Aero frame)
    wheels_out <- bikes[frame=="Zwift Aero"][, drops:=drops-min(drops)]
    
    # - reduce to those matching user inputs
    wheels_out <- wheels_out[level>=input$level[1] & level<=input$level[2] & drops<=input$drops]
    
    # - format the output dataset
    wheels_out <- wheels_out[,.("Wheel"=wheel, 
                                "Level"=level, "Drops"=format(drops, big.mark=","), 
                                "Flat"=ifelse(tempus>min(tempus), 
                                              paste0(format_seconds(tempus), " (+", sprintf("%.1f", tempus-min(tempus)), ")"),
                                              format_seconds(tempus)), 
                                "Climb"=ifelse(alp>min(alp), 
                                               paste0(format_seconds(alp), " (+", sprintf("%.1f", alp-min(alp)), ")"),
                                               format_seconds(alp)))]
    
    # - format the table (DT:: table, not data.table)
    format_dt(wheels_out, 2)
  })
  
}



# run application 
shinyApp(ui = ui, server = server)

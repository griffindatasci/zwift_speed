library(shiny)
library(data.table)
library(DT)

format_seconds <- function(s){
  sprintf("%02.0f:%04.1f", s%/%60, s%%60)
}


format_dt <- function(x){
  col_inds <- c(1:length(x))-1
  left_cols <- col_inds[names(x) %in% c("Frame", "Wheel")]
  right_cols <- col_inds[names(x) %in% c("Flat", "Climb", "Drops")]
  cen_cols <- col_inds[!col_inds%in%c(left_cols, right_cols)]

  datatable(x, 
            rownames=FALSE, 
            options=list(pageLength=20, 
                         dom = "tp",
                         columnDefs = list(list(width="80px", className = "dt-center", targets = cen_cols),
                                           list(width="80px", className = "dt-right", targets = right_cols),
                                           list(width="250px", className = "dt-left", targets = left_cols))), 
            escape=FALSE) 
}

bikes <- fread("data/speed_data.txt")
bikes[class=="road", class:="Road"]
bikes[class=="tt", class:="TT"]



# Add bold to tron row
bikes[model_f=="TRON", make_f:="<strong>Zwift</strong>"]
bikes[model_f=="TRON", model_f:="<strong>TRON</strong>"]


# Paste make/model with line break
bikes[, frame:=paste0(make_f, "<br>", model_f)]
bikes[, wheel:=paste0(make_w, "<br>", model_w)]



ui <- fluidPage(
  tags$head(includeHTML("WWW/google_analytics_tag.html"),
            tags$link(rel="stylesheet", type="text/css", href="style.css"),
            tags$link(rel="preconnect", href="https://fonts.googleapis.com"),
            tags$link(rel="preconnect", href="https://fonts.gstatic.com"),
            tags$link(href="https://fonts.googleapis.com/css2?family=Source+Code+Pro:ital,wght@0,300;0,400;0,500;1,300;1,400;1,500&display=swap", rel="stylesheet")),
    
    
  tags$h1("Zwift Speed Data"),
  
  
  
  
  
  
  includeHTML("WWW/input_panel.html"),
  textOutput("txt_level"), # TODO

  # inputPanel(
     sliderInput("level", "Unlock Level", value=c(0,60), min=0, max=60, dragRange=FALSE),
  #   numericInput("drops", "Drops Budget", value=NA, min=1, max=6000000),
  #   checkboxInput("incl_tt", "Include TT Frames", value=TRUE)
  # ),
  # 
  
  
  
  
  
  
  tabsetPanel(
    tabPanel("Frames", dataTableOutput("frames")),
    tabPanel("Wheels", dataTableOutput("wheels")),
    tabPanel("Bikes", dataTableOutput("bikes"))
  ),
  
  tagAppendAttributes(class="footnote", tags$div(tags$p("This dashboard allows users to explore data from the ",
                                                        tags$a("Zwift Insider speed tests.", href="https://zwiftinsider.com/charts-frames/")),
                                                 tags$p("Frame and wheel selection affects performance in Zwift. Zwift Insider have performed tests using the majority of frames (paired with Zwift 32mm Carbon wheels) and wheels (paired with the Zwift Aero frame) on flat (two laps of Tempus Fugit) and climbing (the Alpe du Zwift) courses, using a bot set at 300W and 75kg (4W/kg)."), 
                                                 tags$p("Because frame and wheel performance are independent of one another, these datasets from around 130 tests can be combined into a dataset of over 3,300 complete bikes, found in the bikes tab above. This dashboard allows rankings to be filtered on the basis of user level and available drops (the in-game currency), and dynamically returns the gap to the best performing item in the filtered dataset.")))
  ,
  
  tagAppendAttributes(class="footnote", tags$div(tags$p("This service is developed and maintained by", 
                                                        tags$a(href="https://github.com/griffindatasci", "GriffinDataSci"))))
  
  )




server <- function(input, output) {
  
  
  
  
  output$txt_level <- renderText({input$level})

  
  
  
  
  
  output$bikes <- renderDataTable({
    # - reduce to those matching user inputs
    bikes_out <- bikes[level>=input$level[1] & level<=input$level[2] & (drops<=input$drops | is.na(input$drops)) & 
                         (class=="Road"|(class=="TT")==input$incl_tt)]
    
    # - format the output dataset
    bikes_out <- bikes_out[order(tempus),
                            .("Frame"=frame, 
                              "Wheel"=wheel, 
                              "Flat"=ifelse(tempus>min(tempus), 
                                            paste0(format_seconds(tempus), "<br>(+", sprintf("%.1f", tempus-min(tempus)), ")"),
                                            format_seconds(tempus)), 
                              "Climb"=ifelse(alp>min(alp), 
                                             paste0(format_seconds(alp), "<br>(+", sprintf("%.1f", alp-min(alp)), ")"),
                                             format_seconds(alp)), 
                              "Class"=class,
                              "Level"=level, 
                              "Drops"=format(drops, big.mark=","))]
    
    # - format the table (DT:: table, not data.table)
    format_dt(bikes_out)
  })
  
  
  output$frames <- renderDataTable({
    # - reduce to frames only (no need to adjust drops price, carbon 32mm wheel is free)
    frames_out <- bikes[wheel=="Zwift<br>32mm Carbon"]
    
    # - reduce to those matching user inputs
    frames_out <- frames_out[level>=input$level[1] & level<=input$level[2] & (drops<=input$drops | is.na(input$drops)) & 
                               (class=="Road"|(class=="TT")==input$incl_tt)]
    
    # - format the output dataset
    frames_out <- frames_out[order(tempus),
                              .("Frame"=frame,
                                "Flat"=ifelse(tempus>min(tempus), 
                                              paste0(format_seconds(tempus), "<br>(+", sprintf("%.1f", tempus-min(tempus)), ")"),
                                              format_seconds(tempus)), 
                                "Climb"=ifelse(alp>min(alp), 
                                               paste0(format_seconds(alp), "<br>(+", sprintf("%.1f", alp-min(alp)), ")"),
                                               format_seconds(alp)),
                                "Class"=class,
                                "Level"=level,
                                "Drops"=format(drops, big.mark=","))]
    
    # - format the table (DT:: table, not data.table)
    format_dt(frames_out)
  })
  
  
  
  
  output$wheels <- renderDataTable({
    # - reduce to wheels only, adjust drops  (price includes Zwift Aero frame)
    wheels_out <- bikes[frame=="Zwift<br>Aero"][, drops:=drops-min(drops)]
    
    # - reduce to those matching user inputs
    wheels_out <- wheels_out[level>=input$level[1] & level<=input$level[2] & (drops<=input$drops | is.na(input$drops))]
    
    # - format the output dataset
    wheels_out <- wheels_out[order(tempus),
                              .("Wheel"=wheel, 
                                "Flat"=ifelse(tempus>min(tempus), 
                                              paste0(format_seconds(tempus), "<br>(+", sprintf("%.1f", tempus-min(tempus)), ")"),
                                              format_seconds(tempus)), 
                                "Climb"=ifelse(alp>min(alp), 
                                               paste0(format_seconds(alp), "<br>(+", sprintf("%.1f", alp-min(alp)), ")"),
                                               format_seconds(alp)), 
                                "Level"=level, 
                                "Drops"=format(drops, big.mark=","))]
    
    # - format the table (DT:: table, not data.table)
    format_dt(wheels_out)
  })
  
}



# run application 
shinyApp(ui = ui, server = server)

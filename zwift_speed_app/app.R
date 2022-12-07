# PREP =========================================================================
library(shiny)
library(data.table)
library(DT)

# Function to format seconds as MM:SS.s
format_seconds <- function(s){
  sprintf("%02.0f:%04.1f", s%/%60, s%%60)
}

# Function to format datatables - all three should get the same formatting
format_dt <- function(x){
  col_inds <- c(1:length(x))-1
  left_cols <- col_inds[names(x) %in% c("Frame", "Wheel")]
  right_cols <- col_inds[names(x) %in% c("Flat", "Climb", "Drops")]
  cen_cols <- col_inds[!col_inds%in%c(left_cols, right_cols)]

  datatable(x, rownames=FALSE, options=list(pageLength=20, dom = "tp",
            columnDefs=list(list(width="80px",  className="dt-center", targets=cen_cols),
                            list(width="80px",  className="dt-right",  targets=right_cols),
                            list(width="250px", className="dt-left",   targets=left_cols))),
            escape=FALSE) 
}

# Function to format times and time gaps with breaks
format_time_gap <- function(x){
  ifelse(x>min(x), 
         paste0(format_seconds(x), "<br>(+", sprintf("%.1f", x-min(x)), ")"),
         format_seconds(x))
}


# DATA =========================================================================
# Read in data
bikes <- fread("data/speed_data.txt")

# Add bold to TRON row
bikes[model_f=="TRON", make_f:="<strong><em>Zwift</em></strong>"]
bikes[model_f=="TRON", model_f:="<strong><em>TRON</em></strong>"]

# Paste make/model with line break
bikes[, frame:=paste0(make_f, "<br>", model_f)]
bikes[, wheel:=paste0(make_w, "<br>", model_w)]

# Mark TT bikes
bikes[class=="tt", frame:=paste0(frame, "<sup><em> TT</em></sup>")]


# Mark gift items
free_wheels <- bikes[make_f=="Zwift" & model_f=="Aero" & drops==319500, paste0(make_w, "<br>", model_w)]
free_frames <- bikes[make_w=="Zwift" & model_w=="32mm Carbon" & drops==0, paste0(make_f, "<br>", model_f)]
bikes[frame %in% free_frames, frame:=paste0("<em>", frame, "</em>")]
bikes[wheel %in% free_wheels, wheel:=paste0("<em>", wheel, "</em>")]


# USER INTERFACE ===============================================================
ui <- fluidPage(
  # HTML head to add analytics tag, style and fonts
  tags$head(includeHTML("WWW/google_analytics_tag.html"),
            tags$link(rel="stylesheet", type="text/css", href="style.css"),
            tags$link(rel="preconnect", href="https://fonts.googleapis.com"),
            tags$link(rel="preconnect", href="https://fonts.gstatic.com"),
            tags$link(href="https://fonts.googleapis.com/css2?family=Source+Code+Pro:ital,wght@0,300;0,400;0,500;1,300;1,400;1,500&display=swap", rel="stylesheet")),
    
  
  # Top of page - title and user input interface  
  tags$h1("Zwift Speed Data"),
  includeHTML("WWW/input_panel.html"),
  
  # Tables
  tabsetPanel(
    tabPanel("Frames", dataTableOutput("frames")),
    tabPanel("Wheels", dataTableOutput("wheels")),
    tabPanel("Bikes", dataTableOutput("bikes"))
  ),
  
  # Footnotes on free items, explainer and link to griffindatasci
  tagAppendAttributes(class="footnote", tags$div("Items in italics are free or special unlock items, not available for purcahse in the drop shop.")),
  tagAppendAttributes(class="footnote", tags$div(tags$p("This dashboard allows users to explore data from the ",
                                                        tags$a("Zwift Insider speed tests.", href="https://zwiftinsider.com/charts-frames/")),
                                                 tags$p("Frame and wheel selection affects performance in Zwift. Zwift Insider have performed tests using the majority of frames (paired with Zwift 32mm Carbon wheels) and wheels (paired with the Zwift Aero frame) on flat (two laps of Tempus Fugit) and climbing (the Alpe du Zwift) courses, using a bot set at 300W and 75kg (4W/kg)."), 
                                                 tags$p("Because frame and wheel performance are independent of one another, these datasets from around 130 tests can be combined into a dataset of over 3,300 complete bikes, found in the bikes tab above. This dashboard allows rankings to be filtered on the basis of user level and available drops (the in-game currency), and dynamically returns the gap to the best performing item in the filtered dataset."))),
  tagAppendAttributes(class="footnote", tags$div(tags$p("This service is developed and maintained by", 
                                                        tags$a(href="https://github.com/griffindatasci", "GriffinDataSci"))))
  
  )


# SERVER =======================================================================
server <- function(input, output) {
  
  output$frames <- renderDataTable({
    # - reduce to frames only (no need to adjust drops price, carbon 32mm wheel is free) and those matching user inputs
    frames_out <- bikes[wheel=="<em>Zwift<br>32mm Carbon</em>" & (level_f<=input$level| is.na(input$level)) & (drops<=input$drops | is.na(input$drops)) & (class=="road"|(class=="tt")==input$incl_tt)]
    
    # - format the output dataset
    frames_out <- format_dt(frames_out[order(tempus), .("Frame"=frame, "Flat"=format_time_gap(tempus), "Climb"=format_time_gap(alp), "Level"=level_f)])
  })
  
  output$wheels <- renderDataTable({
    # - reduce to wheels only, adjust drops (price includes Zwift Aero frame)
    wheels_out <- bikes[frame=="Zwift<br>Aero"][, drops:=drops-min(drops)]
    
    # - reduce to those matching user inputs
    wheels_out <- wheels_out[(level_w<=input$level | is.na(input$level))  & (drops<=input$drops | is.na(input$drops))]
    
    # - format the output dataset
    wheels_out <- format_dt(wheels_out[order(tempus), .("Wheel"=wheel, "Flat"=format_time_gap(tempus), "Climb"=format_time_gap(alp), "Level"=level_w)])
  })
  
  output$bikes <- renderDataTable({
    # - reduce to those matching user inputs
    bikes_out <- bikes[(level<=input$level | is.na(input$level)) & (drops<=input$drops | is.na(input$drops)) & (class=="road"|(class=="tt")==input$incl_tt)]
      
    # - format the output dataset
    bikes_out <- format_dt(bikes_out[order(tempus), .("Frame"=frame, "Wheel"=wheel, "Flat"=format_time_gap(tempus), "Climb"=format_time_gap(alp), "Level"=level)])
  })
}

# RUN APPLICATION ============================================================== 
shinyApp(ui = ui, server = server)
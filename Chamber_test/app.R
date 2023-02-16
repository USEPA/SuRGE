#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# LIBRARY --------------------------------------------------------------------

library(tidyverse)
library(readxl)
library(janitor) # format dataframe names
library(scales) # for ggplot2 datetime formatting 
library(spsurvey) # lake design
library(tictoc) # timing operations
library(conflicted)
conflicted::conflict_scout()
conflict_prefer("select", "dplyr") # select() will call dplyr::select()
conflict_prefer("filter", "dplyr") # filter() will call dplyr::filter()
conflict_prefer("rename", "dplyr") # filter() will call dplyr::rename()



# LOAD AND ORGANIZE DATA -----------------------------------------------------

# Set User path
userPath <- paste0(
  Sys.getenv("USERPROFILE"), 
  "/Environmental Protection Agency (EPA)/SuRGE Survey of Reservoir Greenhouse gas Emissions - Documents/")

# Read in field sheets data 
source(paste0(userPath,
              "rProjects/Joe/SuRGE/scripts/analysis/readFieldSheets.R"))

# read in survey design file
source(paste0(userPath, 
              "rProjects/Joe/SuRGE/scripts/analysis/readSurgeLakes.R"))

# read raw LGR data
source(paste0(userPath, 
              "rProjects/Joe/SuRGE/scripts/analysis/readLgr.R"))

filter(gga, is.na(RDateTime))
gga <- filter(gga, !is.na(RDateTime)) # strip out missing RDateTime

# logical for missing chamber deployment times
missing_chamb_deply_date_time <- is.na(fld_sheet$chamb_deply_date_time)

# Join with fld_sheet to get site_id and chamb_deply_date_time
# This join duplicates the time series for each station within each lake
gga_2 <- gga %>%
  left_join(fld_sheet %>%
              filter(!missing_chamb_deply_date_time) %>%
              select(lake_id, site_id, chamb_deply_date_time),
            by = "lake_id")

#3. ADD CO2 AND CH4 RETRIEVAL AND DEPLOYMENT TIMES
# We may want to model different portions of the time series for CO2 and CH4.
# Here we create fields to hold retrieval and deployment times for each gas.
gga_2 <- gga_2 %>%
  # mutate ensures that all records have deployment and retrieval times
  # for CO2 and CH4. assume deployment time recorded in field is correct,
  # will inspect/modify below.
  mutate(co2DeplyDtTm = chamb_deply_date_time ,
         # assume retrieval 5 minutes after deployment
         co2RetDtTm =  chamb_deply_date_time + (60*5),
         ch4DeplyDtTm = chamb_deply_date_time,
         ch4RetDtTm = chamb_deply_date_time + (60*5))


# Function to get data with user-adjusted times
get_adjusted_data <- function(x) {
  
  # Read in refined deployment and retrieval data from Excel files.
  # use .xls.--Can read file while file is open in Excel.
  # list of files containing deployment and retrieval data.
  sdjDataList <- paste0(userPath, "/data/",
    c("ADA/chamberAdjustmentsAda.xls",
      "CIN/chamberAdjustmentsCIN.xls", 
      "RTP/chamberAdjustmentsRTP.xls", 
      "R10/chamberAdjustmentsR10.xls", 
      "USGS/chamberAdjustmentsUSGS.xls", 
      "DOE/chamberAdjustmentsDOE.xls",
      "NAR/chamberAdjustmentsNAR.xls"))
  
  # Read data
  adjData <- map_df(sdjDataList, readxl::read_xls, sheet = "DATA",
                    col_types = c("text", "numeric", 
                                  rep("date", 4), 
                                  rep("text", 4))) #lake_id is character
  
  #3.3. update deployment and retrieval times
  gga_adj <- x %>% 
    # remove co2DeplyDtTm, co2RetDtTm, ch4DeplyDtTm, and ch4RetDtTm.  They will 
    # be replaced with data from adjData or derived from chamb_deply_date_time
    select(-contains("DtTm")) %>%
    # Remove these columns if present.  Won't be present first time through
    select_if(
      !names(.) %in% c("co2Notes", "ch4Notes", "co2Status", "ch4Status")) %>%
    # Join with adjDataDf.
    left_join(., adjData) %>%
    # ensure all records have deployment and retrieval times for CO2 and CH4
    mutate(co2DeplyDtTm = case_when(
      is.na(co2DeplyDtTm) ~ chamb_deply_date_time, 
      # if na, then use field sheet data
      TRUE ~ co2DeplyDtTm), 
      # if not na, then use data supplied from adjDataDf
      co2RetDtTm = case_when(
        is.na(co2RetDtTm) ~ chamb_deply_date_time + (60*5), 
        # assume retrieval 5 minutes after deployment
        TRUE ~ co2RetDtTm), 
      # if not na, then use data supplied from adjDataDf
      ch4DeplyDtTm = case_when(
        is.na(ch4DeplyDtTm) ~ chamb_deply_date_time, 
        # if na, then use field sheet data
        TRUE ~ ch4DeplyDtTm), 
      # if not na, then use data supplied from adjDataDf
      ch4RetDtTm = case_when(
        is.na(ch4RetDtTm) ~ chamb_deply_date_time + (60*5), 
        # assume retrieval 5 minutes after deployment
        TRUE ~ ch4RetDtTm))  
  # if not na, then use data supplied from adjDataDf
  
  return(gga_adj)
  
}

# USER INTERFACE ------------------------------------------------------------

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Hello"),
  
  sidebarLayout(
    # user inputs (lake, site, and data type)
    sidebarPanel("choose lake and site",
                 selectInput("lake", label = "lake id", 
                             choices = unique(fld_sheet$lake_id) %>% 
                               str_sort(numeric = "TRUE"), 
                             selected = "275"),
                 selectInput("site", label = "site id", 
                             choices = unique(fld_sheet$site_id) %>% 
                               sort(),
                             selected = 2),
                 radioButtons("data", label = "plot which data?", 
                              choices = c("original", "updated")), 
                 # Press the button to render plots
                 actionButton("go", "GO!"), 
                 width = 2),
    # Plot output, side-by-side
    mainPanel("Plots incoming!",
              fluidRow(
                splitLayout(cellWidths = c("50%", "50%"), 
                            plotOutput("plot_ch4"), 
                            plotOutput("plot_co2")),
                width = 12
              )
    )
  )
)

# SERVER --------------------------------------------------------------------


server <- function(input, output) {
  
  observeEvent(input$go, {
  
    if (input$data == "updated")
      gga_data <- get_adjusted_data(gga_2) 
    else
      gga_data <- gga_2
    lake_id.i = input$lake
    site_id.i = input$site  
    
    plots <- gga_data %>% 
      filter(lake_id == lake_id.i, 
             site_id == site_id.i, 
             RDateTime > ch4DeplyDtTm - 60, # start 1 min before deploy
             RDateTime < ch4RetDtTm + 60) # extend plot 1 min post deploy  
    
    output$plot_ch4 <- renderPlot({
      
      plot_ch4 <- plots %>% 
        filter(CH4._ppm > 0) %>%
        ggplot(aes(RDateTime, CH4._ppm)) + 
        geom_point() +
        geom_vline(aes(xintercept = as.numeric(ch4DeplyDtTm))) +
        geom_vline(aes(xintercept = as.numeric(ch4RetDtTm))) +
        scale_x_datetime(date_labels = ("%m/%d %H:%M")) +
        ggtitle(paste("CH4: lake_id = ", lake_id.i, "site_id = ", site_id.i))
      plot_ch4

    })
    
    output$plot_co2 <- renderPlot({
      
      plot_co2 <- plots %>%
        filter(CO2._ppm > 0) %>%
        ggplot(aes(RDateTime, CO2._ppm)) +
        geom_point() +
        geom_vline(aes(xintercept = as.numeric(co2DeplyDtTm))) +
        geom_vline(aes(xintercept = as.numeric(co2RetDtTm))) +
        scale_x_datetime(date_labels = ("%m/%d %H:%M")) +
        ggtitle(paste("CO2: lake_id = ", lake_id.i, "site_id = ", site_id.i))
      plot_co2
      
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


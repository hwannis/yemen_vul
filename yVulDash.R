library(shiny)
library(DT)
library(dplyr)
library(tidyr)
library(plotly)
library(writexl)
library(openxlsx)
library(shinydashboard)
library(sf)
library(lmtest) # for model fitting
library(leaflet)
library(rmapshaper)
library(lwgeom)
library(geojsonsf)
library(webshot)
library(htmlwidgets)

vul_shp<- st_read("shp/Yemen_vul_simp.shp")
vul_df<- read.xlsx("data/vul_data.xlsx")
vul_shp_simplified <- ms_simplify(vul_shp)

# Define UI 
ui <- navbarPage("Yemen Child Vulnerability Mapping",
           tabPanel("Population Mapping",
           tags$div(tags$a(href="https://data.humdata.org/dataset/yemen-population-estimates", 
                           target="_blank", 
                           "Data source: UN OCHA HDX"),
                    style="text-align:right; margin-top:10px; margin-bottom:10px;"
           ),
           fluidPage(
             fluidRow(
               # column(3, selectInput("variable", "Select a variable to sum:",
               #                       choices = names(totaldb)[sapply(totaldb, is.numeric)], width = "200px")),
               # column(3, selectInput("crsdg_filter", "Filter by crsdg:",
               #                       choices = c("All", unique(totaldb$crsdg)), width = "200px")),
               # column(3, verbatimTextOutput("num_indicators")),
               column(3, selectInput("population_group_select", "Select Population group to map:",
                                       choices = names(vul_df[, grepl("total", colnames(vul_df),ignore.case = FALSE) | 
                                                                 grepl("U5", colnames(vul_df),ignore.case = FALSE)| 
                                                                grepl("child", colnames(vul_df),ignore.case = FALSE)| 
                                                                grepl("w15", colnames(vul_df),ignore.case = FALSE)]), width = "200px")),
               # column(3, selectInput("tab1_filter", "Select Population group to map:",
               #                       choices = c("All", unique(totaldb$crsdg)), width = "200px")),
               column(3, downloadButton("downloadMap1", "Download Map as PDF")),
               column(3, downloadButton("downloadPopData", "Download Data"))
             ),tags$h3(
               style = "text-align: center; font-size: 20px;",
               textOutput("selectedVarTitle")
             ),
             # plotlyOutput("bar_chart"),
             leafletOutput("population_map", height= "1200px"), # Add this line for the map
             fluidRow(
               column(6,tableOutput("population_table")),
             column(6, plotlyOutput("box_plot_pop",width = "100%", height= "1500px"))
           ),tags$footer(
             tags$hr(),  # Optional: horizontal line before the disclaimer
             tags$div("The designations and maps used do not reflect a position by UNICEF on the legal status of any country or territory or of its authorities, or the delimitation of any frontiers.", 
                      style = "text-align: center; margin-top: 20px; font-style: italic;")
           )
  )
  ),
  tabPanel("Vulnerability Mapping",
           tags$div(tags$a(href="https://data.humdata.org/dataset/yemen-population-estimates", 
                           target="_blank", 
                           "Data source: UN OCHA HDX and MODA analysis"),
                    style="text-align:right; margin-top:10px; margin-bottom:10px;"
           ),
           fluidPage(
             fluidRow(
               # column(3, selectInput("variable", "Select a variable to sum:",
               #                       choices = names(totaldb)[sapply(totaldb, is.numeric)], width = "200px")),
               # column(3, selectInput("crsdg_filter", "Filter by crsdg:",
               #                       choices = c("All", unique(totaldb$crsdg)), width = "200px")),
               # column(3, verbatimTextOutput("num_indicators")),
               column(3, selectInput("vul_var_select", "Select Vulnerability Variable to Map:",
                                     choices = names(vul_df[, grepl("quintile", colnames(vul_df),ignore.case = FALSE)]), width = "200px")),
               # column(3, selectInput("tab1_filter", "Select Population group to map:",
               #                       choices = c("All", unique(totaldb$crsdg)), width = "200px")),
               column(3, downloadButton("downloadMap2", "Download Map as PDF")),
               column(3, downloadButton("downloadData1", "Download Data")),
               column(3,
                      selectInput("label_lang", "Choose Label", c("None"="none", "English" = "ADM2_EN", "Arabic" = "ADM2_AR", "P-Code"="ADM2_PC"))
                     )
             ),tags$h3(
               style = "text-align: center; font-size: 20px;",
               textOutput("selectedVarTitle2")
             ),
             # plotlyOutput("bar_chart"),
             leafletOutput("vul_map", height= "1200px"),
             tags$h3(
               style = "text-align: center; font-size: 20px;",
               textOutput("selectedVarTitle3")
             ), # Add this line for the map
             tableOutput("vul_table")
           ),tags$footer(
             tags$hr(),  # Optional: horizontal line before the disclaimer
             tags$div("The designations and maps used do not reflect a position by UNICEF on the legal status of any country or territory or of its authorities, or the delimitation of any frontiers.", 
                      style = "text-align: center; margin-top: 20px; font-style: italic;")
           )
  )
)

server <- function(input, output, session) {
  # reactiveValues to hold click information
  click_info <- reactiveValues(id = NULL, lat = NULL, lng = NULL)
  
  # Reset click_info when vul_var_select changes
  observeEvent(input$vul_var_select, {
    print(paste("Dropdown changed to: ", input$vul_var_select))
    print(paste("Click Info before reset: ", click_info$id, click_info$lat, click_info$lng))
    click_info$id <- NULL
    click_info$lat <- NULL
    click_info$lng <- NULL
    print("Click Info after reset: ")
    print(click_info)
  })
  
  # Update click_info when the map is clicked
  observeEvent(input$vul_map_shape_click, {
    print("Map clicked!")
    click_info$id <- input$vul_map_shape_click$id
    click_info$lat <- input$vul_map_shape_click$lat
    click_info$lng <- input$vul_map_shape_click$lng
  })
  
  # Mapping of variable names to date ranges
  name_ranges <- list(
    U5_m = paste0("Under five male"),
    U5_f = paste0("Under five female"),
    U5_t = paste0("Under five total"),
    c5_17m = paste0("Children 5-17 male"),
    c5_17f = paste0("Children 5-17 female"),
    c5_17t = paste0("Children 5-17 total"),
    child_m = paste0("Children male"),
    child_f = paste0("Children female"),
    child_t = paste0("Children total"),
    total_f = paste0("Total female population"),
    total_m = paste0("Total male population"),
    total = paste0("Total population"),
    'w15-49' = paste0("Women 15-49"),
    dep_u5 = paste0("Children under five deprived"),
    dep_c517 = paste0("Children 5-17 deprived"),
    dep_tot = paste0("Children 0-17 deprived"),
    dep_mpi = paste0("Children 0-17 MPI deprived"),
    index_u5= paste0("Under five deprivation index"),
    index_c517= paste0("Children 5-17 deprivation index"),
    index_tot= paste0("Children 0-17 deprivation index"),
    index_mpi = paste0("Children 0-17 MPI deprivation index"),
    index_u5_quintile= paste0("Under five deprivation quintiles"),
    index_c517_quintile= paste0("Children 5-17 deprivation quintiles"),
    index_tot_quintile= paste0("Children 0-17 deprivation quintiles"),
    index_mpi_quintile= paste0("Children 0-17 MPI deprivation quintiles"),
    index_tot_calc_quintile= paste0("Children 0-17 deprivation quintiles weighted")
  )
  
  
  main_df<- left_join(vul_shp_simplified,vul_df[!names(vul_df) %in% c("ADM1_EN", "ADM1_PC","ADM2_EN")],by ="ADM2_PC")
  # print(main_df)
### tab 1
  
  selected_shp <- reactive({
    # Select only the column specified by input$population_group_select
    selected_df <- vul_df %>% 
      select(ADM2_PC, matches(input$population_group_select))
    
    # Join the selected column to vul_shp
    merged_shp <- left_join(vul_shp_simplified, selected_df, by = "ADM2_PC")
    
    return(merged_shp)
  })
  
  output$selectedVarTitle <- renderText({
    var_label <- ifelse(input$population_group_select %in% names(name_ranges), name_ranges[[input$population_group_select]], "")
    title_text <- paste0("Population size by Admin level 2 - ", var_label)
    return(title_text)
  })
  
  # 
  # output$population_map <- renderLeaflet({
  #   # Access the reactive spatial object
  #   shp <- selected_shp()
  #   var_label <- ifelse(input$population_group_select %in% names(name_ranges), name_ranges[[input$population_group_select]], "")
  #   title_text <- paste0("Population size by Admin level 2 - ", var_label)
  # 
  #   # Handle NA values
  #   shp$adjusted_vals <- ifelse(is.na(shp[[input$population_group_select]]), 1, as.numeric(shp[[input$population_group_select]]))
  # 
  #   # For displaying values in a readable format (like 1,234,567)
  #   labels <- formatC(shp$adjusted_vals, format="f", big.mark=',')
  # 
  #   # Log transform the non-NA values
  #   log_vals <- ifelse(shp$adjusted_vals == 1, 1, log(shp$adjusted_vals))
  # 
  #   # Exclude NAs for break calculation
  #   
  #   breaks_log <- quantile(log_vals, probs = seq(0, 1, by = 0.1), na.rm = TRUE)
  # 
  #   # Create a color palette based on the selected variable and explicitly set color for NA values
  #   pal <- colorBin(palette = c("white", "red"), bins = breaks_log, domain = log_vals)
  # 
  #   leaflet(shp) %>%
  #     addProviderTiles(providers$Stamen.Toner) %>%
  #     addPolygons(
  #       fillColor = ~pal(log_vals),
  #       weight = 2,
  #       opacity = 1,
  #       color = "white",
  #       dashArray = "3",
  #       fillOpacity = 0.7,
  #       labelOptions = labelOptions(noHide = TRUE, direction = "auto", textOnly = TRUE)
  #     ) %>%
  #     addLegend(
  #       pal = pal,
  #       values = log_vals,
  #       labFormat = labelFormat(prefix = "", transform = function(x) exp(x)),
  #       title = title_text,
  #       position = "bottomright"
  #     )
  # })
  
  output$population_map <- renderLeaflet({
    # Access the reactive spatial object
    shp <- selected_shp()
    var_label <- ifelse(input$population_group_select %in% names(name_ranges), name_ranges[[input$population_group_select]], "")
    title_text <- paste0("Population size by Admin level 2 - ", var_label)
    
    # Handle NA values
    shp$adjusted_vals <- ifelse(is.na(shp[[input$population_group_select]]), -9999, as.numeric(shp[[input$population_group_select]]))
    
    # Log transform
    log_vals <- ifelse(shp$adjusted_vals == -9999, -9999, log(shp$adjusted_vals))
    
    # Define breaks for log-transformed values, excluding placeholder
    valid_log_vals <- log_vals[log_vals != -9999]
    breaks_log <- quantile(valid_log_vals, probs = seq(0, 1, by = 0.1), na.rm = TRUE)
    
    # Define a custom color function
    custom_pal <- function(val) {
      if(val == -9999) {
        return("grey")
      } else {
        pal <- colorBin(palette = c("white", "red"), domain = valid_log_vals, bins = breaks_log)
        return(pal(val))
      }
    }
    
    leaflet(shp) %>%
      addProviderTiles(providers$Stamen.Toner) %>%
      addPolygons(
        fillColor = ~sapply(log_vals, custom_pal),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        labelOptions = labelOptions(noHide = TRUE, direction = "auto", textOnly = TRUE)
      ) %>%
      addLegend(
        pal = colorBin(palette = c("white", "red"), domain = valid_log_vals, bins = breaks_log),
        values = valid_log_vals,
        labFormat = labelFormat(prefix = "", transform = function(x) ifelse(x == -9999, "NA", round(exp(x)))),
        title = title_text,
        position = "bottomright"
      )
  })



##########pop map hover labels
  observe({

    map_proxy_2 <- leafletProxy("population_map")
    shp <- selected_shp()
    labels <-paste("N: ",format(as.numeric(shp[[input$population_group_select]]), big.mark = ",", scientific = FALSE, nsmall = 0), "; P-code:", shp$ADM2_PC)
    # Access the reactive spatial object

    var_label <- ifelse(input$population_group_select %in% names(name_ranges), name_ranges[[input$population_group_select]], "")
    title_text <- paste0("Population size by Admin level 2 - ", var_label)

    
    # Handle NA values
    shp$adjusted_vals <- ifelse(is.na(shp[[input$population_group_select]]), -9999, as.numeric(shp[[input$population_group_select]]))
    
    # Log transform
    log_vals <- ifelse(shp$adjusted_vals == -9999, -9999, log(shp$adjusted_vals))
    
    # Define breaks for log-transformed values, excluding placeholder
    valid_log_vals <- log_vals[log_vals != -9999]
    breaks_log <- quantile(valid_log_vals, probs = seq(0, 1, by = 0.1), na.rm = TRUE)
    
    # Define a custom color function
    custom_pal <- function(val) {
      if(val == -9999) {
        return("grey")
      } else {
        pal <- colorBin(palette = c("white", "red"), domain = valid_log_vals, bins = breaks_log)
        return(pal(val))
      }
    }
    
    map_proxy_2 %>%
      clearGroup("polygons") %>%
      addPolygons(
        data = shp,
        fillColor =~sapply(log_vals, custom_pal),
        weight = 1,
        opacity = 1,
        color = "lightgrey",
        dashArray = "3",
        fillOpacity = 0.7,
        label = ~labels,
        labelOptions = labelOptions(direction = 'auto',style = list("font-size" = "16px")),
        group = "polygons"
      )
  })


#########pop map hover labels end
  
  reactive_pop_data <- reactive({
    
    pop_data <- sf::st_drop_geometry(main_df) %>% 
      group_by(ADM1_EN, ADM2_EN, ADM2_PC) %>% 
      summarise(selected_population = sum(get(input$population_group_select), na.rm = TRUE), .groups = "drop")
    
    # Calculate total population
    total_pop <- sum(pop_data$selected_population, na.rm = TRUE)
    
    # Add percentage column
    pop_data <- pop_data %>%
      mutate(percentage = (selected_population / total_pop) * 100)
    
    # Sort by population and then calculate cumulative percentage
    pop_data <- pop_data %>%
      arrange(desc(selected_population)) %>%
      mutate(cumulative_percentage = cumsum(percentage))
    
    # Add an index column
    pop_data <- pop_data %>%
      mutate(index = row_number()) %>%
      select(index, everything())
    
    # Ungroup and add the total row
    pop_data <- pop_data %>%
      ungroup() %>%
      add_row(ADM2_EN = "Total", selected_population = total_pop, percentage = 100, cumulative_percentage = 100)
    
    return(pop_data)
  })
  

output$population_table <- renderTable({
  var_label <- ifelse(input$population_group_select %in% names(name_ranges), name_ranges[[input$population_group_select]], "")

  data<-reactive_pop_data()
  colnames(data)[which(colnames(data) == "selected_population")] <- var_label
  
  data[] <- lapply(data, function(x) format(x, big.mark = ",", scientific = FALSE, nsmall = 0))
  return(data)
}, sanitize.text.function = function(x) x)
  
######boxplot
reactive_pop_data_box_plot <- reactive({
  
  # First, calculate the percentages for each ADM2 within ADM1_EN
  pop_data_adm2 <- vul_df %>% 
    group_by(ADM1_EN, ADM2_EN) %>% 
    summarise(selected_population = sum(get(input$population_group_select), na.rm = TRUE), .groups = "drop")
  
  # Calculate total population
  total_pop <- sum(pop_data_adm2$selected_population, na.rm = TRUE)
  
  # Add percentage column
  pop_data_adm2 <- pop_data_adm2 %>%
    mutate(percentage = (selected_population / total_pop) * 100)
  
  gg <- ggplot(pop_data_adm2, aes(x = ADM1_EN, y = percentage)) +
    geom_boxplot() +
    theme_minimal() +
    labs(title = "Population Percentage Distribution by ADM1_EN", y = "Percentage", x = "ADM1_EN") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Convert the ggplot object to a plotly object
  p <- ggplotly(gg)
  
  return(p)
})

# Display the box plot in your shiny app
output$box_plot_pop <- renderPlotly({
  reactive_pop_data_box_plot()
})
  
  output$downloadPopData <- downloadHandler(
    filename = function() {
      paste("YemenPopdata_2023_", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      # Create the excel file
      write_xlsx(reactive_pop_data(), file)
    }
  )
  
  output$downloadMap1 <- downloadHandler(
    filename = function() {
      paste("Population_Map_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      # Access the reactive spatial object
      shp <- selected_shp() # This should work if selected_shp is a reactive expression, if it's a reactiveVal, use selected_shp()
      var_label <- ifelse(input$population_group_select %in% names(name_ranges), name_ranges[[input$population_group_select]], "")
      
      # ... (other leaflet layers and settings)
      vals <- as.numeric(shp[[input$population_group_select]])
      vals <- vals[!is.na(vals)]
      breaks <- quantile(vals, probs = seq(0, 1, by = 0.1), na.rm = TRUE)
      pal <- colorNumeric(palette = c("white", "red"), domain = vals)
      
      map <- leaflet(shp) %>%
        addProviderTiles(providers$Stamen.Toner) %>%
        addPolygons(
          fillColor = ~pal(as.numeric(vals)),
          weight = 2,
          opacity = 1,
          color = "white",
          dashArray = "3",
          fillOpacity = 0.7
        ) %>%
        addLegend(
          pal = pal,
          values = vals,
          labels = sprintf("%s - %s", format(round(breaks[-length(breaks)])), format(round(breaks[-1])))
        )
      
      # Now, you can use mapshot to save the map as a PDF
      mapview::mapshot(map, file = file, remove_controls = c("layersControl"))
    },
    contentType = "application/pdf"
  )
  
  
#########tab 2
  
  output$selectedVarTitle2 <- renderText({
    var_label <- ifelse(input$vul_var_select %in% names(name_ranges), name_ranges[[input$vul_var_select]], "")
    title_text <- paste0("Vulnerability Mapping at Admin level 2 - ", var_label)
    return(title_text)
  })
  
  output$vul_map <- renderLeaflet({
    # Access the reactive spatial object
    shp <- main_df
    var_label <- ifelse(input$vul_var_select %in% names(name_ranges), name_ranges[[input$vul_var_select]], "")
    title_text <- paste0("Admin level 2 - ", var_label)
    
    # Extract the values of the selected variable as a factor
    vals <- factor(shp[[input$vul_var_select]], levels = 1:5, ordered = TRUE)
    vals <- vals[!is.na(vals)]  # remove NAs
    # Create a color palette based on the levels of the ordered factor
    pal <- colorFactor(palette = c("black", "red", "orange", "yellow", "green"), domain = levels(vals))
    
  leaflet(shp) %>%
      addProviderTiles(providers$Stamen.Toner) %>%
      addPolygons(
        fillColor = ~pal(as.character(vals)),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        layerId = ~ADM2_PC
        # popup = ~paste0("<strong>P-code: </strong>", ADM2_PC, "<br>",
        #                 "<strong>Child Population: </strong>", child_t, "<br>",
        #                 "<strong>Total population: </strong>", total,"<br>",
        #                 "<strong>", var_label,": </strong>", child_t, shp[[input$vul_var_select]])
      ) %>%
      addLegend(
        pal = pal,
        values = levels(vals),
        title = var_label,
        opacity = 0.7
      )
  })
  
  # # Reactive expression to watch both map clicks and dropdown changes
  # info_reactive <- reactive({
  #   # Get the map click info
  #   click <- input$vul_map_shape_click
  #   # Return NULL if there hasn't been a click
  #   if(is.null(click)) return(NULL)
  #   
  #   # Get the selected variable from dropdown
  #   var <- input$vul_var_select
  #   
  #   # Get the value for the clicked region and the selected variable
  #   selected_value <- main_df[main_df$ADM2_PC == click$id, var, drop = TRUE]
  #   
  #   # Form the info string
  #   var_label <- ifelse(var %in% names(name_ranges), name_ranges[[var]], "")
  #   info <- sprintf("<strong>%s: </strong> %s", var_label, selected_value)
  #   
  #   list(lat = click$lat, lng = click$lng, info = info)
  # })
  # 
  # # Watch the reactive and update the popup whenever its value changes
  # observeEvent(info_reactive(), {
  #   info <- info_reactive()
  #   if(is.null(info)) return()
  #   
  #   leafletProxy("vul_map") %>%
  #     clearPopups() %>%
  #     addPopups(lat = info$lat, lng = info$lng, popup = info$info)
  # })

  
  
  # observeEvent(input$vul_map_shape_click, {
  #   click(input$vul_map_shape_click)
  # })
##### map hover code

  observe({
    label_lang <- input$label_lang
    map_proxy <- leafletProxy("vul_map")
    shp <- main_df
    labels <- if(label_lang == 'none') {
      rep("", nrow(shp)) # Assigning empty labels if 'none' is selected
    } else {
      as.character(shp[[label_lang]]) # Assigning corresponding labels
    }
    # Extract the values of the selected variable as a factor
    vals <- factor(shp[[input$vul_var_select]], levels = 1:5, ordered = TRUE)
    vals <- vals[!is.na(vals)]  # remove NAs
    # Create a color palette based on the levels of the ordered factor
    pal <- colorFactor(palette = c("black", "red", "orange", "yellow", "green"), domain = levels(vals))

    map_proxy %>%
      clearGroup("polygons") %>%
      addPolygons(
        data = shp,
        fillColor = ~pal(as.character(vals)),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        label = ~labels,
        labelOptions = labelOptions(direction = 'auto'),
        group = "polygons"
      )
  })
  
  
  reactive_vul_data <- reactive({
    var_label <- ifelse(input$vul_var_select %in% names(name_ranges), name_ranges[[input$vul_var_select]], "")
    vul_df <- vul_df %>% rename(!!var_label := !!sym(input$vul_var_select))
    vul_df %>% 
      group_by(!!sym(var_label)) %>%
      summarise('Number of districts'= n(), 'Vulnerable MPI children'=sum(dep_mpi, na.rm = TRUE),'Under-five pop' = sum(U5_t, na.rm = TRUE), 'children 5-17'=sum(c5_17t,na.rm=TRUE),'children 0-17'=sum(child_t,na.rm=TRUE),
      'Total population 0-17'=sum(total,na.rm=TRUE)) %>% 
      arrange('children 0-17')
    })
  

  output$selectedVarTitle3 <- renderText({
    var_label <- ifelse(input$vul_var_select %in% names(name_ranges), name_ranges[[input$vul_var_select]], "")
    title_text <- paste0("Vulnerability Mapping Summary tables population counts - ", var_label)
    return(title_text)
  })
  
  output$vul_table <- renderTable({
    var_label <- ifelse(input$vul_var_select %in% names(name_ranges), name_ranges[[input$vul_var_select]], "")
    
    data<-reactive_vul_data()
    colnames(data)[which(colnames(data) == "selected_population")] <- var_label
    
    data[] <- lapply(data, function(x) format(x, big.mark = ",", scientific = FALSE, nsmall = 0))
    return(data)
  }, sanitize.text.function = function(x) x)
}
# Run the application 
shinyApp(ui = ui, server = server)

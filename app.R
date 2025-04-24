library(shiny)
library(shinydashboard)
library(DT)
library(tidyverse)
library(readxl)
library(gsignal)
library(nls2)
library(shinyWidgets)
library(ggiraph)
library(openxlsx)
library(shinyjs)



# UI ----

ui <- dashboardPage(


  dashboardHeader(title = "PAP Waveform analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Load Waveform", tabName = "load", icon = icon("upload")),
      menuItem("Pre-occlusion", tabName = "pre-occ", icon = icon("backward")),
      menuItem("Post-occlusion", tabName = "post-occ", icon = icon("forward")),
      menuItem("Transition", tabName = "transition", icon = icon("x")),
      menuItem("Validation", tabName = "validation", icon = icon("check"))
    )
  ),
  dashboardBody(
    useShinyjs(),  
    tabItems(
      tabItem(tabName = "load",
              h2("Load a PAP waveform"),
              fileInput("file_wave", "Choose a XLSX File", accept = c(".xlsx")),
              textOutput("study_name"),
              textOutput("animal_number"),
              textOutput("timepoint"),
              
              h2("Raw data"),
              girafeOutput("raw_plot"),
              sliderInput("slider_window", "Smoothing factor:", min = 1, max = 100, value = 20),
              textOutput("t_pre_occlusion"),
              textOutput("t_post_occlusion")

      ),
      
      tabItem(tabName = "pre-occ",
              h2("Pre-occlusion"),
              plotOutput("plot_pre"),
              sliderInput("slider_threshold_pre", "Threshold peaks:", min = 1, max = 30, value = 10),
              actionButton("calculations_pre", "Calculate variables", icon = icon("play")),
              uiOutput("data_pre")
              

      ),
      
      tabItem(tabName = "post-occ",
              h2("Post-occlusion"),
              plotOutput("plot_post"),
              sliderInput("slider_threshold_post", "Threshold peaks:", min = 1, max = 30, value = 10),
              actionButton("calculations_post", "Calculate variables", icon = icon("play")),
              uiOutput("data_post")
             
      ),
      
      tabItem(tabName = "transition",
              h2("Transition"),
              girafeOutput("plot_occ", width = "100%", height = "600px"),
              textOutput("text_pcap"),
              textOutput("text_pcap_95"),
              textOutput("text_pcap_152")
              

      ),
      
      tabItem(tabName = "validation",
              h2("Data validation"),
              downloadButton("download_template", "Download Excel Template"),
              fileInput("file1", "Choose an existing dataset", accept = c(".xlsx")),
              DTOutput("table"),
              actionButton("append_btn", "Append Data", icon = icon("play")),  
              downloadButton("download_btn", "Download Final Excel", disabled = TRUE)  # Initially disabled
              
      )
    )
  )
)

# Server ----
server <- function(input, output) {
  


  ######### Loading files
  raw_data <- reactiveVal(NULL)
  raw_data_window <- reactiveVal()

  raw_data <- reactive({
    req(input$file_wave) 
    d <- read_excel(input$file_wave$datapath, sheet = "Ventilation") 
    names(d) <- c("time", "pap")
    
    window_size <- input$slider_window
    kernel <- rep(1 / window_size, window_size)  # Moving average kernel
    smoothed_y <- gsignal::filter(kernel, 1, d$pap)
  
    d$pap <- smoothed_y
    d <- d %>% slice(-c(1:50))

    d$time <- seq(1, nrow(d))

    return(d)
  })



  
  study_name_r <- reactiveVal()
  animal_number_r <- reactiveVal()
  hour_study_r <- reactiveVal()
  
  ### Identification of the file
  
  identification <- reactive({
    req(input$file_wave)
    s <- input$file_wave$name
    splits <- str_split(s, "_")

    study_name <- as.character(splits[[1]][1])
    animal_number <- as.character(splits[[1]][2])
    hour_study <- as.character(str_split(splits[[1]][length(splits[[1]])], "\\.")[[1]][1])
    
    study_name_r(study_name)
    animal_number_r(animal_number)
    hour_study_r(hour_study)
    
    ident_frame <- c(study_name, animal_number, hour_study)
    return(ident_frame)
    
  })
  
  string_name_study_calculated <- reactiveVal(FALSE)
  
  observeEvent(input$file_wave, {
    string_name_study_calculated(TRUE)
  })
  
  output$study_name <- renderText({
      if (string_name_study_calculated()){
        
    return(paste("Study name: ", identification()[1]))
      }else {
        return("Study name: ")
      }
  })
  
  output$animal_number <- renderText({
    if (string_name_study_calculated()){
      
      return(paste("Animal number: ", identification()[2]))
    }else {
      return("Animal number: ")
    }
  })
  
  output$timepoint <- renderText({
    if (string_name_study_calculated()){
      
      return(paste("Timepoint: ", identification()[3]))
    }else {
      return("Timepoint: ")
    }
  })
  
  #### Display raw plot
  
  output$raw_plot <- renderGirafe({
   p <- ggplot(as.data.frame(raw_data()), aes(x = time, y = pap))+
      theme_minimal()+
      geom_point_interactive(size = 0.1, color = "steelblue", aes(tooltip = paste("Time: ", time, "PAP: ", pap), data_id = time))+
     geom_line(size = 0.5, color = "steelblue")+
      labs(x = "Timepoints", y = "PAP (mmHg)")
    girafe(ggobj = p)
  })
  
  selected_point <- reactiveVal()

  observeEvent(input$raw_plot_selected, ({
    selected_point(as.vector(input$raw_plot_selected)) 
    
    
  output$t_pre_occlusion <- renderText({
      return(paste("Before occlusion: ", selected_point()[1]))
    })
  
  output$t_post_occlusion <- renderText({
    return(paste("After occlusion: ", selected_point()[2]))
  })

  
  }))

  
  ### Plot pre-occlusion
  
  data_peaks <- reactiveVal()
  d_pre <- reactiveVal()
  
  output$plot_pre <- renderPlot({
    
    d_pre_2 <- reactiveVal(raw_data() %>% 
      dplyr::filter(time < as.numeric(selected_point()[1])))
    
    peaks <- findpeaks(d_pre_2()$pap, DoubleSided = TRUE, MinPeakDistance = as.numeric(input$slider_threshold_pre))
    
    p <- peaks$pks
    l <- peaks$loc
    
    data_peaks(tibble(peaks = p, locations = l))
    
    d_pre(d_pre_2())
    
    ggplot(d_pre_2(), aes(x = time, y = pap))+
      theme_minimal()+
      geom_line(color = "steelblue")+
      geom_point(data = data_peaks(), aes(x = locations, y = peaks), size = 1.5)+
      labs(x = "Timepoints", y = "PAP (mmHg)")
    
  })

  paps<-reactiveVal()
  papd <- reactiveVal()
  ppv <- reactiveVal()
  pp_r <- reactiveVal()
  papm_r <- reactiveVal()
  
  observeEvent(input$calculations_pre, {
    
    median_peaks <- median(data_peaks()$peaks)
    systolics <- data_peaks()$peaks[data_peaks()$peaks > median_peaks]
    diastolics <- data_peaks()$peaks[data_peaks()$peaks < median_peaks]
    
    mean_systolics <- median(systolics)
    mean_diastolics <- median(diastolics)
    
    mean_pap = mean(d_pre()$pap)
    
    papm_r(mean_pap)  
      
    if (length(systolics) > length(diastolics)){
      systolics <- systolics[1:length(diastolics)]
    }else {
      diastolics <- diastolics[1:length(systolics)]
    }
    
    pulse_pressures <- systolics - diastolics
    pp <- median(pulse_pressures)
    pulse_pressure_variation <- max(pulse_pressures) - min(pulse_pressures)
    pulse_pressure_variation_percent <- (pulse_pressure_variation/((max(pulse_pressures) + min(pulse_pressures))/2))*100
    
    paps(mean_systolics)
    papd(mean_diastolics)
    ppv(pulse_pressure_variation_percent)
    pp_r(pp)
    
    output$data_pre <- renderUI({
      HTML(paste(
        "PAPs:", round(mean_systolics, 1), " mmHg", "<br>", 
        "PAPd:", round(mean_diastolics, 1), " mmHg", "<br>",
        "PAPm:", round(mean_pap, 1), " mmHg", "<br>",
        "PP:", round(pp, 1), " mmHg", "<br>",
        "PPV:", round(pulse_pressure_variation_percent, 1), "%"
      ))
      })
  })
  
  ### Plot post-occlusion
  
  data_peaks_post <- reactiveVal()
  max_min_r <- reactiveVal()
  d_post <- reactiveVal()
  wedge <- reactiveVal()
  
  output$plot_post <- renderPlot({
    
    d_post(raw_data() %>% 
                            dplyr::filter(time > as.numeric(selected_point()[2])))
    
    peaks <- findpeaks(d_post()$pap, DoubleSided = TRUE, MinPeakDistance = as.numeric(input$slider_threshold_post))
    
    p <- peaks$pks
    l <- peaks$loc
    
    data_peaks_post(tibble(peaks = p, locations = l+ as.numeric(selected_point()[2])))
    
    ggplot(d_post(), aes(x = time, y = pap))+
      theme_minimal()+
      geom_line(color = "steelblue")+
      geom_point(data = data_peaks_post(), aes(x = locations, y = peaks), size = 1.5)+
      labs(x = "Timepoints", y = "PAP (mmHg)")
    
  })

  observeEvent(input$calculations_post, {
    
    median_peaks <- median(data_peaks_post()$peaks)
    systolics <- data_peaks_post()$peaks[data_peaks_post()$peaks > median_peaks]
    diastolics <- data_peaks_post()$peaks[data_peaks_post()$peaks < median_peaks]
    
    mean_systolics <- median(systolics)
    mean_diastolics <- median(diastolics)
    
    if (length(systolics) > length(diastolics)){
      systolics <- systolics[1:length(diastolics)]
    }else {
      diastolics <- diastolics[1:length(systolics)]
    }
    
    
    max_min <- max(as.numeric(d_post()$pap)) - min(as.numeric(d_post()$pap))
    max_min_r(max_min)
    wedge(mean_diastolics)
    
    output$data_post <- renderUI({
      HTML(paste(
        "Wedge:", round(mean_diastolics, 1), " mmHg", "<br>",
        "Max - min occlusion:", round(max_min, 1), " mmHg"
      ))
    })
  })

  ### Transition
  
  selected_occlusion <- reactiveVal()
  paps_interval <- reactiveVal()
  t0 <- reactiveVal()
  dataset_prediction <- reactiveVal()
  paps_predicted_global <- reactiveVal()
  pcap <- reactiveVal()
  pcap_152 <- reactiveVal()
  pcap_95 <- reactiveVal()
  a_param <- reactiveVal()
  b_param <- reactiveVal()
  c_param <- reactiveVal()
  
  observeEvent(input$plot_occ_selected, ({
    selected_occlusion(as.vector(input$plot_occ_selected)) 
    
    t0((as.numeric(selected_occlusion())))
    
    tzero <- t0()
    
    
    t1 <- tzero+ 0.3*120
    t2 <- tzero+ 2* 120
    
    times_interval <- c(tzero, round(t1, 0), t2)
    paps_interval(raw_data()[raw_data()$time %in% times_interval, ])
    
    d_fitting <- raw_data() %>% 
      dplyr::filter(time >= t1) %>% 
      dplyr::filter(time <= t2) 
    
    t_begin <- d_fitting$time[1] -1
    
    d_fitting$time <- d_fitting$time - t_begin
    d_fitting$pap_scaled <- d_fitting$pap / max(d_fitting$pap)
    
    model_fitting <- nls2(pap_scaled ~ a * exp(-b * time) + c,
                          start = list(a = 1, b = 0.1, c = 1),
                          data = d_fitting, control = nls.control(maxiter = 100),
                          algorithm = "port") 
    
    predicted <- predict(model_fitting, newdata = data.frame(x = d_fitting$time))
    
    b_original <- coef(model_fitting)["b"] 
    a_original <- coef(model_fitting)["a"] * max(d_fitting$pap)
    c_original <- coef(model_fitting)["c"] * max(d_fitting$pap)
    
    a_param(a_original)
    b_param(b_original)
    c_param(c_original)
    
    time_extraction <- seq(from = 0, to = max(d_fitting$time)+ (t1-tzero), by = 1)
    
    paps_predicted_global(a_original * exp(-b_original * time_extraction) + c_original)
    
    dataset_prediction(tibble(time = time_extraction, pap = paps_predicted_global()))
    
    pcap(paps_predicted_global()[1])
    pcap_95(paps_predicted_global()[11])
    pcap_152(paps_predicted_global()[18]) # 152 ms sono 18 punti a 120 Hz, frequenza di acquisizion
    

    
    output$text_pcap <- renderText({
      return(paste("Pcap 0 ms: ", round(paps_predicted_global()[1],1), " mmHg"))
    })
    
    
    output$text_pcap_95 <- renderText({
      return(paste("Pcap 95 ms: ", round(paps_predicted_global()[11],1), " mmHg"))
    })
    
    output$text_pcap_152 <- renderText({
      return(paste("Pcap 152 ms: ", round(paps_predicted_global()[18],1), " mmHg"))
    })
    
    
  }))
  
  output$plot_occ <- renderPlot({
  
  d_occ <- reactiveVal(raw_data() %>% 
    dplyr::filter(time > as.numeric(selected_point()[1])) %>%
    dplyr::filter(time < as.numeric(selected_point()[2])))
  

  output$plot_occ <- renderGirafe({
    k <- ggplot(d_occ(), aes(x = time, y = pap))+
      theme_minimal()+
      geom_point_interactive(size = 0.5, color = "steelblue", aes(tooltip = paste("Time: ", time, "PAP: ", pap), data_id = time))+
      geom_line(size = 0.5, color = "steelblue")+
      labs(x = "Timepoints", y = "PAP (mmHg)")
      if (!is.null(t0())) {
        k <- k+geom_line(data = dataset_prediction(), aes(x = time+as.numeric(t0()), y = pap), linetype = "dashed", linewidth = 0.8)+
        geom_point(data = tibble(time = as.numeric(t0()), pap = as.numeric(paps_predicted_global()[1])), size = 2, color = "orange")+
          geom_point(data = tibble(time = as.numeric(t0()+11), pap = as.numeric(paps_predicted_global()[11])), size = 2, color = "darkgreen")+
        geom_point(data = tibble(time = as.numeric(t0()+18), pap = as.numeric(paps_predicted_global()[18])), size = 2, color = "indianred")
          
        }
      
    girafe(ggobj = k)
  })

  })
  
  
  #### Final table
  
  new_data <- reactive({
    
    data.frame(
      
      study = study_name_r(),
      id = animal_number_r(),
      hour = hour_study_r(),
      
      paps = paps(),
      papd = papd(),
      papm = papm_r(),
      pp = pp_r(),
      ppv = ppv(),
      a_param = a_param(),
      b_param = b_param(),
      c_param = c_param(),
      wedge = wedge(),
      swing_occ = max_min_r(),
      pcap_0 = pcap(),
      pcap_95 = pcap_95(),
      pcap_152 = pcap_152()
      
    )

  })
  

  # Reactive value to store the loaded data
  loaded_data <- reactiveVal(NULL)
  
  # Reactive value to store the file path of the uploaded file
  uploaded_file <- reactiveVal(NULL)
  
  # When an Excel file is uploaded, load the data and display it
  observeEvent(input$file1, {
    req(input$file1)  # Ensure the file is uploaded
    
    # Store the uploaded file path
    uploaded_file(input$file1$datapath)  # Save the uploaded file's path in the reactive variable
    
    # Load the Excel file into a data frame
    wb <- loadWorkbook(input$file1$datapath)
    sheet_data <- read.xlsx(wb, 1)  # Read the first sheet
    
    # Store the data in the reactive value
    loaded_data(sheet_data)
    
    # Display the table
    output$table <- renderDT({
      datatable(loaded_data(), options = list(pageLength = 5))
    })
  })
  
  
  # Action when the "Append New Data" button is clicked
  observeEvent(input$append_btn, {
    req(loaded_data())  # Ensure the data is loaded before appending
    
    # Get the current data from the reactive value
    current_data <- loaded_data()
    
    # Append the new data to the existing data
    updated_data <- rbind(current_data, new_data())
    
    # Update the reactive value with the new data
    loaded_data(updated_data)
    
    # Update the table display with the new data
    output$table <- renderDT({
      datatable(loaded_data(), options = list(pageLength = 5))
    })
    
    showNotification("New data has been appended", type = "message")
    
    # Enable the "Download Final Excel" button after appending data
    shinyjs::enable("download_btn")  # Enable the download button visually and functionally
  })
  
  # Download the final Excel file with the appended data
  output$download_btn <- downloadHandler(
    filename = function() {
      req(uploaded_file())  # Ensure that the uploaded file exists
      return(paste0(tools::file_path_sans_ext(basename(uploaded_file())), ".xlsx"))  # Ensure it's an .xlsx file
    },
    content = function(file) {
      req(loaded_data())  # Ensure data is available to save
      
      # Save the final data to the file (overwrite it)
      wb <- createWorkbook()
      addWorksheet(wb, "Sheet1")
      writeData(wb, "Sheet1", loaded_data())
      saveWorkbook(wb, file)  # Save to the provided file path (which will be the final downloaded file)
    }
  )
  
  
  output$download_template <- downloadHandler(
    filename = function() {
      "pcap_template.xlsx"
    },
    content = function(file) {
      # Create workbook and sheet
      wb <- createWorkbook()
      addWorksheet(wb, "Sheet1")
      
      # Define column headers
      headers <- data.frame(
        study = "",
        id = "",
        hour = "",
        paps = "",
        papd = "",
        papm = "",
        pp = "",
        ppv = "",
        a_param = "",
        b_param = "",
        c_param = "",
        wedge = "",
        swing_occ = "",
        pcap_0 = "",
        pcap_95 = "",
        pcap_152 = ""
      )
      
      # Write headers to Excel
      writeData(wb, sheet = "Sheet1", headers, startRow = 1, colNames = TRUE)
      
      # Save the workbook
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
}
  

# Run the app ----
shinyApp(ui, server)
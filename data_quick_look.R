#' APHH Merge Viewing App

ui <- fluidPage(
  #Layout
  sidebarLayout(
    sidebarPanel(
      fileInput("file1","Choose File",
                accept = c("text/csv","text/comma-separated-values,text/plain",".csv")
                ),
      checkboxInput(inputId = "campaign_split",label = "Split by Campaign?",value = T),
      conditionalPanel(condition = "input.campaign_split == true",
                       uiOutput("campaign_ui")
      ),
      tags$h5("Click Create Data to update options after changing the above/loading file"),
      actionButton(inputId = "create_data",label = "Create Data"),
      tags$h4("Multiple Species Comparison"),
      uiOutput("spec_grp_ui"),
      tags$h4("Regression Species"),
      uiOutput("spec1_ui"),
      uiOutput("spec2_ui"),
      tags$h5("Note: Species 1 is used for diurnal box plot"),
      conditionalPanel(condition = "input.campaign_split == true",
                       tags$h4("Wind Speed and Direction"),
                       uiOutput("wind_speed"),
                       uiOutput("wind_dir")
      ),
      actionButton(inputId = "update_plots",label = "Update Plots")
    ),
    
    mainPanel(
      tags$h1("APHH Merge Viewer"),
      tabsetPanel(
        tabPanel("Time Series", plotOutput("timeseries")),
        tabPanel("Diurnal", plotOutput("diurnal")),
        tabPanel("Diurnal Box",
                 plotOutput("diurnal_box"),
                 inputPanel(
                   sliderInput(inputId = "sd_range",label = "Limit y-axis by x number of standard deviations",min = 1,max = 10,step = 1,value = 5)
                 ),
                 wellPanel(
                   tags$h4("Standard Deviation"),
                   textOutput("sd_summary")
                 )
        ),
        
        tabPanel("Regression",
                 plotOutput("regression"),
                 uiOutput("reg_col"),
                 tags$h4("Solid regression line uses Deming regression from ",tags$code("MethComp::Deming")),
                 verbatimTextOutput("orth_sum_reg1"),
                 verbatimTextOutput("orth_sum_reg2"),
                 tags$h4("Dashed regression line uses linear regression from ",tags$code("stats::lm")),
                 verbatimTextOutput("lin_reg")
        ),
        tabPanel("Density",
                 plotOutput("density")),
        tabPanel("Polar Plot",
                 conditionalPanel(condition = "input.campaign_split == true",
                                  plotOutput("polar_plot"),
                                  tags$h5("Use Update plots after chainging statistics options. Polar plots can be slow to load"),
                                  selectInput(inputId = "polar_stat", label = "Statistic: ",
                                              choices = c("mean","median","max","stdev","weighted.mean","frequency","r","percentile"),
                                              selected = "mean"),
                                  tags$h4("Created using ",tags$code("openair::polarPlot")),
                                  conditionalPanel(condition = "input.polar_stat == 'percentile'",
                                                   sliderInput(inputId = "polar_percent",label = "Percentile: ",
                                                               min = 0,max = 100,step = 1,value = 100)
                                  )
                 ),
                 conditionalPanel(condition = "input.campaign_split == false",
                                  tags$h4("Polar Plot unavaliable, split by Campaign to use")
                                  )
                 )
        )
      )
    )
  )



server <- function(input, output,session) {
  options(shiny.maxRequestSize=30*1024^2) 
  #Load Packages
  library(ggplot2)
  library(MethComp)
  library(magrittr)
  library(lubridate)
  library(reshape2)
  library(openair)
  library(viridisLite)
  library(dplyr)
  library(shiny)
  library(stringr)

  #Format Reactive Data
  raw_list = reactive({
    req(input$file1)
    raw_data = read.csv(input$file1$datapath)
    raw_data$date %<>% ymd_hms
    names(raw_data) = tolower(names(raw_data))
    campaign_choices = levels(raw_data$campaign)
    raw_list = list(
      raw_data = raw_data,
      campaign_choices = campaign_choices
    )
  })
  
  data_list = reactiveValues()
  observeEvent(input$create_data,{
    wide_data = raw_list()$raw_data
    if(input$campaign_split){
      
      wide_data = wide_data[wide_data$campaign == input$campaign,]
      #Drop columns that are all NA
      wide_data = wide_data[,colSums(is.na(wide_data))<nrow(wide_data)]
      wide_data = wide_data[,-which(names(wide_data) %in% c("campaign"))]
      long_data = reshape2::melt(wide_data,id.vars = "date")
      long_data$value = as.numeric(long_data$value)
      wide_data$hour = hour(wide_data$date)
      mychoices = names(wide_data)[!names(wide_data) %in% c("hour","date")]
    }else{
      new_wide = data.frame(date = wide_data$date)
      for (i in 1:length(raw_list()$campaign_choices)){
        temp = wide_data[wide_data$campaign == raw_list()$campaign_choices[i],]
        #Drop columns that are all NA
        temp = temp[,colSums(is.na(temp))<nrow(temp)]
        temp = temp[,-which(names(temp) %in% c("campaign"))]
        names(temp) = paste0(names(temp),"_",raw_list()$campaign_choices[i])
        names(temp)[grep(paste0("^date_",raw_list()$campaign_choices[i],"$"),names(temp))] ="date"
        new_wide = left_join(new_wide,temp,"date")
      }
      wide_data = new_wide
      long_data = reshape2::melt(wide_data,"date")
      long_data$value = as.numeric(long_data$value)
      wide_data$hour = hour(wide_data$date)
      mychoices = names(wide_data)[!names(wide_data) %in% c("hour","date")]
    }
    
    data_list$wide = wide_data
    data_list$long = long_data
    data_list$choices = mychoices
  }
  )
  
  orth_reg = reactive({
    MethComp::Deming(data_list$wide[,input$spec1],data_list$wide[,input$spec2])
  })
  
  lin_reg = reactive({
    lm(data_list$wide[,input$spec2] ~ data_list$wide[,input$spec1])
  })
  
  group_melt = reactive({
    data_list$long[data_list$long$variable %in% input$spec_grp,]
  })
  
  group_diurnal = reactive({
    openair::timeVariation(data_list$wide, pol = input$spec_grp)
  })
  
  
  colours  = reactive({
    viridisLite::viridis(length(input$spec_grp))
  })
  
  fill_col = "grey97"
  line_col = "black"
  gen_theme = theme(
    plot.background = element_rect(fill = fill_col, colour = "grey92"),
    panel.background = element_rect(fill = fill_col, colour = fill_col),
    panel.grid.major = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text = element_text(colour = line_col,size = 20,face = "bold"),
    axis.title = element_text(colour = line_col,size = 25),
    axis.ticks = element_line(colour = line_col),
    axis.line = element_line(colour = line_col),
    panel.border = element_rect(colour = line_col,fill = NA)
  )
  
  leg_theme =  theme(
    legend.position = "bottom",
    legend.text = element_text(colour = line_col,size = 20),
    legend.title = element_blank(),
    legend.key = element_blank(),
    legend.background = element_rect(fill = fill_col, colour = fill_col)
  )
  #create reactive ui
  output$campaign_ui = renderUI({
    selectInput(inputId = "campaign", label = "Campaign: ",
                choices = raw_list()$campaign_choices, selected = raw_list()$campaign_choices[1])
  })
  output$spec_grp_ui = renderUI({
    selectInput(inputId = "spec_grp", label = NULL,
                       choices = data_list$choices,selected = data_list$choices[c(1:2)],multiple = T)
  })
  
  output$spec1_ui = renderUI({
    selectInput(inputId = "spec1", label = "Species 1:",
                choices = data_list$choices, selected = data_list$choices[1])
  })
  
  output$spec2_ui = renderUI({
    selectInput(inputId = "spec2", label = "Species 2:",
                choices = data_list$choices, selected = data_list$choices[2])
  })
  
  wd_col = reactive({which(data_list$choices == data_list$choices[str_detect(tolower(data_list$choices),"wd")][1])})
  ws_col = reactive({which(data_list$choices == data_list$choices[str_detect(tolower(data_list$choices),"ws")][1])})
  
  if(!is.null(ws_col)){
    output$wind_speed = renderUI({
      selectInput(inputId = "wind_speed", label = "Wind Speed:",
                  choices = data_list$choices, selected = data_list$choices[ws_col()])
    })
  }else{
    output$wind_speed = renderUI({
      selectInput(inputId = "wind_speed", label = "Wind Speed:",
                  choices = data_list$choices, selected = data_list$choices[1])
    })
  }
  
  if(!is.null(wd_col)){
    output$wind_dir = renderUI({
      selectInput(inputId = "wind_dir", label = "Wind Direction:",
                  choices = data_list$choices, selected = data_list$choices[wd_col()])
    })
  }else{
    output$wind_dir = renderUI({
      selectInput(inputId = "wind_dir", label = "Wind Direction:",
                  choices = data_list$choices, selected = data_list$choices[2])
    })
  }
  output$reg_col = renderUI({
    selectInput(inputId = "reg_col", label = "Colour by: ",
              choices = c("none",data_list$choices),
              selected = "none")
  })
  
  
  #Create Stats
  output$sd_summary = reactive({
    input$update_plots
    print("Standard Deviation")
    sd(isolate({data_list$wide[,input$spec1]}),na.rm = T)
  })
  
  output$orth_sum_reg1 = renderPrint({
    input$update_plots
    summary(isolate({orth_reg()}))
  })
  
  output$orth_sum_reg2 = renderPrint({
    input$update_plots
    isolate({orth_reg()})
  })
  
  output$lin_reg = renderPrint({
    input$update_plots
    isolate({summary(lin_reg())})
  })
  
  #Create Options
  my_ymax = reactive({
    mean(data_list$wide[,input$spec1],na.rm = T)+(input$sd_range*sd(data_list$wide[,input$spec1],na.rm = T))
  })
  
  
  #Create Plots
  
  output$polar_plot = renderPlot({
    input$update_plots
    if(isolate({input$polar_stat}) != "percentile"){
      x = openair::polarPlot(isolate({data_list$wide}),
                             pol = isolate({input$spec_grp}),cols = viridisLite::viridis(200),
                             x = isolate({input$wind_speed}),wd = isolate({input$wind_dir}),
                             statistic = isolate({input$polar_stat}))
    }else{
      x = openair::polarPlot(isolate({data_list$wide}),
                             pol = isolate({input$spec_grp}),cols = viridisLite::viridis(200),
                             x = isolate({input$wind_speed}),wd = isolate({input$wind_dir}),
                             statistic = isolate({input$polar_stat}),percentile = isolate({input$polar_percent}))
    }
    x = x$plot
    x
  })
  
  output$timeseries = renderPlot({
      input$update_plots
      ggplot(
        isolate({group_melt()}))+
        geom_point(aes(date,value,group = variable,col = variable))+
        ylab("")+
        xlab("Date")+
        scale_colour_manual(values = isolate({colours()}))+
        gen_theme+
        leg_theme+
        guides(color = guide_legend(override.aes = list(size=5)))
    })

  
  output$diurnal = renderPlot({
    input$update_plots
    ggplot(
      isolate({group_diurnal()$data$hour}))+
      geom_point(aes(hour,Mean,group = variable, colour = variable),size = 2)+
      geom_line(aes(hour,Mean,group = variable, colour = variable),size = 1)+
      xlab("Hour of Day")+
      ylab("")+
      scale_colour_manual(values = isolate({colours()}))+
      gen_theme+
      leg_theme
  })
  
  output$diurnal_box = renderPlot({
    input$update_plots
    ggplot(
      isolate({data_list$wide}))+
      geom_boxplot(aes_string(x = "hour",y = input$spec1, group = "hour"))+
      ylim(NA,my_ymax())+
      gen_theme
  })
  
  output$regression = renderPlot({
    input$update_plots
    if(input$reg_col == "none"){
      ggplot(
        isolate({data_list$wide}))+
        geom_point(
          aes_string(
            isolate({input$spec1}),
            isolate({input$spec2})
            ),
          size = 4,
          alpha = 0.5)+
        geom_abline(aes(intercept = isolate({orth_reg()[[1]]}),
                        slope = isolate({orth_reg()[[2]]}))
        )+
        geom_abline(aes(intercept = isolate({lin_reg()[[1]][1]}),
                        slope = isolate({lin_reg()[[1]][2]})),lty = 2
        )+
        xlab(isolate({input$spec1}))+
        ylab(isolate({input$spec2}))+
        gen_theme+
        theme(legend.position = "none")
    }else{
      ggplot(
        isolate({data_list$wide}))+
        geom_point(
          aes_string(
            isolate({input$spec1}),
            isolate({input$spec2}),
            col = input$reg_col),
          size = 4,
          alpha = 0.5)+
        geom_abline(aes(intercept = isolate({orth_reg()[[1]]}),
                        slope = isolate({orth_reg()[[2]]}))
        )+
        geom_abline(aes(intercept = isolate({lin_reg()[[1]][1]}),
                        slope = isolate({lin_reg()[[1]][2]})),lty = 2
        )+
        scale_color_gradientn(colours = viridisLite::viridis(200))+
        xlab(isolate({input$spec1}))+
        ylab(isolate({input$spec2}))+
        gen_theme+
        theme(legend.position = "none")
    }
    
    
  })
  
  output$density = renderPlot({
    input$update_plots
    ggplot(
      isolate({group_melt()}))+
      geom_density(aes(value,group = variable,fill = variable,lty = variable),alpha = 0.6,size = 1)+
      ylab("Density")+
      xlab("")+
      scale_fill_manual(values = isolate({colours()}))+
      gen_theme+
      leg_theme
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)


ui<-dashboardPage(
  dashboardHeader(title="Agriculture Dashboard",dropdownMenu(
    type = "messages",
    messageItem(from = "Madeleine",
                message = "Check out the Data Source!",
                href = "https://data.oecd.org/"
    )
  )
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem(text="Crop Produce",tabName="crop",icon=icon("leaf"),
               menuSubItem(text="Summary by Country",tabName="crop-2",icon=icon("globe-asia")),
               menuSubItem(text="Top Producers",tabName = "general-2",icon = icon("trophy"))),
      menuItem(text="Meat Consumption",tabName="meat",icon=icon("bacon"),
               menuSubItem(text="Summary by Country",tabName = "meat-2",icon=icon("globe-asia")),
               menuSubItem(text="Top Consumers",tabName = "general-1",icon = icon("trophy"))),
      menuItem(text="General Overview",tabName="general",icon=icon("globe")))),
  dashboardBody(
    shinyDashboardThemes(theme = "grey_dark"),
    tabItems(
      tabItem(tabName = "general",
              fluidPage(fluidRow(tags$h3("Agriculture Produce Consumed and Produced",align="center")),
                        fluidRow(box(width=12,column(width=3,selectInput(inputId = "input_gen",label="Select Year",selected = 2019,choices = c(1990:2019))),
                                 column(width=9,plotlyOutput("plot_general")))),
                        fluidRow(tags$h3("General Map",align="center")),
                        fluidRow(box(width=12,column(width=3,selectInput(inputId = "input_gen1",label="Select Year",selected = 2019,choices = c(1990:2019))),
                                     column(width=9,leafletOutput("leaflet_general")))))),
      
      
      
      tabItem(tabName = "crop-2",fluidPage(fluidRow(
        column(width=2,
               selectInput(inputId = "input_3",label="Select Country",selected = "Worldwide",choices = distinct_country))),
        fluidRow(
          valueBoxOutput("valuebox_1",width=3),
          valueBoxOutput("valuebox_2",width=3),
          valueBoxOutput("valuebox_3",width=3),
          valueBoxOutput("valuebox_4",width=3)),
        fluidRow(column(width=3,(box(width=12,title="World Ranking",background  ="navy", valueBoxOutput("rankbox_1",width=12),
                                     valueBoxOutput("rankbox_2",width=12),
                                     valueBoxOutput("rankbox_3",width=12),
                                     valueBoxOutput("rankbox_4",width=12)))),
                 column(width=9,box(width=12,title="Crop Produced",plotlyOutput("plot_p12")))),
        fluidRow(tags$h5("*All Measurements are in Thousand Metric Tonnes",align = "right")))),
      tabItem(tabName = "meat-2",
              fluidPage(fluidRow(column(width=2,
                                        selectInput(inputId = "input_4",label="Select Country",selected = "Australia",choices = distinct_country))),
                        fluidRow(
                          valueBoxOutput("valuebox_5",width=3),
                          valueBoxOutput("valuebox_6",width=3),
                          valueBoxOutput("valuebox_7",width=3),
                          valueBoxOutput("valuebox_8",width=3)),
                        fluidRow(column(width=3,(box(width=12,title="World Ranking",background  ="navy" ,valueBoxOutput("rankbox_5",width=12),
                                                     valueBoxOutput("rankbox_6",width=12),
                                                     valueBoxOutput("rankbox_7",width=12),
                                                     valueBoxOutput("rankbox_8",width=12)))),
                                 column(width=9,(box(width=12,title="Crop Produced",plotlyOutput("plot_p11"))))),
                        fluidRow(tags$h5("*All Measurements are in Thousand Metric Tonnes",align = "right")))),
      tabItem(tabName = "general-1",
              fluidPage(
                fluidRow(infoBoxOutput("infobox_beef",width=3),infoBoxOutput("infobox_poultry",width=3),infoBoxOutput("infobox_sheep",width=3),
                         infoBoxOutput("infobox_pork",width=3)),
                
                fluidRow(tags$h2('Top Consumers Summary',align="center")),
                
                fluidRow(
                  column(width=3,box(width=12,status = "info",sliderInput("slider2", label = h5("Select Year Range"), min = 1990, 
                                                                          max = 2019, value = c(2010, 2019),sep = ""),
                                     checkboxGroupInput("checkGroup", label = h5("Select Meat"), 
                                                        choices = list("Beef" = "Beef", "Poultry" = "Poultry", "Pork" = "Pork","Sheep"="Sheep"), selected = "Beef"),
                                     sliderInput("slider1", label = h5("Select Top N"), min = 0, max = 20, value = 10) )),
                  column(width=9,box(width=12,background = "navy",plotlyOutput("plot_p13")))),
                fluidRow(tags$h2('Meat Consumers Map',align="center")),
                fluidRow(box(width=12,status="danger"),column(width=3,radioButtons("checkmap", label = h5("Select Meat"), 
                                                                                   choices = list("Beef" = "Beef", "Poultry" = "Poultry", "Pork" = "Pork","Sheep"="Sheep"),
                                                                                   selected = "Beef"),
                                                              
                                                              sliderInput("slidermap", label = h5("Select Year Range"), min = 1990, 
                                                                          max = 2019, value = 2019,sep="")),
                         column(width=9,leafletOutput("plot_p15"))),
                fluidRow(tags$h5("*All Measurements are in Thousand Metric Tonnes",align = "right")))),
      tabItem(tabName = "general-2",
              fluidPage(
                fluidRow(infoBoxOutput("infobox_wheat",width=3),infoBoxOutput("infobox_soybean",width=3),infoBoxOutput("infobox_rice",width=3),infoBoxOutput("infobox_maize",width=3)),
                fluidRow(tags$h2('Top Producers Summary',align="center")),
                
                fluidRow(
                  column(width=3,box(width=12,status = "info",sliderInput("slider3", label = h5("Select Year Range"), min = 1990, 
                                                                          max = 2019, value = c(2010, 2019),sep = ""),
                                     checkboxGroupInput("checkGroup1", label = h5("Select Meat"), 
                                                        choices = list("Maize" = "Maize", "Soybean" = "Soybean", "Rice" = "Rice","Wheat"="Wheat"),
                                                        selected = "Maize"),
                                     sliderInput("slider4", label = h5("Select Top N"), min = 0, 
                                                 max = 20, value = 10))),
                  column(width=9,box(width=12,background = "navy",plotlyOutput("plot_p14")))),
                fluidRow(tags$h2('Crop Producers Map',align="center")),
                
                fluidRow(box(width=12,status="danger",column(width=3,radioButtons("checkmap2", label = h5("Select Crop"), 
                                                                                  choices = list("Maize" = "Maize", "Soybean" = "Soybean", "Rice" = "Rice","Wheat"="Wheat"),
                                                                                  selected = "Maize"),
                                                             
                                                             sliderInput("slidermap2", label = h5("Select Year"), min = 1990, 
                                                                         max = 2019, value = 2019,sep="")),
                             column(width=9,leafletOutput("plot_p16")))),
                fluidRow(tags$h5("*All Measurements are in Thousand Metric Tonnes",align = "right")))))))




      

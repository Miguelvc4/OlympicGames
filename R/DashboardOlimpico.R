
#' @title Dashboard Olimpico
#' @description Esta funcion permite generar un  Dashboard con una serie de visualizaciones sobre los datos historicos de los juegos olimpicos.
#' @details Esta funcion permite generar un dashboard interactivos con una serie de visualizaciones y filtros de lso datos historicos de los juegos olimpicos desde la edicion de Atenas 1896.
#' @export
#' @import shiny
#' @import tidyverse
#' @import shinydashboard
#' @import htmlwidgets
#' @import downloader
#' @importFrom plotly plotlyOutput
#' @importFrom plotly renderPlotly
#'

DashboardOlimpico <- function()
{

  #data("DatosResumen")
  #data("DatosOlimpicos")
  #data("DatosUnificados")
  #data("DatosEdad")

  #download("https://github.com/ramnathv/rCharts/archive/master.tar.gz", "rCharts.tar.gz")
  #install.packages("rCharts.tar.gz", repos = NULL, type = "source")
  #library(rCharts)

  ui <-
    ########
  dashboardPage(skin = "blue",

                dashboardHeader(title = "120 Years of Olympic Games Data",titleWidth = 350),

                dashboardSidebar(
                  sidebarUserPanel("Miguel Velasquez",
                                   image = "http://estrategia.info/cath/wp-content/uploads/2013/10/anillos-olimpicos1.png"
                  ),

                  sidebarMenu(
                    menuItem("Datos Completos", tabName = "Datos", icon = icon("th")),
                    menuItem("Analisis General", tabName = "AnalisisGeneral", icon = icon("history")),
                    menuItem("Analisis Por Genero", tabName = "AnalisisGenero", icon = icon("venus-mars")),
                    menuItem("Peso VS Altura", tabName = "CorrelacionPesoAltura", icon = icon("project-diagram")),
                    menuItem("Top Paises", tabName = "TopPaises", icon = icon("medal")),
                    menuItem("World Map", tabName = "WorldMap", icon = icon("globe-americas")),
                    menuItem("Correlacion PIB y Medallas", tabName = "CorrelacionPIB", icon = icon("dollar-sign")),
                    menuItem("Distribucion por Edad", tabName = "DistribucionEdad", icon = icon("chart-bar"))
                  )
                ),
                dashboardBody(
                              tabItems(

                                ##############################################
                                #DATOS
                                ##############################################
                                tabItem(tabName = "Datos",
                                        titlePanel("Datos Historicos"),
                                        fluidRow(column(DT::dataTableOutput("DatosOlimpicos"),
                                                        width = 12)),
                                        br(),

                                        fluidRow(downloadButton('downloadData', 'Download Full Data',
                                                                style="display: block; margin: 0 auto; width: 230px;color: black;"),
                                                 width = 12)
                                ),

                                ##############################################
                                # ANALISIS GENERAL
                                ##############################################
                                tabItem(tabName = "AnalisisGeneral",
                                        titlePanel("Analisis General"),
                                        sidebarLayout( #To have a personalized sidebar per tab
                                          sidebarPanel(
                                            sliderInput("Years_AG", "Year range",
                                                        min(DatosOlimpicos$Year), max(DatosOlimpicos$Year),
                                                        value = c( min(DatosOlimpicos$Year),
                                                                   max(DatosOlimpicos$Year)),
                                                        step = 4),

                                            radioButtons("Filtro_AG",
                                                         label="Select Filter:",
                                                         choices=c("All" = "all",
                                                                   "By Country" = "country",
                                                                   "By Sport" = "sport"),
                                                         selected="all"),

                                            uiOutput(outputId = 'Season_AG'),

                                            uiOutput(outputId = 'Country_AG'),

                                            uiOutput(outputId = 'Sport_AG')

                                          ),

                                          mainPanel(box(solidHeader = TRUE, collapsible = TRUE, width = 300,
                                                        showOutput("VisualizacionAtletas", "highcharts")),
                                                    box(solidHeader = TRUE,collapsible = TRUE, width = 300,
                                                        showOutput("VisualizacionNaciones", "highcharts")),
                                                    showOutput("VisualizacionEventos", "highcharts"))
                                        )
                                ),

                                ##############################################
                                # ANALISIS POR GENERO
                                ##############################################
                                tabItem(tabName = "AnalisisGenero",
                                        #titlePanel("ANaLISIS POR GeNERO"),
                                        sidebarLayout(
                                          sidebarPanel(
                                            sliderInput("Years_Gen", "Year range:",
                                                        min(DatosOlimpicos$Year), max(DatosOlimpicos$Year),
                                                        value = c(min(DatosOlimpicos$Year),
                                                                  max(DatosOlimpicos$Year)),
                                                        step = 4),

                                            radioButtons("Filtro_Gen",
                                                         label="Select Filter:",
                                                         choices=c("All" = "all_gen",
                                                                   "By Country" = "country_gen",
                                                                   "By Sport" = "sport_gen"),
                                                         selected="all_gen"),

                                            uiOutput(outputId = 'Country_Gen'),

                                            uiOutput(outputId = 'Sport_Gen')
                                          ),
                                          mainPanel(showOutput("VisualizacionGenero", "highcharts"),br(),

                                                    fluidRow(
                                                      valueBoxOutput("numDeportistas", width = 4),
                                                      valueBoxOutput("numHombres", width = 4),
                                                      valueBoxOutput("numMujeres", width = 4)
                                                    )
                                          )

                                        )

                                ),

                                ##############################################
                                # PESO vs ALTURA
                                ##############################################
                                tabItem(tabName = "CorrelacionPesoAltura",
                                        sidebarLayout(
                                          sidebarPanel(
                                            sliderInput("Years_PA", "Year range:",
                                                        min(DatosOlimpicos$Year), max(DatosOlimpicos$Year),
                                                        value = c(min(DatosOlimpicos$Year),
                                                                  max(DatosOlimpicos$Year)),
                                                        step = 4),

                                            selectInput(inputId = "Sport_PA",
                                                        label = "Select Sport:",
                                                        choices = levels(DatosOlimpicos$SportsCompetition),
                                                        selected = levels(DatosOlimpicos$SportsCompetition)[2],
                                                        multiple = FALSE),

                                            selectInput(inputId = "Gender_PA",
                                                        label = "Select Gender:",
                                                        choices = levels(DatosOlimpicos$Gender),
                                                        selected = levels(DatosOlimpicos$Gender)[1:2],
                                                        multiple = TRUE)
                                          ),
                                          mainPanel(showOutput("Correlacion", "highcharts"),br(),

                                                    fluidRow(
                                                      infoBoxOutput("medalGold", width = 4),
                                                      infoBoxOutput("medalSilver", width = 4),
                                                      infoBoxOutput("medalBronze", width = 4)
                                                    )
                                          )
                                        )
                                ),

                                ##############################################
                                # TOP Country
                                ##############################################
                                tabItem(tabName = "TopPaises",
                                        sidebarLayout( #To have a personalized sidebar per tab
                                          sidebarPanel(

                                            selectInput(inputId = "Sport_TC",
                                                        label = "Select Sport:",
                                                        choices = levels(DatosOlimpicos$SportsCompetition),
                                                        selected = levels(DatosOlimpicos$SportsCompetition)[2],
                                                        multiple = FALSE),
                                            uiOutput(outputId = 'Region_TC'),

                                            selectInput(inputId = "Gender_Tc",
                                                        label = "Select Gender:",
                                                        choices = levels(DatosOlimpicos$Gender),
                                                        selected = levels(DatosOlimpicos$Gender)[1:2],
                                                        multiple = TRUE)
                                          ),
                                          mainPanel(plotlyOutput("BarrasAgrupadas", height = 600))
                                        )
                                ),

                                ##############################################
                                # WORLD MAP
                                ##############################################
                                tabItem(tabName = "WorldMap",
                                        sidebarLayout(
                                          sidebarPanel(


                                            sliderInput("Years_WM", "Year range:",
                                                        min(DatosOlimpicos$Year), max(DatosOlimpicos$Year),
                                                        value = c(min(DatosOlimpicos$Year),
                                                                  max(DatosOlimpicos$Year)),
                                                        step = 4)


                                          ),
                                          mainPanel(br(),br(),plotlyOutput("Worldmap",height = "auto"))

                                        )
                                ),

                                ##############################################
                                #CORRELACION PIB TOTAL MEDALLAS
                                ##############################################
                                tabItem(tabName = "CorrelacionPIB",
                                        sidebarLayout(
                                          sidebarPanel(
                                            sliderInput("Years_PIB", "Year range:",
                                                        min(DatosUnificados$Year), max(DatosUnificados$Year),
                                                        value = c(min(DatosUnificados$Year),
                                                                  max(DatosUnificados$Year)),
                                                        step = 4)
                                          ),
                                          mainPanel(showOutput("CorrelacionPIB", "highcharts"),

                                                    fluidRow(column(width = 4, offset = 5,
                                                                    valueBoxOutput("ValCorrelacion",width = NULL)))
                                          )
                                        )
                                ),

                                ##############################################
                                #DISTRIBUCION EDAD
                                ##############################################
                                tabItem(tabName = "DistribucionEdad",
                                        sidebarLayout(
                                          sidebarPanel(
                                            sliderInput("Years_Edad", "Year range:",
                                                        min(DatosEdad$Year), max(DatosEdad$Year),
                                                        value = c(min(DatosEdad$Year),
                                                                  max(DatosEdad$Year)),
                                                        step = 4),

                                            sliderInput("Rango_Edad", "Year range:",
                                                        min(DatosEdad$Age), max(DatosEdad$Age),
                                                        value = c(min(DatosEdad$Age),
                                                                  max(DatosEdad$Age)),
                                                        step = 1),

                                            radioButtons("Filtro_Edad",
                                                         label="Select Filter:",
                                                         choices=c("All" = "all_Edad",
                                                                   "By Country" = "country_Edad",
                                                                   "By Sport" = "sport_Edad"),
                                                         selected="all_Edad"),

                                            uiOutput(outputId = 'Country_Edad'),

                                            uiOutput(outputId = 'Sport_Edad'),

                                            selectInput(inputId = "Gender_Edad",
                                                        label = "Select Gender:",
                                                        choices = levels(DatosEdad$Gender),
                                                        selected = levels(DatosEdad$Gender)[1:2],
                                                        multiple = TRUE)

                                          ),
                                          mainPanel(br(),br(),showOutput("VisualizacionEdad", "highcharts")
                                          )

                                        )
                                )



                              )
                )
  )


  server <- function(input, output)
  {
    output$DatosOlimpicos <- DT::renderDataTable(
      DT::datatable({DatosResumen},
                    options= list(lengthMenu= list(c(5,10,25,50,100,1),
                                                   c('5','10','25','50','100','Todo'))
                                  ,pagelength=10,dom = "Blfrtip"
                                  , buttons = list("copy", "print",list(extend = "collection"
                                                                        , buttons = c("csv", "excel", "pdf"),text = "Download"
                                  ))),
                    selection = 'multiple',
                    style='bootstrap',
                    extensions =  c('Buttons','Responsive'),
                    class = 'cell-border stripe',
                    filter = 'top'
      ),
    )


    output$downloadData <- downloadHandler(
      filename = function() {
        paste("OlympicGamesData-", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv(DatosResumen, file)
      })


    ####################################################################
    # ANALISIS GENERAL
    ####################################################################

    observeEvent(input$Filtro_AG, {
      if (input$Filtro_AG == 'country')
      {
        output$Sport_AG <- renderUI(NULL)

        output$Country_AG <-
          renderUI(
            selectInput(inputId='Country_AG',label='Country',
                        selected = levels(DatosOlimpicos$Region)[1],
                        multiple = TRUE,
                        choices = levels(DatosOlimpicos$Region)
            )
          )

        output$Season_AG <-
          renderUI(
            selectInput(inputId='Season_AG',label='Season',
                        selected = levels(DatosOlimpicos$Season)[1],
                        multiple = FALSE,
                        choices = levels(DatosOlimpicos$Season)
            )
          )
      }
      else  if (input$Filtro_AG == 'sport')
      {
        output$Country_AG <- renderUI(NULL)
        output$Season_AG <- renderUI(NULL)

        output$Sport_AG <-
          renderUI(
            selectInput(inputId='Sport_AG',label='Sport',
                        selected = levels(DatosOlimpicos$SportsCompetition)[2],
                        multiple = TRUE,
                        choices = levels(DatosOlimpicos$SportsCompetition)
            )
          )
      }
      else
      {
        output$Country_AG <- renderUI(NULL)
        output$Season_AG <- renderUI(NULL)
        output$Sport_AG <- renderUI(NULL)
      }
    })

    #==================================ATLETAS==================================
    output$VisualizacionAtletas <- renderChart2({
      if (input$Filtro_AG == 'country')
      {
        DatosAtletas <- DatosOlimpicos  %>%
          filter(Year >= input$Years_AG[1] & Year <= input$Years_AG[2]
                 & Season %in% input$Season_AG & Region %in% input$Country_AG) %>%
          group_by(Year,Region) %>%
          summarize(
            Athletes = length(unique(ID))
          )

        GraficoAtletas <- hPlot(x = "Year", y = "Athletes",
                                group = "Region",
                                data = DatosAtletas,
                                type = 'line')

        GraficoAtletas$title(text = "Athletes Over Time")
        chart_width <- 0.95*as.numeric(JS('window.innerWidth'))
        GraficoAtletas$set(width = chart_width)
        GraficoAtletas$exporting(enabled = T)
        GraficoAtletas$yAxis(title=list(text="Athletes ",align='high', offset= 0,rotation=0,y= -20))

        return(GraficoAtletas)
      }else if (input$Filtro_AG == 'sport')
      {
        DatosAtletas <- DatosOlimpicos  %>%
          filter(Year >= input$Years_AG[1] & Year <= input$Years_AG[2]
                 & SportsCompetition %in% input$Sport_AG) %>%
          group_by(Year,SportsCompetition) %>%
          summarize(
            Athletes = length(unique(ID))
          )

        GraficoAtletas <- hPlot(x = "Year", y = "Athletes",
                                group = "SportsCompetition",
                                data = DatosAtletas,
                                type = 'line')
        GraficoAtletas$title(text = "Athletes Over Time")
        chart_width <- 0.95*as.numeric(JS('window.innerWidth'))
        GraficoAtletas$set(width = chart_width)
        GraficoAtletas$exporting(enabled = T)
        GraficoAtletas$yAxis(title=list(text="Athletes ",align='high', offset= 0,rotation=0,y= -20))
        return(GraficoAtletas)
      }else
      {
        DatosAtletas <- DatosOlimpicos  %>%
          filter(Year >= input$Years_AG[1] & Year <= input$Years_AG[2]) %>%
          group_by(Year, Season) %>%
          summarize(
            Athletes = length(unique(ID))
          )

        GraficoAtletas <- hPlot(x = "Year", y = "Athletes",
                                group = "Season",
                                data = DatosAtletas,
                                type = 'line')
        GraficoAtletas$title(text = "Athletes Over Time")
        chart_width <- 0.95*as.numeric(JS('window.innerWidth'))
        GraficoAtletas$set(width = chart_width)
        GraficoAtletas$exporting(enabled = T)
        GraficoAtletas$yAxis(title=list(text="Athletes ",align='high', offset= 0,rotation=0,y= -20))
        return(GraficoAtletas)
      }

    })

    #==================================NACIONES==================================
    output$VisualizacionNaciones <- renderChart2({
      if (input$Filtro_AG == 'country')
      {

        DatosAnalisisGeneral <- DatosOlimpicos  %>%
          filter(Year >= input$Years_AG[1] & Year <= input$Years_AG[2]
                 & Season %in% input$Season_AG & Region %in% input$Country_AG) %>%
          group_by(Year,Season,Region) %>%
          summarize(
            Events = length(unique(Event))
          )

        GraficoEventos <- hPlot(x = "Year", y = "Events",
                                group = "Region",
                                data = DatosAnalisisGeneral,
                                type = 'line')

        GraficoEventos$title(text = "Events Over Time")
        chart_width <- 0.95*as.numeric(JS('window.innerWidth'))
        GraficoEventos$set(width = chart_width)
        GraficoEventos$exporting(enabled = T)
        GraficoEventos$yAxis(title=list(text="Events",align='high', offset= 0,rotation=0,y= -20))
        return(GraficoEventos)

      }else if (input$Filtro_AG == 'sport')
      {
        DatosNaciones <- DatosOlimpicos  %>%
          filter(Year >= input$Years_AG[1] & Year <= input$Years_AG[2]
                 & SportsCompetition %in% input$Sport_AG) %>%
          group_by(Year,SportsCompetition) %>%
          summarize(
            Nations = length(unique(Region))
          )

        GraficoNaciones <- hPlot(x = "Year", y = "Nations",
                                 group = "SportsCompetition",
                                 data = DatosNaciones,
                                 type = 'line')
        GraficoNaciones$title(text = "Nations Over Time")
        chart_width <- 0.95*as.numeric(JS('window.innerWidth'))
        GraficoNaciones$set(width = chart_width)
        GraficoNaciones$exporting(enabled = T)
        GraficoNaciones$yAxis(title=list(text="Nations",align='high', offset= 0,rotation=0,y= -20))
        return(GraficoNaciones)

      }else
      {
        DatosNaciones <- DatosOlimpicos  %>%
          filter(Year >= input$Years_AG[1] & Year <= input$Years_AG[2]) %>%
          group_by(Year, Season) %>%
          summarize(
            Nations = length(unique(Region))
          )

        GraficoNaciones <- hPlot(x = "Year", y = "Nations",
                                 group = "Season",
                                 data = DatosNaciones,
                                 type = 'line')
        GraficoNaciones$title(text = "Nations Over Time")
        chart_width <- 0.95*as.numeric(JS('window.innerWidth'))
        GraficoNaciones$set(width = chart_width)
        GraficoNaciones$exporting(enabled = T)
        GraficoNaciones$yAxis(title=list(text="Nations",align='high', offset= 0,rotation=0,y= -20))
        return(GraficoNaciones)
      }
    })

    #==================================EVENTOs==================================
    output$VisualizacionEventos <- renderChart2({
      if (input$Filtro_AG == 'all')
      {
        DatosEventos <- DatosOlimpicos  %>%
          filter(Year >= input$Years_AG[1] & Year <= input$Years_AG[2]) %>%
          group_by(Year, Season) %>%
          summarize(
            Events = length(unique(Event))
          )

        GraficoEventos <- hPlot(x = "Year", y = "Events",
                                group = "Season",
                                data = DatosEventos,
                                type = 'line')
        GraficoEventos$title(text = "Events Over Time")
        chart_width <- 0.95*as.numeric(JS('window.innerWidth'))
        GraficoEventos$set(width = chart_width)
        GraficoEventos$exporting(enabled = T)
        GraficoEventos$yAxis(title=list(text="Events",align='high', offset= 0,rotation=0,y= -20))
        return(GraficoEventos)
      }else
      {
        DatosEventos <- DatosOlimpicos  %>% filter(Year =='1') %>%
          group_by(Year, Season) %>%
          summarize(
            Events = length(unique(Event))
          )
        GraficoEventos <- hPlot(x = "Year", y = "Events",
                                group = "Season",
                                data = DatosEventos,
                                type = 'line')
        chart_width <- 0.95*as.numeric(JS('window.innerWidth'))
        GraficoEventos$set(width = chart_width)
        GraficoEventos$exporting(enabled = T)
        GraficoEventos$yAxis(title=list(text="Events",align='high', offset= 0,rotation=0,y= -20))
        return(GraficoEventos)
      }
    })


    ####################################################################
    # ANALISIS POR GENERO
    ####################################################################

    observeEvent(input$Filtro_Gen, {
      if (input$Filtro_Gen == 'country_gen')
      {
        output$Sport_Gen <- renderUI(NULL)

        output$Country_Gen <-
          renderUI(
            selectInput(inputId='Country_Gen',label='Country',
                        selected = levels(DatosOlimpicos$Region)[1],
                        choices = levels(DatosOlimpicos$Region)
            )
          )
      }
      else  if (input$Filtro_Gen == 'sport_gen')
      {
        output$Country_Gen <- renderUI(NULL)

        output$Sport_Gen <-
          renderUI(
            selectInput(inputId='Sport_Gen',label='Sport',
                        selected = levels(DatosOlimpicos$SportsCompetition)[6],
                        choices = levels(DatosOlimpicos$SportsCompetition)
            )
          )
      }
      else
      {
        output$Country_Gen <- renderUI(NULL)
        output$Sport_Gen <- renderUI(NULL)
      }
    })


    output$VisualizacionGenero <- renderChart2({
      if (input$Filtro_Gen == 'country_gen')
      {
        DatosGenero <- DatosOlimpicos %>%
          filter(Year >= input$Years_Gen[1] & Year <= input$Years_Gen[2]
                 & Season=="Summer" & Region %in% input$Country_Gen) %>%
          group_by(Year,Gender,Region) %>%
          summarize(
            Genero = length(unique(ID))
          )

        DatosAtletas <- DatosOlimpicos  %>%
          filter(Year >= input$Years_Gen[1] & Year <= input$Years_Gen[2]
                 & Season=="Summer" & Region  %in% input$Country_Gen)

      }else if (input$Filtro_Gen == 'sport_gen')
      {
        DatosGenero <- DatosOlimpicos  %>%
          filter(Year >= input$Years_Gen[1] & Year <= input$Years_Gen[2]
                 & SportsCompetition %in% input$Sport_Gen) %>%
          group_by(Year, SportsCompetition,Gender) %>%
          summarize(
            Genero = length(unique(ID))
          )

        DatosAtletas <- DatosOlimpicos  %>%
          filter(Year >= input$Years_Gen[1] & Year <= input$Years_Gen[2]
                 & SportsCompetition %in% input$Sport_Gen)

      }
      else
      {
        DatosGenero <- DatosOlimpicos  %>%
          filter(Year >= input$Years_Gen[1] & Year <= input$Years_Gen[2]
                 & Season=="Summer") %>%
          group_by(Year, Gender) %>%
          summarize(
            Genero = length(unique(ID))
          )

        DatosAtletas <- DatosOlimpicos  %>%
          filter(Year >= input$Years_Gen[1] & Year <= input$Years_Gen[2]
                 & Season=="Summer")

      }

      grafico <- hPlot(x = "Year", y = "Genero",
                       group = "Gender",
                       data = DatosGenero,
                       type = 'line')

      grafico$title(text = ".", style=list(color = "white"))
      grafico$yAxis(gridLineColor='transparent')
      grafico$yAxis(gridLineColor='transparent', lineWidth= 1,tickWidth= 1,title=list(text="Athletes ",align='high', offset= 0,rotation=0,y= -20))
      grafico$colors('#FFA8FF','#25A0C5')
      chart_width <- 0.95*as.numeric(JS('window.innerWidth'))
      grafico$set(width = chart_width,height =450)
      grafico$exporting(enabled = T)


      DatosHombres <- DatosAtletas %>% filter(Gender=='Male')
      DatosMujeres <- DatosAtletas %>% filter(Gender=='Female')

      #Número de Deportistas
      output$numDeportistas <- renderValueBox({
        valueBox(
          paste0(nrow(distinct(DatosAtletas,ID))), paste("Atletas"),
          icon = icon("venus-mars"), color = "olive"
        )
      })

      #Número de Hombres
      output$numHombres <- renderValueBox({
        valueBox(
          paste0(nrow(distinct(DatosHombres,ID))), paste("Hombres"),
          icon = icon("male"), color = "aqua"
        )
      })

      #Número de Mujeres
      output$numMujeres <- renderValueBox({
        valueBox(
          paste0(nrow(distinct(DatosMujeres,ID))), paste("Mujeres"),
          icon = icon("female"), color = "orange"
        )
      })
      return(grafico)
    })

    ####################################################################
    # PESO VS ALTURA
    ####################################################################
    output$Correlacion <- renderChart2({

      datosCorrelacion <- DatosOlimpicos %>%
        filter (SportsCompetition==input$Sport_PA &
                  Year >= input$Years_PA[1] & Year <= input$Years_PA[2] &
                  Gender %in% input$Gender_PA)

      gr<-hPlot(Height ~ Weight, data = datosCorrelacion, type = "scatter", group = "Medal", radius =4,marker=list(symbol='square'))

      gr$title(text = ".", style=list(color = "white")) #Changes title in function of the indicator
      gr$colors('#D19C67','	#d6ae01', '#C0C0C0 ')
      gr$tooltip(formatter = "#! function() { return '<b>Weight: </b>'+ this.x + '<br> '+ '<b>Height:</b> ' + this.y; } !#")
      gr$plotOptions(scatter = list(marker = list(symbol = 'circle')))
      gr$chart(zoomType = "xy")
      gr$yAxis(gridLineColor='transparent')
      gr$yAxis(gridLineColor='transparent', lineWidth= 1,tickWidth= 1,title=list(text="Height",align='high', offset= 0,rotation=0,y= -20))
      ord <- c(Gold = 0, Silver = 1, Bronze = 2)
      gr$params$series <- lapply(gr$params$series, function(d){
        temp = ord[d$name]
        names(temp) = NULL
        d$legendIndex = temp
        return(d)
      })
      gr$exporting(enabled = T)
      chart_width <- 0.95*as.numeric(JS('window.innerWidth'))
      gr$set(width = chart_width,height =450)

      MedallasOro <- datosCorrelacion %>% filter(Medal=='Gold')
      MedallasPlata <- datosCorrelacion %>% filter(Medal=='Silver')
      MedallasBronce <- datosCorrelacion %>% filter(Medal=='Bronze')

      output$medalGold <- renderInfoBox ({
        infoBox("Gold",
                paste0(nrow(distinct(MedallasOro,ID))),
                icon = icon("medal"), color = "blue"
        )
      })

      output$medalSilver <- renderInfoBox ({
        infoBox("Silver",
                paste0(nrow(distinct(MedallasPlata,ID))),
                icon = icon("medal"), color = "purple"
        )
      })

      output$medalBronze <- renderInfoBox ({
        infoBox("Bronze",
                paste0(nrow(distinct(MedallasBronce,ID))),
                icon = icon("medal"), color = "green"
        )
      })

      return(gr)
    })

    ####################################################################
    # TOP Country
    ####################################################################
    output$BarrasAgrupadas <-  renderPlotly({

      datosreducidos <- DatosOlimpicos %>%
        filter (SportsCompetition==input$Sport_TC
                & Region %in% input$Region_TC & Gender %in% input$Gender_Tc)

      medal_counts <- datosreducidos %>% filter(!is.na(Medal))%>%
        group_by(Region, Medal) %>%
        summarize(Count=length(Medal)) %>% ungroup()

      levs <- medal_counts %>%
        group_by(Region) %>%
        summarize(Total=sum(Count)) %>%
        arrange(Total) %>%
        select(Region)

      medal_counts$Region <- factor(medal_counts$Region, levels=levs$Region)

      ColoresMedallas <- c("Gold" = "#FFE920","Silver" = "#C0C0C0","Bronze" = "#D19C67")


      ggplot(medal_counts, aes(x=Region, y=Count, fill=Medal)) +
        geom_col() +
        coord_flip() +
        scale_fill_manual(values = ColoresMedallas,limits = names(ColoresMedallas)) +

        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black"),
              axis.title.y=element_blank(),axis.title.x=element_blank(),legend.title = element_blank()) +
        scale_y_continuous(expand = c(0, 0))
      #geom_text(aes(label = stat(y), group = Region), stat = 'summary', fun.y = sum, vjust = -9)


    })

    observeEvent(input$Sport_TC, {
      DatosSport <- DatosOlimpicos %>%
        filter (SportsCompetition==input$Sport_TC)

      DatosAgrupadosRegion <- DatosSport %>% filter(!is.na(Medal))%>%
        group_by(Region) %>%
        summarize(Count=length(Medal))

      DatosAgrupadosRegion <-
        DatosAgrupadosRegion[order(DatosAgrupadosRegion$Count,decreasing = TRUE),]

      output$Region_TC <-
        renderUI(
          selectInput(
            inputId = 'Region_TC',
            label = 'Select Country:',
            selected = DatosAgrupadosRegion$Region[0:15],
            multiple = TRUE,
            choices = DatosAgrupadosRegion$Region
          )
        )

    })

    ####################################################################
    # WORLD MAP
    ####################################################################
    output$Worldmap <-  renderPlotly({


      paises <- DatosOlimpicos %>% filter(!is.na(Medal)  &
                                            Year >= input$Years_WM[1] & Year <= input$Years_WM[2]) %>%
        group_by(Region,Medal) %>%
        summarize(Total = length(Medal))

      paises<- spread(paises, Medal, Total)
      paises[is.na(paises)] <- 0
      paises$Total = paises$Gold + paises$Silver + paises$Bronze


      world <- map_data("world")


      mapdat <- tibble(region=unique(world$region))
      mapdat <- left_join(mapdat, paises, by = c("region" = "Region"))

      #mapdat$Total[is.na(mapdat$Total)] <- 0
      mapdat[is.na(mapdat)] <- 0
      world <- left_join(world, mapdat, by="region")


      p<-   ggplot(world, aes(x = long, y = lat)) +
        geom_polygon(aes( group = group, fill = Total,
                          text = paste("<b>Region:</b>", region, "<br> <b>Total Medallas:</b>", Total,
                                       "<br><b>Gold:</b>", Gold,"<b>Silver</b>:", Silver,"<b>Bronze:</b>", Bronze)),
                     color='gray70', size=0.1) +
        labs(title = NULL, x = NULL, y=NULL) +

        theme_minimal() +

        theme(axis.ticks = element_blank(),
              axis.text = element_blank(),
              panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "white"),
              plot.title = element_text(hjust = 0.5)) +
        guides(fill=guide_colourbar(title="Athletes")) +
        scale_fill_gradient(low="#BDF4CB",high="#23819C")

      p <- ggplotly(p, tooltip = c("text"))

    })

    ####################################################################
    # CORRELACION PIB
    ####################################################################

    output$CorrelacionPIB <-  renderChart2({

      datosCorrelacionPIB <- DatosUnificados %>%
        filter (Year >= input$Years_PIB[1] & Year <= input$Years_PIB[2])

      valorCorrelacion <- cor(datosCorrelacionPIB$PIB_LOG, datosCorrelacionPIB$TotalMedallas)

      graficoPIB <- hPlot( TotalMedallas ~ PIB_LOG, data = datosCorrelacionPIB, type = "scatter", radius =4,marker=list(symbol='square'))

      graficoPIB$title(text = ".", style=list(color = "white"))
      graficoPIB$tooltip(formatter = "#! function() { return '<b>Total Medallas: </b>'+ this.x + '<br> '+ '<b>PIB (log):</b> ' + this.y; } !#")
      graficoPIB$chart(zoomType = "xy")

      graficoPIB$xAxis(title=list(text="PIB (log)"))
      graficoPIB$yAxis(gridLineColor='transparent')
      graficoPIB$yAxis(gridLineColor='transparent', lineWidth= 1,tickWidth= 1,title=list(text="Medallas",align='high', offset= 0,rotation=0,y= -20))

      graficoPIB$exporting(enabled = T)
      chart_width <- 0.95*as.numeric(JS('window.innerWidth'))
      graficoPIB$set(width = chart_width,height =450)


      output$ValCorrelacion <- renderValueBox({
        valueBox(
          round(valorCorrelacion, digits = 2) , paste("Correlacion (R)"),
          icon = icon("chart-line"), color = "light-blue"
        )
      })

      return(graficoPIB)

    })

    ####################################################################
    # DISTRIBUCION POR EDAD
    ####################################################################

    observeEvent(input$Filtro_Edad, {
      if (input$Filtro_Edad == 'country_Edad')
      {
        output$Sport_Edad<- renderUI(NULL)

        output$Country_Edad <-
          renderUI(
            selectInput(inputId='Country_Edad',label='Country',
                        selected = levels(DatosEdad$Region)[1],
                        choices = levels(DatosEdad$Region),selectize = FALSE
            )
          )
      }
      else  if (input$Filtro_Edad == 'sport_Edad')
      {
        output$Country_Edad <- renderUI(NULL)

        output$Sport_Edad <-
          renderUI(
            selectInput(inputId='Sport_Edad',label='Sport',
                        selected = levels(DatosEdad$SportsCompetition)[6],
                        choices = levels(DatosEdad$SportsCompetition),
                        selectize = FALSE
            )
          )
      }
      else
      {
        output$Country_Edad<- renderUI(NULL)
        output$Sport_Edad <- renderUI(NULL)
      }
    })


    output$VisualizacionEdad <- renderChart2({
      if (input$Filtro_Edad == 'country_Edad')
      {
        DatosEdadAtletas <- DatosEdad %>%
          filter(Year >= input$Years_Edad[1] & Year <= input$Years_Edad[2] &
                   Age >= input$Rango_Edad[1] & Age <= input$Rango_Edad[2] &
                   Gender %in% input$Gender_Edad & Region %in% input$Country_Edad) %>%
          group_by(Age) %>%
          summarize(Total = length(unique(ID)))
      }else if (input$Filtro_Edad == 'sport_Edad')
      {
        DatosEdadAtletas <- DatosEdad %>%
          filter(Year >= input$Years_Edad[1] & Year <= input$Years_Edad[2] &
                   Age >= input$Rango_Edad[1] & Age <= input$Rango_Edad[2] &
                   Gender %in% input$Gender_Edad & SportsCompetition %in% input$Sport_Edad) %>%
          group_by(Age) %>%
          summarize(Total = length(unique(ID)))
      }else
      {
        DatosEdadAtletas <- DatosEdad %>%
          filter(Year >= input$Years_Edad[1] & Year <= input$Years_Edad[2] &
                   Age >= input$Rango_Edad[1] & Age <= input$Rango_Edad[2] &
                   Gender %in% input$Gender_Edad) %>%
          group_by(Age) %>%
          summarize(Total = length(unique(ID)))
      }


      graficoBarras <- hPlot(y = "Total", x = "Age",data = DatosEdadAtletas,
                             type = 'column')

      graficoBarras$title(text = ".", style=list(color = "white"))
      graficoBarras$yAxis(gridLineColor='transparent')
      graficoBarras$yAxis(gridLineColor='transparent', lineWidth= 1,tickWidth= 1,title=list(text="Athletes ",align='high', offset= 0,rotation=0,y= -20))

      graficoBarras$chart(zoomType = "xy")
      graficoBarras$exporting(enabled = T)
      chart_width <- 0.95*as.numeric(JS('window.innerWidth'))
      graficoBarras$set(width = chart_width,height =450)

      return(graficoBarras)
    })

  }


  shinyApp(ui,server)

}

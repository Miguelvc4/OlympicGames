
#' @title Analisis General
#' @description Esta funcion permite generar una serie de visualizaciones con los datos de los juegos olimpicos.
#' @details Esta funcion permite generar 3 visualizaciones interactivas para representar la evolucion de los juegos olimpicos, considerando la cantidad de Atletas participantes, Naciones y Eventos.
#' @export
#' @import shiny
#' @import tidyverse
#' @import shinydashboard
#' @import downloader
#' @import dplyr

AnalisisGeneral <- function()
{

  #data("DatosOlimpicos")

  #download("https://github.com/ramnathv/rCharts/archive/master.tar.gz", "rCharts.tar.gz")
  #install.packages("rCharts.tar.gz", repos = NULL, type = "source")
  #library(rCharts)

  ui <- dashboardPage(skin = "blue",
          dashboardHeader(title = "120 Years of Olympic Games Data",titleWidth = 350),

          dashboardSidebar(
            sidebarUserPanel("Miguel Velasquez",
                             image = "http://estrategia.info/cath/wp-content/uploads/2013/10/anillos-olimpicos1.png"),
                        sidebarMenu(
                          menuItem("Analisis General", tabName = "AnalisisGeneral", icon = icon("history")))
                        ),

         dashboardBody(

           tabItems(

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
             )

             )



        )
  )

  server <- function(input, output) {

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



  }

  shinyApp(ui,server)


}

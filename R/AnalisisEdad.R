
#' @title Distribucion por Edad
#' @description Esta funcion permite generar una visualizacion interactia con la distribucion por edad de los atletas olimpicos a lo largo de los años.
#' @usage AnalisisEdad
#' @details Esta funcion permite generar una visualizacion interactiva con los datos historicos de los juegos olimpicos desde la edicion de Atenas 1896.
#' La visualizacion consiste en la distribucion por Edad de los diferentes competidores, ademas se agregaron una serie de filtros
#' para facilitar la interaccion con la informacion y ademas permitir parametrizar los datos que se desean representar.
#' @export
#' @import shiny
#' @import shinydashboard
#' @import downloader


AnalisisEdad <- function()
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
                          menuItem("Distribucion por Edad", tabName = "DistribucionEdad", icon = icon("chart-bar")))
                        ),

         dashboardBody(

           tabItems(
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

  server <- function(input, output) {
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


return(shinyApp(ui,server))

}


#' @title Analisis por Genero
#' @description Esta funcion permite representar la participacion de Hombres y Mujeres en los Juegos Olimpicos a lo largo de los años.
#' @details Esta funcion permite representar la participacion de Hombres y Mujeres en los Juegos Olimpicos a lo largo de los años.
#' @export
#' @import shiny
#' @import tidyverse
#' @import shinydashboard
#' @import downloader


AnalisisGenero <- function()
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
                          menuItem("Analisis Por Genero", tabName = "AnalisisGenero", icon = icon("venus-mars")))
                        ),

         dashboardBody(

           tabItems(

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

             )

             )



        )
  )

  server <- function(input, output) {

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


  }

  shinyApp(ui,server)


}

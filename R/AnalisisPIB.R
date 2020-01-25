
#' @title Correlacion entre el Producto Interno Bruto (PIB) y la cantidad de medallas ganadas
#' @description Esta funcion permite representar el PIB de cada pais en relacion a la cantidad de medallas ganadas.
#' @details Esta funcion permite generar una visualizacion interactiva con los datos historicos de los juegos olimpicos.
#' La visualizacion consiste en la representacion de la relacion entre el PIB de cada pais y la cantidad de medallas ganadas.
#' Para comprobar la fuerza de la relacion entre las dos variable se realiza el calculo de la correlacion.
#' Ademas se agregaron una serie de filtros que permiten interactuar con la informacion graficada y ademas poder parametrizar lo que se desea representar.
#' @export
#' @import shiny
#' @import shinydashboard
#' @import downloader



AnalisisPIB <- function()
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
                          menuItem("Correlacion PIB y Medallas", tabName = "CorrelacionPIB", icon = icon("dollar-sign")))
                        ),

         dashboardBody(

           tabItems(
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
             )

             )



        )
  )

  server <- function(input, output) {
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

  }

  shinyApp(ui,server)


}

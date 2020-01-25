
#' @title Grafica Tabular.
#' @description Esta es una funcion para mostrar todos los datos historicos de los juegos olimpicos.
#' @details Esta funcion permite mostrar de forma tabular los datos historicos de los juegos olimpicos desde la edicion de Atenas 1896.
#' Se agregaron filtros en cada una de las columnas de la visualizacion, para que se pueda filtrar la informacion que se necesite.
#' Ademas se agregaron secciones para descargar todos estos datos historicos, asi como para  imprimirlos y exportarlos a diferentes formatos.
#' @export
#' @import shiny
#' @import tidyverse
#' @import shinydashboard
#' @import htmlwidgets


GraficaTabular <- function()
{

  #data("DatosResumen")

  ui <- dashboardPage(skin = "blue",
          dashboardHeader(title = "120 Years of Olympic Games Data",titleWidth = 350),

          dashboardSidebar(
            sidebarUserPanel("Miguel Velasquez",
                             image = "http://estrategia.info/cath/wp-content/uploads/2013/10/anillos-olimpicos1.png"),
                        sidebarMenu(
                          menuItem("Datos Completos", tabName = "Datos", icon = icon("th")))
                        ),

         dashboardBody(

           tabItems(

             ##############################################
             #DATOS
             ##############################################
             tabItem(tabName = "Datos",
                     titlePanel("Datos Historicos"),
                     fluidRow(column(DT::dataTableOutput("DatosOlimpicos"),
                                     #%>%
                                    #   withSpinner(),
                                     width = 12)),
                     br(),

                     fluidRow(downloadButton('downloadData', 'Download Full Data',
                                             style="display: block; margin: 0 auto; width: 230px;color: black;"),
                              width = 12)
             ))



        )
  )

  server <- function(input, output) {

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

  }

  shinyApp(ui,server)


}

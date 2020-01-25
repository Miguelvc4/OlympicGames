
#' @title Peso VS Altura
#' @description Esta funcion permite generar un diagrama de dispersion con la informacion de Peso y Altura de cada Atleta.
#' @details Esta funcion permite generar un diagrama de dispersion con la informacion de Peso y Altura de cada Atleta.
#' @export
#' @import shiny
#' @import tidyverse
#' @import shinydashboard
#' @import downloader


AnalisisPesoAltura <- function()
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
                          menuItem("Peso VS Altura", tabName = "CorrelacionPesoAltura", icon = icon("project-diagram")))
                        ),

         dashboardBody(

           tabItems(

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
             )

             )



        )
  )

  server <- function(input, output) {

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

  }

  shinyApp(ui,server)


}

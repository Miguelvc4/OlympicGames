
#' @title Mapa Coropletico
#' @description Esta Funcion permite crear un mapa coropletico con la informacion de las medallas ganadas por cada pais
#' @details Esta funcion permite generar una visualizacion interactiva con los datos historicos de los juegos olimpicos desde la edicion de Atenas 1896.
#' La visualizacion consiste en la representacion grafica de la cantidad de medallas ganadas por cada pais.
#' Se utiliza un mapa coropletico para representar dicha informacion. La visualizacion permtie recorrer un mapa en 2D con la informacion de cada pais.
#' Ademas se agregaron una serie de filtros que permiten interactuar con la informacion graficada y parametrizar lo que se desea representar.
#' @export
#' @import shiny
#' @import shinydashboard
#' @import maps
#' @importFrom plotly ggplotly


AnalisisMapa <- function()
{

  #data("DatosOlimpicos")

  ui <- dashboardPage(skin = "blue",
          dashboardHeader(title = "120 Years of Olympic Games Data",titleWidth = 350),

          dashboardSidebar(
            sidebarUserPanel("Miguel Velasquez",
                             image = "http://estrategia.info/cath/wp-content/uploads/2013/10/anillos-olimpicos1.png"),
                        sidebarMenu(
                          menuItem("World Map", tabName = "WorldMap", icon = icon("globe-americas")))
                        ),

         dashboardBody(

           tabItems(

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
             )
             )



        )
  )

  server <- function(input, output) {
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

  }

  shinyApp(ui,server)


}

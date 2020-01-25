
#' @title TOP Paises
#' @description Esta Funcion permite crear una visualizacion interactiva con la informacion de los paises que mas medallas ganan en cada uno de lso deportes.
#' @details Esta funcion permite generar una visualizacion interactiva con los datos historicos de los juegos olimpicos desde la edicion de Atenas 1896.
#' La visualizacion consiste en la representacion del TOP 15 de paises que mas medallan han ganado en cada uno de los deportes.
#' Sin embargo tambien permite buscar un pais determinado que no este dentro de este top15 para poder visualizar las medallas que han ganado.
#' Se agregaron una serie de filtros para facilitar la interaccion con la informacion.
#' @export
#' @import shiny
#' @import tidyverse
#' @import shinydashboard
#' @import ggplot2


AnalisisTopPaises <- function()
{

  #data("DatosOlimpicos")

  ui <- dashboardPage(skin = "blue",
          dashboardHeader(title = "120 Years of Olympic Games Data",titleWidth = 350),

          dashboardSidebar(
            sidebarUserPanel("Miguel Velasquez",
                             image = "http://estrategia.info/cath/wp-content/uploads/2013/10/anillos-olimpicos1.png"),
                        sidebarMenu(
                          menuItem("Top Paises", tabName = "TopPaises", icon = icon("medal")))
                        ),

         dashboardBody(

           tabItems(

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
             )
             )



        )
  )

  server <- function(input, output) {
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

  }

  shinyApp(ui,server)


}

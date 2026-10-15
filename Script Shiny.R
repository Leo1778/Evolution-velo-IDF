
library(shiny)
library(leaflet)
library(dplyr)
library(viridisLite)

#charger les données
carte_velo_app <- readRDS("carte_velo_app.rds")
# INTERFACE : ce que voit l'utilisateur
ui <- fluidPage(
  titlePanel("Le vélo en Île-de-France"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "indicateur",
        label = "Indicateur à cartographier :",
        choices = c(
          "Part du vélo en 2017 (%)" = "part_velo_2017",
          "Part du vélo en 2023 (%)" = "part_velo_2023",
          "Évolution 2017–2023 (points)" = "evol_velo"
        ),
        selected = "part_velo_2023"
      )
    ),
    
    mainPanel(
      leafletOutput("carte", height = "650px")
    )
  )
)

# SERVEUR : les calculs et la carte
server <- function(input, output, session) {
  
  output$carte <- renderLeaflet({
    
    variable <- input$indicateur
    
    # Palette adaptée à l'indicateur
    if (variable == "evol_velo") {
      lim <- max(abs(carte_velo_app$evol_velo), na.rm = TRUE)
      
      pal <- colorNumeric(
        palette = colorRampPalette(
          c("#2166AC", "white", "#B2182B")
        )(256),
        domain = c(-lim, lim),
        na.color = "transparent"
      )
      
      titre_legende <- "Évolution (points)"
      
    } else {
      pal <- colorNumeric(
        palette = viridis(256),
        domain = carte_velo_app[[variable]],
        na.color = "transparent"
      )
      
      titre_legende <- "Part du vélo (%)"
    }
    
    # Carte interactive
    leaflet(carte_velo_app) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~pal(carte_velo_app[[variable]]),
        fillOpacity = 0.8,
        color = "#666666",
        weight = 0.5,
        label = ~paste0(
          CODGEO, " : ",
          round(carte_velo_app[[variable]], 2),
          if (variable == "evol_velo") " points" else " %"
        ),
        highlightOptions = highlightOptions(
          weight = 2,
          color = "#333333",
          bringToFront = TRUE
        )
      ) %>%
      addLegend(
        pal = pal,
        values = carte_velo_app[[variable]],
        title = titre_legende,
        position = "bottomright"
      )
  })
}

# Lancement
shinyApp(ui = ui, server = server)
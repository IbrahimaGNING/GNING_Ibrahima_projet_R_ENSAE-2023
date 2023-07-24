# Charger les bibliothèques nécessaires
library(sp)
library(ggplot2)
library(dplyr)
library(shiny)
library(leaflet)
library(rnaturalearth)
library(rnaturalearthdata)
library(shinythemes)

# Charger les données géographiques pour tous les pays d'Afrique
africa <- subset(ne_countries(scale = "medium", continent = "Africa"))

# Charger les données à partir du fichier CSV
data <- read.csv("ACLED-Western_Africa.csv")

# Interface utilisateur Shiny
ui <- fluidPage(
  # theme
  theme = shinytheme('superhero'),
  
  # Titre de l'application
  titlePanel("Carte interactive des événements en Afrique de l'Ouest"),
  
  # Barre latérale avec des sélecteurs pour filtrer les données
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "pays",
        label = "Sélectionnez un pays",
        choices = c("Tous", unique(data$pays)),
        selected = "Tous",
        multiple = TRUE
      ),
      selectInput(
        inputId = "evenement",
        label = "Sélectionnez un événement",
        choices = c("Tous", unique(data$type)),
        selected = "Tous",
        multiple = TRUE
      ),
      selectInput(
        inputId = "annee",
        label = "Sélectionnez une année",
        choices = c("Tous", unique(data$annee)),
        selected = "Tous",
        multiple = TRUE
      )
    ),
    
    # Afficher la carte interactive
    mainPanel(
      leafletOutput(outputId = "map",
                    width = "100%",
                    height = "500px")
    )
  )
)

# Fonction serveur pour Shiny
server <- function(input, output, session) {
  filtered_data <- reactive({
    # Filtrer les données en fonction des sélections de l'utilisateur
    data_filtered <- data
    if (input$pays != "Tous") {
      data_filtered <- data_filtered[data_filtered$pays %in% input$pays, ]
    }
    if (input$evenement != "Tous") {
      data_filtered <- data_filtered[data_filtered$type %in% input$evenement, ]
    }
    if (input$annee != "Tous") {
      data_filtered <- data_filtered[data_filtered$annee %in% input$annee, ]
    }
    return(data_filtered)
  })
  
  output$map <- renderLeaflet({
    # Créer la carte avec leaflet
    leaflet() %>%
      addProviderTiles(providers$Stamen.Toner) %>%
      fitBounds(lng1 = -18, lat1 = 4, lng2 = 14, lat2 = 26) %>%
      addPolygons(data = africa, fill = "lightblue", stroke = TRUE, color = "gray", weight = 1) %>%
      addCircleMarkers(data = filtered_data(), lng = ~longitude, lat = ~latitude, 
                       radius = 5, fillColor = "red", fillOpacity = 0.7,
                       popup = ~paste("Pays: ", pays, "<br/>", "Type: ", type, "<br/>", "Année: ", annee))
  })
}

# Lancer l'application Shiny
shinyApp(ui = ui, server = server)





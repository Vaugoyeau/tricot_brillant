library(shiny)
library(shiny.semantic)

ui <- shinyUI(
  semanticPage(
    fileInput("image_choisi", "Image à importer", width = "400px", type = "small"),
    numericInput("nbr_rang_10cm", "Nombre de rangs pour l'échantillon de 10 cm", value = 25, width = "400px", type = "small"),
    numericInput("nbr_maille_10cm", "Nombre de mailles pour l'échantillon de 10 cm", value = 20, width = "400px", type = "small"),
    numericInput("hauteur_tricot", "Hauteur du tricot à réaliser en cm", value = 20, width = "400px", type = "small"),
    numericInput("largeur_tricot", "Largeur du tricot à réaliser en cm", value = 20, width = "400px", type = "small"),
    # numericInput("poids_echantillon", "Poids de l'échantillon de 10 * 10 cm", value = 10, width = "400px", type = "small"),
    h3("Image à tricoter"),
    imageOutput("image_a_tricoter")
  )
)

server <- shinyServer(function(input, output, session) {
  output$image_a_tricoter <- renderImage({
    if (is.null(input$image_choisi)) return("Pas d'image sélectionnée")
    tricot::knitting_image(
      tricot::image_load(
        input$image_choisi
      ), 
      tricot::grid_size(
        input$hauteur_tricot, 
        input$largeur_tricot, 
        tricot::square_size(
          input$nbr_rang_10cm,
          input$nbr_maille_10cm
        )
      )
    )
  })
})

shiny::shinyApp(ui, server)


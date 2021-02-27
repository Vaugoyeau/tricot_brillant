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
    action_button("tricotage_image", "Créer l'image à tricoter"),
    h3("Image à tricoter"),
    plotOutput("image_a_tricoter")#,
    # h3("Instruction à télécharger"),
    # action_button("image_telechargee", "Télécharger la grille")
  )
)

server <- shinyServer(function(input, output, session) {
  
  observeEvent(
    input$tricotage_image, 
    {
      output$image_a_tricoter <- renderPlot({
        plotly::ggplotly(
          tricot::knitting_image(
            tricot::image_load(
              input$image_choisi$datapath
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
        )
      })
    }
    
  )
  
  
  # observeEvent(input$image_telechargee, {
  #   
  # })
  
})

shiny::shinyApp(ui, server)


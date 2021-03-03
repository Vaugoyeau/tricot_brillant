library(shiny)
library(shiny.semantic)
library(plotly)

## Utilisation des fonctions du package {tricot}

#' Load image
#'
#' @param path Path to load image
#' @inheritParams magick::image_read 
#' @inheritParams stringr::str_detect
#'
#' @return
#' Magick image in R
#' @export
#'
#' @examples
image_load <- function(path) {
  if (!file.exists(path)) {stop("path is incorrect")}
  
  if (sum(stringr::str_detect(path, c(".jpeg", ".jpg", ".png", ".tif"))) > 0) {magick::image_read(path)}
  else if (stringr::str_detect(path, ".pdf")) {magick::image_read_pdf(path, pages = 1)}
  else {stop("format is not supported")}
}


#' Square of size
#'
#' @param rg Row number to have 10 cm
#' @param m Stitch number to have 10 cm
#'
#' @return
#' Square size to do grid on image
#' @export
#'
#' @examples
square_size <- function(rg, m) {
  if (!is.numeric(rg)) {stop("rg should be numeric")}
  if (length(rg) != 1) {stop("rg should have only one value")}
  if (!is.numeric(m)) {stop("m should be numeric")}
  if (length(m) != 1) {stop("m should have only one value")}
  
  c(
    height = round(10/rg, 3),
    width = round(10/m, 3)
  )
}


#' Grid size
#'
#' @param h Expected height (cm) of knitting
#' @param w Expected width (cm) of knitting
#' @param ss Size square calculated from square_size()
#'
#' @return
#' Grid size to do grid on image
#' @export
#'
#' @examples
grid_size <- function(h, w, ss) {
  if (!is.numeric(h)) {stop("h should be numeric")}
  if (length(h) != 1) {stop("h should have only one value")}
  if (!is.numeric(w)) {stop("w should be numeric")}
  if (length(w) != 1) {stop("w should have only one value")}
  if (!is.numeric(ss)) {stop("gs should be numeric")}
  if (length(ss) != 2) {stop("gs should have only two values")}
  
  c(
    grid = ceiling(h/ss["height"]),
    grid = ceiling(w/ss["width"])
  )
}


#' Creation of knitting image
#'
#' @param img Magick image
#' @param gs Grid size calculated from grid_size()
#' @inheritParams dplyr::arrange 
#' @inheritParams magick::image_raster
#' @inheritParams magick::image_scale
#' @inheritParams ggplot2::ggplot
#' @inheritParams ggplot2::aes
#' @inheritParams ggplot2::geom_point
#' @inheritParams ggplot2::scale_color_manual
#' @inheritParams dplyr::distinct
#' @inheritParams ggplot2::theme
#' @inheritParams ggplot2::xlab
#' @inheritParams ggplot2::ylab
#'
#' @return
#' Knitting image
#' @export
#'
#' @examples
knitting_image <- function(img, gs) {
  
  if(class(img) != "magick-image") {stop("img must be magick image")}
  if (!is.numeric(gs)) {stop("gs should be numeric")}
  if (length(gs) != 2) {stop("gs should have only two values")}
  
  raster_image <- 
    dplyr::arrange(
      magick::image_raster(
        magick::image_scale(
          img, 
          glue::glue('{gs["grid.width"]}x{gs["grid.height"]}!'))
      ), 
      col
    )
  
  
  kimage <- ggplot2::ggplot(
    dplyr::mutate(
      raster_image,
      "maille" = x,
      "rang" = max(y) + 1 - y
    ) 
  ) + 
    ggplot2::aes(x = maille, y = rang, color = col) + 
    ggplot2::geom_point(shape = 15) + 
    ggplot2::scale_color_manual(values = dplyr::distinct(raster_image, col)$col) + 
    ggplot2::theme(
      legend.position = "none", 
      panel.background = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(colour = "black"),
      panel.grid.minor = ggplot2::element_line(colour = "grey")
    )
  
  return(kimage)
  
}


myGridTemplate <- grid_template(
  default = list(
    areas = rbind(
      c("telechargement", "image"),
      c("info", "image")
    ),
    cols_width = c("auto", "auto"),
    rows_height = c("100px", "auto")
  ),
  mobile = list(
    areas = rbind(
      "telechargement",
      "info",
      "image"
    ),
    rows_height = c("100px", "300px", "auto"),
    cols_width = c("100%")
  )
)


ui <- shinyUI(
  semanticPage(
    grid(
      myGridTemplate,
      telechargement = fileInput("image_choisi", "Image à importer", width = "300px"),
      info = card(
      div(
       class = "content", 
       div(
         class = "header", 
         "Informations à remplir"
         ),
        numericInput("nbr_rang_10cm", "Nombre de rangs pour l'échantillon (10 cm)", value = 25, width = "400px", type = "small"),
        numericInput("nbr_maille_10cm", "Nombre de mailles pour l'échantillon (10 cm)", value = 20, width = "400px", type = "small"),
        numericInput("hauteur_tricot", "Hauteur du tricot à réaliser en cm", value = 20, width = "400px", type = "small"),
        numericInput("largeur_tricot", "Largeur du tricot à réaliser en cm", value = 20, width = "400px", type = "small"),
        # numericInput("poids_echantillon", "Poids de l'échantillon de 10 * 10 cm", value = 10, width = "400px", type = "small"),
      )
    ),
      image = plotlyOutput("image_a_tricoter")
    # ,
    # 
    # h3("Schema à télécharger"),
    # action_button("image_telechargee", "Télécharger la grille")
    )
  )
)

server <- shinyServer(function(input, output, session) {
  
  output$image_a_tricoter <- renderPlotly({
    
    if (is.null(input$image_choisi)) return(plotly::ggplotly(ggplot2::ggplot() + ggplot2::ggtitle("pas d'image chargée")))
    
    plotly::ggplotly(
      knitting_image(
        image_load(
          input$image_choisi$datapath
        ), 
        grid_size(
          input$hauteur_tricot, 
          input$largeur_tricot, 
          square_size(
            input$nbr_rang_10cm,
            input$nbr_maille_10cm
          )
        )
      )
    )
    
  }
  
  )
  
  
  # observeEvent(input$image_telechargee, {
  #   htmltools::save_html(
  #     plotly::ggplotly(output$image_a_tricoter), 
  #     "image_a_tricotee.html"
  #   )
  #     
  # })
  
})

shiny::shinyApp(ui, server)


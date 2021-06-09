header_UI <- function() {
  tags$head(
    tags$meta(charset = "UTF-8"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1.0"),
    tags$title('App de consumo MME por departamento'),
    tags$meta(name = "author", content = "Daniel Parra G."),
    tags$meta(name = "description",
              content = "Proyecto Aplicativo con datos de consumo de MME en Colombia a nivel nacional y departamento."),
    tags$meta(name = 'keywords',
              content = 'Consumo, Medicamentos Monopolio del Estado, Fondo Nacional de Estupefacientes, DDD, sDDD, DDDc'),
    tags$link(rel = "stylesheet", type = "text/css", href = "main.css"),
    tags$link(
      rel = "shortcut icon",
      type = "image/png",
      href = "icon_1.png",
      sizes = "16x16"
    )
  )
}

titulo_UI <- function() {
  # Titulo de Aplicación
  
  tags$div(class = 'container',
           fluidRow(
             class = 'tituloApp',
             column(
               9,
               tags$h2("Fondo Nacional de Estupefacientes", class = 'titulo1'),
               tags$h3("Gestión de la Regionalización/FRE", class = 'subtitulo1')
               # tags$h3("Programa de Farmacovigilancia", class = 'subtitulo1')
             ),
             column(
               3,
               tags$span(class = 'helper'),
               tags$image(src = 'SaludTodos.png',
                          alt = 'La salud es de todos', class = 'logoMinSalud')
             )
           ))
}

# headerServer <- function(id) {
#   moduleServer(
#     id = id,
#     module = function(input, output, server){
#
#     }
#   )
# }
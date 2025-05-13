LiquidMRCUI <- function(id, reagent, reagKey, explan) {
  ns <- NS(id)
  box(title = div(style = 'font-size:18px;valign="bottom"', tags$b('MRC de ', reagent)), 
      width = 12, status = 'primary', collapsible = TRUE, collapsed = TRUE,
      h5(explan),
      radioButtons(ns('SourceOption'), label = '¿Qué desea hacer?', 
                   choices = list('Crear disolución nueva utilizando una disolucion calibrante MRC' = 'daCapo',
                                  "Subir un archivo '.dis' generado anteriormente" = 'archivo'), 
                   selected = 'archivo'),
      tags$hr(),
      conditionalPanel(
        condition = 'input.SourceOption == "daCapo"',
        ns = ns,
        fluidRow(
          column(8, pickerInput(ns("MRCElected"), label = 'Seleccione el MRC:',
                                choices = MRCs.ArchiveNames[[reagKey]], width = '100%',# inline = FALSE,
                                multiple = FALSE)),
          column(4, tags$br(), uiOutput(ns("MRC_CertiFile"))))),
      conditionalPanel(
        condition = 'input.SourceOption == "archivo"',
        ns = ns,
        fileInput(ns('DisFile'), label = 'Escoja el archivo', multiple = FALSE, accept = '.dis'))
  )
}

LiquidMRCServer <- function(input, output, session, reagKey, IDUsuario) {
  
  fileDwnHTML <- reactive(a(href = paste0('CertMRC/', reagKey, '/', input$MRCElected, '.pdf'),
                            "Descargar certificado ", download = NA, target = "_blank"))
  output$MRC_CertiFile <- renderUI(fileDwnHTML())
  return()
}
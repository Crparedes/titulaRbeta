BalanceCalibCertUI <- function(id) {
  ns <- NS(id)
  box(title = div(style = 'font-size:20px;valign="bottom"', tags$b('Trazabilidad Metrológica de las Balanzas')), 
      width = 7, status = 'primary', collapsible = TRUE, collapsed = FALSE,# height = '300px', 
      #uiOutput(ns('CalibCertPicker')),
      pickerInput(ns("CalibCertElected"), 
                  label = 'Verifique la disponibilidad de los certificados de calibración de las balanzas que necesita:',
                  choices = CalibCertShow, width = '100%',# inline = FALSE,
                  multiple = FALSE, selected = 'MT XPE 205',
                  options = list(#`actions-box` = TRUE, size = 10, `deselect-all-text` = "Deseleccionar todos",
                    #`select-all-text` = "Seleccionar todos", 
                    `none-selected-text` = "Revise la vigencia del certificado (fecha)")),
      h6('Para solicitar la inclusión o la actualización de un certificado de calibración contacte a la persona
         encargada del mantenimiento de la aplicativo.'), tags$hr(),
      fluidRow(
        column(5, materialSwitch(ns('ShallprintTheCalibCert'), label = 'Información del certificado de calibración', 
                                 value = FALSE, status = "primary")),
        column(5, materialSwitch(ns('ShallplotTheCalibCert'), label = 'Gráfico de error de indicación', 
                                 value = FALSE, status = "primary"))),
      #checkboxGroupInput(ns('CalibCertPlotPrint'), label = NULL,
      #                   choices = list('Información completa' = 'Complete',
      #                                  'Gráfico de error de indicación' = 'Plot'), 
      #                   inline = TRUE),
      verbatimTextOutput(ns('printTheCalibCert')), 
      conditionalPanel(condition = "input.ShallplotTheCalibCert",
                       ns = ns,
                       plotOutput(ns('plotTheCalibCert'), width = '95%'))
  )
}

BalanceCalibCertServer <- function(input, output, session) {
  plotTheCalibCert <- reactive({
    if (input$ShallplotTheCalibCert) plot(CalibCertList[[input$CalibCertElected]])
    })
  
  output$printTheCalibCert <- renderPrint({
    if (input$ShallprintTheCalibCert) {
      cat('Certificado de calibración digital balanza', input$CalibCertElected, '\n\n')
      print(CalibCertList[[input$CalibCertElected]])#, complete = complete())
    }
  })
  output$plotTheCalibCert  <- renderPlot(plotTheCalibCert())
  
}
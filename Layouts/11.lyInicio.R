inicioLy <- fluidRow(
    column(12, 
      box(title = div(style = 'font-size:25px', tags$b('Introducción')), width = 12, status = 'primary', collapsible = TRUE, collapsed = FALSE,
            h4("Esta aplicación facilita el manejo de datos de los siguientes protocolos de medicion:", tags$br(),
               tags$ul(
                 tags$li(tags$b("Fracción másica de iones plomo, cadmio y calcio, en disoluciones 
                                calibrantes monoelementales, por titulación complejométrica con EDTA."), "M-XX-XX-0000"),
                 tags$li(tags$b("Fracción másica de EDTA en la sal disódica dihidratada del ácido 
                                etilendiamonitetracético, por titulación complejométrica con ion plomo."), "M-XX-XX-0000")
               ), tags$br(), tags$br(),
               'El uso del aplicativo se describe a continuación. 
               Cada módulo contiene instrucciones detalladas para su operación.',
               tags$ol(
                 tags$li("En la página de inicio verifique que la información de los certificados 
                         de calibración de las balanzas que utilizará 
                         se encuentre cargada en el aplicativo."), 
                 tags$li("Diríjase a la opción ", tags$b(icon("fill-drip"), "MRCs y disoluciones"),
                         " para cargar la información de las disoluciones que preparó para la determinación."), 
                 tags$li("Navegue hasta el módulo correspondiente al tipo de titulación que realizará y 
                         realice titulaciones independientes de la muestra de interés."),
                 tags$li("Cuando termine las titulaciones, proceso que puede durar más de un día, combine lo resultados
                         independientes haciendo uso de la pestaña de combinación de resultados en el mismo módulo")),
               tags$br(), tags$br(), 
               "La información de los certificados de calibración de las balanzas 
               se utiliza según se describe en la documentación del paquete de R ", 
               tags$b(a("masscor", href = "https://CRAN.R-project.org/package=masscor"), "."))),
      BalanceCalibCertUI('BalanceCalibCert'),
      box(title = div(style = 'font-size:20px;valign="bottom"', tags$b('Inicio')), 
          width = 5, status = 'primary', collapsible = TRUE, collapsed = FALSE,#height = '300px',
          tags$b("Información general:"),
          tags$div(id = "inline", 
                   textInput("nombre", label = "Nombre: .", width = "100%", value = 'Cristhian Paredes'),
                   textInput("correo", label = "Correo: .", value = "caparedes@inm.gov.co"),
                   dateInput('Fecha', label = 'Fecha: .', language = 'es')),
          tags$br(), 
          checkboxInput('BalanzVerif', label = 'Está disponible el certificado de calibración de la balanza', value = TRUE, width = '100%'), 
          tags$hr(), 
          conditionalPanel('input.BalanzVerif', actionButton('Start1', tags$b('Comenzar')))
      ))
)

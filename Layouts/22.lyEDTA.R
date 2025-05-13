EdtaLy <- fluidRow(
    column(12, 
      tabBox(title = div(style = 'font-size:21px', tags$b('Fraccion masica de EDTA en la sal disodica dihidratada del ácido 
                                etilendiamonitetracético')), 
             width = 12, side = 'right', #status = 'primary', 
             tabPanel(
               title = tags$b('Titular'),
               fluidRow(
                 #verbatimTextOutput(ns('test')),
                 column(1, tags$br()),
                 column(6, tags$br(),
                        tags$div(
                          id = "inline", style = 'font-size:12px', 
                          pickerInput("BalanzaTitEDTA", label = 'Balanza para las titulaciones: .',
                                      choices = CalibCertShow, width = '500px', selected = 'MT XPE 205', multiple = FALSE),
                        ),
                        tags$br(),  
                        uiOutput('EDTA.InitTit'),
                        tags$hr()
                 ),
                 column(4, tags$br(), tags$div(style = 'font-size:12px', uiOutput('PrintDisPb'), uiOutput('PrintDisEDTA.Sample'))),
                 column(12,
                        column(1, tags$br()),
                        tabBox(id = 'EDTA.TabBox', width = 10, side = 'left', height = '1300px')
                 )
               )),       
             tabPanel(title = tags$b('Visualizar y combinar resultados'),
                      CombinaUI('EDTAComb1'))
      )
    )
)

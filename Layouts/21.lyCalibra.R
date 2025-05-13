CalibraMonoLy <- fluidRow(
    column(12, 
      tabBox(title = div(style = 'font-size:21px', tags$b('Fracción masica de iones metalicos en disoluciónes calibrantes monoelementales')), 
             width = 12, side = 'right', #status = 'primary', 
             tabPanel(
               title = tags$b('Titular'),
               fluidRow(
                 #verbatimTextOutput(ns('test')),
                 column(1, tags$br()),
                 column(6, tags$br(),
                        radioButtons('Elemento', label = 'Especie en la disolución calibrante: ', inline = TRUE, 
                                     choices = list('Plomo (II)' = 'Pb', 'Cadmio (II)' = 'Cd', 'Calcio (II)' = 'Ca'), 
                                     selected = 'Cd'),#character(0)),
                        conditionalPanel(
                          condition = 'input.Elemento == "Pb"',
                          tags$div(
                            id = "inline", style = 'font-size:12px',
                            splitLayout(cellWidths = c("50%", "20%"),
                                        numericInput('LeadAM', label = 'Peso atómica del plomo [g mol$^{-1}$]: .', value = 207.20),
                                        numericInput('u_LeadAM', label = ' \u00B1 ', value = 0.06))
                          )
                        ),
                        # tags$hr(),
                        tags$div(
                          id = "inlineTOP", style = 'font-size:12px', 
                          textInput('sampleID', label = 'Identificación muestra: .', value = 'Calibrante'),
                          textAreaInput('dscrMuestraMonoelemTit', label = 'Observaciones:  .', rows = 2, 
                                        placeholder = '(Información adiciónal)', width = '100%'),
                          pickerInput("BalanzaMonoelemTit", label = 'Balanza utilizada: .',
                                      choices = CalibCertShow, width = '500px', selected = 'MT XPE 205', multiple = FALSE),
                        ), 
                        tags$br(),  
                        uiOutput('MonoElemInitTit'),
                        tags$hr()
               ),
               column(4, tags$br(), tags$div(style = 'font-size:12px', uiOutput('PrintDisEDTA'))),
               column(12,
                      column(1, tags$br()),
                      tabBox(id = 'monoElemTabBox', width = 10, side = 'left', height = '1300px')
               )
             )),       
             tabPanel(title = tags$b('Visualizar y combinar resultados'),
                      CombinaUI('CalibraMonoComb1'))
      )
      )
)

EDTA.IndividualUI <- function(id) {
  ns <- NS(id)
  #uiOutput(ns('pestana'))
  tabPanel(title = tags$b(paste0('Tit.', sub("EDTA.", "", id, fixed = TRUE))), 
    column(6, tags$br(), 
           #actionButton(ns('PausarModular'), label = 'Pausar titulaR'),
           tags$b('Datos de la titulacion:'), tags$br(),
           tags$div(id = "inline", style = 'font-size:12px', uiOutput(ns('MasaAlic'))), tags$br(), uiOutput(ns('brwz')), tags$br(),
           conditionalPanel(condition = 'input.MasaAlic > 0', ns = ns,
                            rHandsontableOutput(ns("TitData"), width = '100%'))),
    column(6, tags$br(),
           tags$b('Curva de titulacion:'), tags$br(), 
           fluidRow(column(12, align = 'center', plotOutput(ns('TitCurvePlot'), width = '80%'))), tags$br(),
           actionButton(ns('TermTit'), label = 'Terminar titulacion'), tags$br(),
           uiOutput(ns('TitulTerminada'))
           ))
}

EDTA.IndividualServer <- function(input, output, session, BalanzaTitEDTA, DisPb_MRC, DisMuestraEDTA, IDUsuario, number, devMode, fecha) {
  output$brwz <- renderUI(
    if(devMode()) return(actionButton(session$ns('brwz'), label = tags$b('Pausar módulo'))))
  observeEvent(input$brwz, browser())
  
  sampleID <- DisMuestraEDTA$infoDisSAMPLE()$`ID_Disolucion`
  CondiMasaAlic <- reactive(
    if(!is.null(DisPb_MRC$infoDisMRC())) return(numericInput(session$ns('MasaAlic'), value = 0, 
                                                             label = 'Masa de la alicuota del MRC de ion plomo [g]: .'))
  )
  output$MasaAlic <- renderUI(CondiMasaAlic())
  
  horaInicio <- eventReactive(input$MasaAlic, paste0(fecha(), format(Sys.time(), '_%H-%M')))
  horaFinal  <- eventReactive(input$TermTit, paste0(fecha(), format(Sys.time(), '_%H-%M')))
  
  TableDat_0  <- reactiveValues(hot = data.frame('Titrant' = c(0.0001, rep(NA, 29)),  'Signal' = c(0.1, rep(NA, 29)), 'DerAppr' = c(0.1, rep(NA, 29))))
  TableData <- reactive({
    DT <- NULL
    if (!is.null(input$TitData)) {
      DT <- setDT(hot_to_r(input$TitData))
      TableDat_0[["hot"]]  <-  DT
    } else {#For initial data upload
      if (!is.null(TableDat_0[["hot"]])) {DT <- TableDat_0[["hot"]]}
    }
    if (!is.null(DT)) {
      nDat <- length(na.omit(DT$Signal))
      try(isolate(DT$DerAppr[1:nDat] <- c(NA, abs((DT$Signal[2:nDat] - DT$Signal[1:(nDat - 1)])/(DT$Titrant[2:nDat] - DT$Titrant[1:(nDat - 1)]))/100)))
      #rhandsontable(DT)
      rhandsontable(DT, colHeaders = c('Masa de titulante [g]', 'Diferencia de potencial \n  [mV]', 'Derivada aprox. abs. \n |d(m)/d(V)|'), 
                    readOnly = FALSE, fillHandle = list(direction = 'vertical', autoInsertRow = TRUE)) %>% 
        hot_col(col = 1, type = 'numeric', format = "0.0000") %>% 
        hot_col(col = 2, type = 'numeric', format = "0.0") %>%  
        hot_col(col = 3, type = 'numeric', format = "0.000", readOnly = TRUE, halign = 'htRight') %>% 
        hot_validate_numeric(col = 1, min = 0, allowInvalid = TRUE) %>% 
        hot_heatmap(cols = 3, color_scale = c('#edf2f4', '#9caac6')) %>% 
        hot_table(highlightCol = TRUE, highlightRow = TRUE)
    }
  })
  
  TitCurvDat <- reactive(hot_to_r(input$TitData))
  output$TitData <- renderRHandsontable(TableData())
  
  CleanDf <- eventReactive(input$TermTit, {
    dff <- as.data.frame(TitCurvDat()[(as.numeric(!is.na(TitCurvDat()$Titrant)) + as.numeric(!is.na(TitCurvDat()$Signal))) > 1, ])
    dff <- dff[dff[, 1] > 0, ]
    if(nrow(dff) > 3) {return(df2t.curve(df = dff, plot = FALSE))} else {return()}})
  
  TitCurvePlot <- reactive(
    tryCatch(
      expr = {
        if(length(na.omit(TitCurvDat()$Titrant)) >= 3) {
          if(input$TermTit < 1) {
            plot(TitCurvDat()$Signal[TitCurvDat()$Titrant != 0] ~ TitCurvDat()$Titrant[TitCurvDat()$Titrant != 0])
          } else {
            plotCurve(curve = CleanDf(), xlab = 'Titulante [g]', ylab = 'Señal [mV]')
          }
        } else {
          plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
          text(x = 0.5, y = 0.5, paste("No hay suficientes datos..."), 
               cex = 1.6, col = "black")
        }}, 
      error = function(cond) {
        plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
        text(x = 0.5, y = 0.5, paste("No hay datos..."), 
             cex = 1.6, col = "black")
             })
  )
  
  TitulTerminada <- eventReactive(input$TermTit, {
    if(length(na.omit(TitCurvDat()$Titrant)) < 6) {
      tags$b('No se puede terminar la titulacion con menos de 7 datos!')
    } else {
      tags$div(
        tags$hr(),
        tags$b('Resultados de la titulación:'), tags$br(),
        tags$div(style = 'font-size:12px', 
                 infoBox(
                   width = 12, title = tags$b('Fraccion masica de EDTA en la muestra solida: '), icon = icon("vials"), color = 'black', 
                   value = tags$div(tags$b(round(ResParcial(), 3), ' %')#, renderPrint(summaryTitration())
                                    ), 
                   subtitle = 'Exporte y guarde el archivo de resultados individuales.´'), tags$br(), tags$br(),
                downloadButton(session$ns('DwnlResFile'), label = tags$b('Descargar archivo .tit')), tags$br(), tags$br())
      )
    }
  })
  MasaEquiv <- reactive(try(EP.1stDer(curve = CleanDf())))
  ResParcial <- reactive(input$MasaAlic * DisPb_MRC$infoDisMRC()$`Concentración [mmol/kg]` / MasaEquiv() *
                           DisMuestraEDTA$infoDisSAMPLE()$`Peso molar`[1] * DisMuestraEDTA$infoDisSAMPLE()$`Factor de dilucion 1:` /
                          10^6 * 100)
  ResParcUnc <- reactive(propagate(expr = expression(Mali * C_Pb * FD / (Meq - Mbln) * MW / 10^6 * 100),
                                   data = cbind(Meq = c(convMass(CalibCertList[[BalanzaTitEDTA]], reading = MasaEquiv()),
                                                        sqrt(2) * uncertConvMass(CalibCertList[[BalanzaTitEDTA]], reading = MasaEquiv(), 
                                                                                 d = 0.1, d.units = 'mg')),
                                                Mbln = c(0, 0.0028/(2 * sqrt(3))),
                                                C_Pb = c(DisPb_MRC$infoDisMRC()$`Concentración [mmol/kg]`,
                                                          DisPb_MRC$infoDisMRC()$`Incertidumbre [mmol/kg]`),
                                                FD = c(DisMuestraEDTA$infoDisSAMPLE()$`Factor de dilucion 1:`,
                                                       DisMuestraEDTA$infoDisSAMPLE()$`Incertidumbre FD`),
                                                Mali = c(convMass(CalibCertList[[BalanzaTitEDTA]], reading = input$MasaAlic),
                                                         uncertConvMass(CalibCertList[[BalanzaTitEDTA]], reading = input$MasaAlic, 
                                                                        d = 0.1, d.units = 'mg')),
                                                MW = DisMuestraEDTA$infoDisSAMPLE()$`Peso molar`),
                                   second.order = FALSE, do.sim = FALSE
                                   ))
  
  summaryTitration <- reactive(
    list(Muestra = sampleID, MasaAlicuota = input$MasaAlic, 
         MasaEquiv = MasaEquiv(), 'Fracción másica [%]' = ResParcial(), 'Incertidumbre estandar [%]' = ResParcUnc(),
         'Disolucion muestra' = DisMuestraEDTA$infoDisSAMPLE(),
         'Disolucion titulante' = DisPb_MRC$infoDisMRC(),
         'Certificado calibracion balanza' = CalibCertList[[BalanzaTitEDTA]],
         Analista = IDUsuario(),#[1], correoAnalista = IDUsuario()[2],
         TitCurvDat = TitCurvDat()[complete.cases(TitCurvDat()), ],
         Inicio = horaInicio(),
         Final = horaFinal()
         ))
  
  output$DwnlResFile <- downloadHandler(
    filename = function() {paste0("EDTA.", sampleID, ".", number, "_", isolate(horaInicio()), ".tit")},
    content = function(file) {saveRDS(summaryTitration(), file = file)}, contentType = NULL)
  
  output$TitCurvePlot <- renderPlot(TitCurvePlot())
  output$TitulTerminada <- renderUI(TitulTerminada())
}
CalibraMonoIndividualUI <- function(id) {
  ns <- NS(id)
  #uiOutput(ns('pestana'))
  tabPanel(title = tags$b(paste0('Tit.', sub("monoElemTit", "", id, fixed = TRUE))), 
    column(6, tags$br(),
           tags$b('Datos de la titulación:'), tags$br(),
           tags$div(id = "inline", style = 'font-size:12px', uiOutput(ns('MasaAlic')), uiOutput(ns('MasaEDTA0'))), tags$br(), 
           uiOutput(ns('brwz')),
           tags$br(),
           conditionalPanel(condition = 'input.MasaAlic > 0', ns = ns,
                            rHandsontableOutput(ns("TitData"), width = '100%'))),
    column(6, tags$br(),
           tags$b('Curva de titulación:'), tags$br(), 
           fluidRow(column(12, align = 'center', plotOutput(ns('TitCurvePlot'), width = '80%'))), tags$br(),
           actionButton(ns('TermTit'), label = 'Terminar titulación'), tags$br(),
           uiOutput(ns('TitulTerminada'))
           ))
}

CalibraMonoIndividualServer <- function(input, output, session, Elemento, LeadAM, u_LeadAM,
                                        sampleID, dscrMuestraMonoelemTit,
                                        BalanzaMonoelemTit,
                                        DisEDTA_MRC, IDUsuario, number, devMode, fecha) {
  output$brwz <- renderUI(
    if(devMode()) return(actionButton(session$ns('brwz'), label = tags$b('Pausar módulo'))))
  observeEvent(input$brwz, browser())
  
  CondiMasaAlic <- reactive(
    if(!is.null(DisEDTA_MRC$infoDisMRC())) return(numericInput(session$ns('MasaAlic'), value = 0, label = 'Masa de alicuota [g]: .'))
  )
  output$MasaAlic <- renderUI(CondiMasaAlic())
  
  CondiMasaEDTA0 <- reactive(
    if(!is.null(DisEDTA_MRC$infoDisMRC())) return(numericInput(session$ns('MasaEDTA0'), value = 0, label = 'Masa inicial de la disolución de EDTA [g]: .'))
  )
  output$MasaEDTA0 <- renderUI(CondiMasaEDTA0())
  
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
    dff[, 1] <- dff[, 1] + input$MasaEDTA0
    if(nrow(dff) > 3) {
      return(df2t.curve(df = dff, plot = FALSE))
    } else {
      return()
    }
  })
  
  # observeEvent(input$TermTit, { # https://stackoverflow.com/questions/54652364/r-shiny-automatically-start-download
  #   if (is.numeric(ResParcial())) {
  #     runjs(paste0("$('#", number(), "-DwnlResFile')[0].click();"))
  #   }
  # })
  
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
      tags$b('No se puede terminar la titulación con menos de 7 datos!')
    } else {
      tags$div(
        tags$hr(),
        tags$b('Resultados de la titulación:'), tags$br(),
        tags$div(style = 'font-size:12px', 
                 infoBox(
                   width = 12, title = tags$b('Fracción másica del elemento en la alícuota: '), icon = icon("vials"), color = 'black', 
                   value = tags$div(tags$b(round(ResParcial(), 3), ' [mg/kg]')#, renderPrint(summaryTitration())
                                    ), 
                   subtitle = 'Exporte y guarde el archivo de resultados individuales.´'), tags$br(), tags$br(),
                downloadButton(session$ns('DwnlResFile'), label = tags$b('Descargar archivo .tit')), tags$br(), tags$br())
      )
    }
  })
  MasaEquiv <- eventReactive(input$TermTit, {try(EP.1stDer(curve = CleanDf()))})
  MasAtoElem <- reactive(ifelse(Elemento == 'Pb', LeadAM, ElementsAtomicMass[[Elemento]][1]))
  u_MasAtoElem <- reactive(ifelse(Elemento == 'Pb', u_LeadAM, ElementsAtomicMass[[Elemento]][2]))
  ResParcial <- reactive(MasaEquiv() * DisEDTA_MRC$infoDisMRC()$`Concentración [mmol/kg]` / input$MasaAlic * MasAtoElem())
  ResParcUnc <- reactive(propagate(expr = expression((Meq - Mbln) * Cedta / Mali * Mato),
                                   data = cbind(Meq = c(convMass(CalibCertList[[BalanzaMonoelemTit]], reading = MasaEquiv()),
                                                        sqrt(2) * uncertConvMass(CalibCertList[[BalanzaMonoelemTit]], reading = MasaEquiv(), 
                                                                                 d = 0.1, d.units = 'mg')),
                                                Mbln = c(0, 0.0028/(2*sqrt(3))),
                                                Cedta = c(DisEDTA_MRC$infoDisMRC()$`Concentración [mmol/kg]`,
                                                          DisEDTA_MRC$infoDisMRC()$`Incertidumbre [mmol/kg]`),
                                                Mali = c(convMass(CalibCertList[[BalanzaMonoelemTit]], reading = input$MasaAlic),
                                                         uncertConvMass(CalibCertList[[BalanzaMonoelemTit]], reading = input$MasaAlic)),
                                                Mato = c(MasAtoElem(), u_MasAtoElem())),
                                   second.order = FALSE, do.sim = FALSE
                                   ))
  
  summaryTitration <- reactive(
    list(Muestra = sampleID, Elemento = Elemento, MasaAlicuota = input$MasaAlic, 
         MasaEquiv = MasaEquiv(), 'Fracción másica [mg/kg]' = ResParcial(), 'Incertidumbre estandar' = ResParcUnc(), 
         'Disolución titulante' = DisEDTA_MRC$infoDisMRC(),
         'Certificado calibración balanza' = CalibCertList[[BalanzaMonoelemTit]],
         Analista = IDUsuario(),#[1], correoAnalista = IDUsuario()[2],
         TitCurvDat = TitCurvDat()[complete.cases(TitCurvDat()), ],
         dscripMuestra = dscrMuestraMonoelemTit, 
         MasAtoElem = c(MasAtoElem(), u_MasAtoElem()),
         Inicio = horaInicio(),
         Final = horaFinal()
         ))
  
  output$DwnlResFile <- downloadHandler(
    filename = function() {paste0(Elemento, "_", sampleID, ".", number, "_", isolate(horaInicio()), ".tit")},
    content = function(file) {saveRDS(summaryTitration(), file = file)}, contentType = NULL)
  
  output$TitCurvePlot <- renderPlot(TitCurvePlot())
  output$TitulTerminada <- renderUI(TitulTerminada())
  output$DescaResu <- renderUI(DescaResu())
  return(list('Titulación' = summaryTitration#, 
              #'Exito' = !is.null(is.numeric(ResParcial()))
              ))
}
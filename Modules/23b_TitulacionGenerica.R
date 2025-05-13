GenericCurveUI <- function(id) {
  ns <- NS(id)
  #uiOutput(ns('pestana'))
  fluidRow(
    column(12, div(style = 'font-size:21px', tags$b('Curva de titulación genérica')), tags$br()),
    column(5, offset = 1, tags$b('Datos de la titulacion:'), tags$br(), uiOutput(ns('brwz')), tags$br(),
           rHandsontableOutput(ns("TitData"), width = '100%')),
    column(6, tags$b('Curva de titulacion:'), tags$br(), 
           plotOutput(ns('TitCurvePlot'), width = '80%')), tags$br(),
           actionButton(ns('TermTit'), label = 'Terminar titulacion'), tags$br(),
           uiOutput(ns('TitulTerminada')))
}

GenericCurveServer <- function(input, output, session, devMode) {
  output$brwz <- renderUI(if(devMode()) return(actionButton(session$ns('brwz'), label = tags$b('Pausar módulo'))))
  observeEvent(input$brwz, browser())
  
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
    if(length(na.omit(TitCurvDat()$Titrant)) < 5) {
      tags$b('No se puede terminar la titulacion con menos de 6 datos!')
    } else {
      tags$div(
        tags$hr(),
        tags$b('Resultados de la titulación:'), tags$br(),
        tags$div(style = 'font-size:12px', 
                 infoBox(width = 4, title = tags$b('Masa de equivalencia: '), icon = icon("vials"), color = 'black', 
                         value = tags$b(round(MasaEquiv(), 5), ' g')))
      )
    }
  })
  MasaEquiv <- reactive(try(EP.1stDer(curve = CleanDf())))
 
  output$TitCurvePlot <- renderPlot(TitCurvePlot())
  output$TitulTerminada <- renderUI(TitulTerminada())
}
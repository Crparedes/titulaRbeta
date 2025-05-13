CombinaUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    #verbatimTextOutput(ns('test')),
    column(3, 
           tags$b('Importar archivos .tit de resultados individuales'),
           fileInput(ns('TitFiles'), width = '100%', 
                     label = NULL, multiple = TRUE, accept = '.tit'),
           uiOutput(ns('buttonUpload')), uiOutput(ns('brwz')), tags$hr(),
           uiOutput(ns('visualizacion')),
           conditionalPanel(condition = 'input.visualizacion == "Comb"', ns = ns, uiOutput(ns('titFilesSelectComb'))),
           conditionalPanel(condition = 'input.visualizacion == "Indi"', ns = ns, uiOutput(ns('titFilesSelectIndi'))), 
           conditionalPanel(condition = 'input.buttonUpload > 0', ns = ns, 
                            uiOutput(ns('Calcular'))),
           tags$hr(),
           #radioButtons(ns('independCriteria'), label = 'Criterio de independiencia', 
           #             choices = list('Diferente dia' = 'dia', 'Diferente disolucion titulante' = 'DifDisTit'), 
           #             inline = TRUE, selected = 'DifDisTit'),
           verbatimTextOutput(ns('printed'))
           ),
    column(9, 
           conditionalPanel(condition = 'input.visualizacion == "Comb"', ns = ns, 
                            tags$b('Combinación de resultados'), tags$br(),
                            column(7, plotOutput(ns('plotCombinados'), width = '100%')),
                            column(5, box(title = tags$b('Resumen de resultados combinados'), width = 12, status = 'primary',
                                          tableOutput(ns('resultadosCombi'))), tags$br(),
                                   uiOutput(ns('DescDigit.SIBttn')), tags$hr(), 
                                   uiOutput(ns('DescMatDarBttn')), uiOutput(ns('DescMatExcelBttn'))), tags$hr(), tags$br(), tags$hr(), 
                            column(12, uiOutput(ns('TablasPorDia'))),
                            tags$hr()),
           conditionalPanel(condition = 'input.visualizacion == "Indi"', ns = ns, 
                            tags$b('Visualizacion de resultado individual'), tags$hr(),
                            column(7, plotOutput(ns('plotIndiv'), width = '100%')),
                            column(5, uiOutput(ns('resIndiv'))))#,
           #uiOutput(ns('Cajas'))
    ))
}

CombinaServer <- function(input, output, session, IDUsuario, especie, tol, devMode, fecha) {
  output$brwz <- renderUI(
    if(devMode()) return(actionButton(session$ns('brwz'), label = tags$b('Pausar módulo'))))
  observeEvent(input$brwz, browser())
  
  FileNames <- reactive(input$TitFiles$name)
  
  ifelsebuttonUpload <- reactive({
    dplyr::case_when(especie == 'EDTA' ~ all(substr(FileNames(), start = 1, stop = 5) == 'EDTA.'),
              especie == 'Elem' ~ length(unique(substr(FileNames(), start = 1, stop = 3))) == 1)})
  
  
  buttonUpload <- eventReactive(input$TitFiles, {
    ifelse(ifelsebuttonUpload(),
           return(actionButton(session$ns('buttonUpload'), label = tags$b('Subir archivos'))),
           return(box(status = 'danger', width = 12, 
                      dplyr::case_when(especie == 'EDTA' ~ list(tags$b('Todos los archivos deben ser de muestras de EDTA. Intente nuevamente')),
                                especie == 'Elem' ~ list(tags$b('Todos los archivos deben corresponder al mismo elemento. Intente nuevamente'))))))})
  output$buttonUpload <- renderUI(buttonUpload())
  
  visualizacion <- eventReactive(input$buttonUpload, {
    radioButtons(session$ns('visualizacion'), label = NULL,
                 choices = list('Combinar los resultados' = 'Comb', 'Visualizar resultado individual' = 'Indi'))})
  output$visualizacion <- renderUI(visualizacion())
  
  Calcular <- reactive(
    ifelse(length(unique(substr(FileNames(), start = 1, stop = 3))) == 1,
           return(actionButton(session$ns('Calcular'), label = tags$b('Mostrar/recalcular resultados'))),
           return(NULL)))
  output$Calcular <- renderUI(Calcular())
  
  
  DataCompl <- reactiveValues()
  observeEvent(input$TitFiles, {
    #DataCompl <- reactiveValues()
    for (i in 1:length(FileNames())) DataCompl[[FileNames()[i]]] <- readRDS(input$TitFiles[i, ]$datapath)
    #browser()
  })
  
  # DataComplList <- reactive({
  #   x <- reactiveValuesToList(DataCompl)
  #   x <- x[names(x) %in% FileNames()] # To clean old files
  #   return(x)})
  
  titFilesSelectComb <- reactive({
    x <- reactiveValuesToList(DataCompl)
    x <- x[names(x) %in% FileNames()]
    pos <- dplyr::case_when(especie == 'EDTA' ~ c(12, 11),
                     especie == 'Elem' ~ c(13, 13))
    Fechas <- as.factor(unlist(sapply(x, 
                                      function(x) {substr(as.character(x[[pos[1]]]), 
                                                          start = nchar(as.character(x[[pos[1]]])) - 18, 
                                                          stop = nchar(as.character(x[[pos[1]]])) - 3)})))
    VecMomento <- as.factor(unlist(sapply(x, function(x) {x[[pos[2]]]})))
    choices <- names(x)[order(VecMomento)]
    #browser()
    return(checkboxGroupInput(session$ns('titFilesSelectComb'), label = tags$b("Archivos a considerar:"), 
                              choices = choices, selected = choices, width = '100%'))
  })
  output$titFilesSelectComb <- renderUI(titFilesSelectComb())
  
  DataCleanDF <- eventReactive(input$Calcular, {#browser()
    pos <- dplyr::case_when(especie == 'EDTA' ~ c(11, 2, 3, 4, 5, 12),
                     especie == 'Elem' ~ c(13, 3, 4, 5, 6, 13))
    
    x <- reactiveValuesToList(DataCompl)
    x <- x[names(x) %in% FileNames()]
    DataTrimmedList <- x[names(x) %in% input$titFilesSelectComb] # To consider only selected files
    VecMomento <- as.factor(unlist(sapply(DataTrimmedList, function(x) {x[[pos[1]]]})))
    
    VecElement <- ifelse(especie == 'EDTA', rep(NA, length(VecMomento)),
                         ifelse(especie == 'Elem', unlist(sapply(DataTrimmedList, function(x) {x[[2]]}))[order(VecMomento)], NULL))
    VecMuestra <- unlist(sapply(DataTrimmedList, function(x) {x[[1]]}))[order(VecMomento)]
    VecDescrip <- ifelse(especie == 'EDTA', 
                         unlist(sapply(DataTrimmedList, function(x) {ifelse(!is.null(x[[6]][[3]]), x[[6]][[3]], x[[6]][[2]])}))[order(VecMomento)],
                         ifelse(especie == 'Elem', unlist(sapply(DataTrimmedList, function(x) {x[[11]]}))[order(VecMomento)], NULL))
    VecMasaAli <- as.numeric(unlist(sapply(DataTrimmedList, function(x) {x[[pos[2]]]})))[order(VecMomento)]
    VecMasaEqi <- as.numeric(unlist(sapply(DataTrimmedList, function(x) {x[[pos[3]]]})))[order(VecMomento)]
    VecFraccMa <- as.numeric(unlist(sapply(DataTrimmedList, function(x) {x[[pos[4]]]})))[order(VecMomento)]
    VecFracUnc <- as.numeric(unlist(sapply(DataTrimmedList, function(x) {x[[pos[5]]]$prop[[3]]})))[order(VecMomento)]
    VecFechas0 <- as.factor(unlist(sapply(DataTrimmedList, function(x) {substr(as.character(x[[pos[6]]]), start = 1, stop = 10)})))[order(VecMomento)]
    #browser()
    x <- data.frame(VecElement, VecMuestra, VecDescrip, VecMasaAli, VecMasaEqi, VecFraccMa, VecFracUnc, VecFechas0,
                    index = 1:length(VecFracUnc))
    #browser()
    return(x)
  })
  
  DescMatDarBttn <- eventReactive(DataCleanDF(), {
    downloadButton(session$ns('DescMatDar'), label = tags$b('Descargar matriz de resultados en RDS'))})
  output$DescMatDarBttn <- renderUI(DescMatDarBttn())
  output$DescMatDar <- downloadHandler(
    filename = function() {paste0("MatrizResultados_", fecha(), format(Sys.time(), '_%H-%M'), '.rds')},
    content = function(file) {saveRDS(DataCleanDF(), file = file)}, contentType = NULL)
  
  DescMatExcelBttn <- eventReactive(DataCleanDF(), {
    downloadButton(session$ns('DescMatExcel'), label = tags$b('Descargar matriz de resultados en Excel'))})
  output$DescMatExcelBttn <- renderUI(DescMatExcelBttn())
  output$DescMatExcel <- downloadHandler(
    filename = function() {paste0("MatrizResultados_", fecha(), format(Sys.time(), '_%H-%M'), '.xlsx')},
    content = function(file) {write_xlsx(x = DataCleanDF(), path = file, format_headers = TRUE)}, contentType = NULL)
  
  DescDigit.SIBttn <- eventReactive(DataCleanDF(), {
    downloadButton(session$ns('DescDigit.SI'), label = tags$b('Descargar resultados en SI Digital (XML)'))})
  output$DescDigit.SIBttn <- renderUI(DescDigit.SIBttn())
  output$DescDigit.SI <- downloadHandler(
    filename = function() {paste0("MatrizResultados_", fecha(), format(Sys.time(), '_%H-%M'), '.xlsx')},
    content = function(file) {write_xlsx(x = DataCleanDF(), path = file, format_headers = TRUE)}, contentType = NULL)
  
  
      
  resultadosCombi <- eventReactive(DataCleanDF(), {
    AverageValue <- mean(DataCleanDF()$VecFraccMa)
    IncertTipoB <- max(DataCleanDF()$VecFracUnc)
    StandarDev <- sd(DataCleanDF()$VecFraccMa)
    n_ind <- length(unique((DataCleanDF()$VecFechas0)))
    IncertTipoA <- StandarDev/sqrt(n_ind)
    IncertComb <- sqrt(IncertTipoB^2 + IncertTipoA^2)
    LevTest <- tryCatch(leveneTest(VecFraccMa ~ VecFechas0, data = DataCleanDF()), error = function(e) 'no aplica')
    return(data.frame('.' = c('Promedio de las mediciones', 'Incertidumbre tipo B', 'Desviacion estandar de las mediciones', 
                              'Numero de datos', 'Numero de datos independientes (dia)', 'Incertidumbre tipo A', 'Incertidumbre combinada',
                              'Incertidumbre expandida (k=2)', 'Valor p homogeneidad de varianzas (Levene)'),
                      'Valor' = as.character(c(round(c(AverageValue, IncertTipoB, StandarDev), 3), 
                                               round(c(length((DataCleanDF()$VecFechas0)), n_ind)),
                                               round(c(IncertTipoA, IncertComb, IncertComb * 2), 3),
                                               ifelse(n_ind > 1, round(LevTest$`Pr(>F)`[1], 4), LevTest))),
                      'Unidades' = c(rep(unidad, 3), rep('', 2), rep(unidad, 3), '')))
  })
  
  plotCombinados <- reactive({
    if (especie == 'EDTA') {
      ylab <- expression(paste('Fracción masica de EDTA / g ', ' ', g^{-1}, ' (%)'))
    } else {
      if (especie == 'Elem') {
        ylab <- expression(paste('Fracción masica del elemento / ', 'mg k', g^{-1}))
      }
    }
    
    p <- ggplot(data = DataCleanDF(), aes(x = index)) + theme_bw() + 
      labs(y = ylab, x = NULL) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            axis.text.y = element_text(color = "black"),
            #axis.ticks.x = element_blank(), 
            axis.text.x = element_blank(), legend.title = element_blank()) +
      scale_y_continuous(expand = c(0, 0.4), n.breaks = 8) +
      #geom_hline(aes(yintercept = 0.999 * mean(VecFraccMa)), linetype = 4, lwd = 0.5, col = 'red1') +
      geom_hline(aes(yintercept = (1 - tol) * mean(VecFraccMa)), linetype = 2, lwd = 0.5, col = 'gray60') +
      geom_hline(aes(yintercept = mean(VecFraccMa)), linetype = 1, lwd = 0.5, col = 'gray60') +
      geom_hline(aes(yintercept = (1 + tol) * mean(VecFraccMa)), linetype = 2, lwd = 0.5, col = 'gray60') +
      #geom_hline(aes(yintercept = 1.001 * mean(VecFraccMa)), linetype = 4, lwd = 0.5, col = 'red1') +
      geom_point(aes(y = VecFraccMa, color = VecFechas0)) + 
      geom_errorbar(aes(ymin = VecFraccMa - VecFracUnc, ymax = VecFraccMa + VecFracUnc, color = VecFechas0), width = 0.4)
    #browser()
    print(p)
    })
  output$plotCombinados <- renderPlot(plotCombinados())
  output$resultadosCombi <- renderTable(resultadosCombi())
  
  unidad <- dplyr::case_when(especie == 'EDTA' ~ 'g / g (%)', especie == 'Elem' ~ 'mg / kg')
  
  TablasPorDia <- eventReactive(DataCleanDF(), {
    x <- list()
    for (i in unique((DataCleanDF()$VecFechas0))) {
      j <- i
      DayRes <- which(DataCleanDF()$VecFechas0 == j)
      datFram <- data.frame(Archivo = row.names(DataCleanDF())[DayRes],
                            Resultado = DataCleanDF()$VecFraccMa[DayRes], 
                            '.' = rep(unidad, length(DayRes)))
      nDat <- nrow(datFram)
      temp <- box(title = tags$b(paste0('Resumen de resultados del ', j)), status = 'primary', collapsible = TRUE, collapsed = FALSE,
                  column(5, #renderTable(isolate(datFram), digits = 3),
                         tags$h4('Promedio del día:', tags$b(round(mean(datFram$Resultado), 3), unidad), tags$br(),
                                 'Desviación estándar relativa del día:', tags$b(round(sd(datFram$Resultado)/mean(datFram$Resultado)*100, 3), '%'))),
                  column(7, tags$h4('Valor p prueba de normalidad de Shapiro-Wilk:', 
                                    tags$b(tryCatch(signif(shapiro.test(datFram$Resultado)$p.value, 3), 
                                                    error = function(e) 'No se puede calcular para menos de tres datos')), 
                                    tags$br(),
                                    'Valores p de las pruebas de datos anómalos de Grubbs:', tags$br(),
                                    tryCatch(
                                      tags$h4('  · ', tags$b(signif(grubbs.test(datFram$Resultado, type = 10)$p.value, 3)),
                                              ' para el valor más ', 
                                              ifelse(word(grubbs.test(datFram$Resultado, type = 10)$alternative, 1) == 'highest', 'alto.', 'bajo.')),
                                      error = function(e) 'No se puede calcular para menos de tres datos'),
                                    tryCatch(
                                      tags$h4('  · ', tags$b(signif(grubbs.test(datFram$Resultado, type = 11)$p.value, 3)),
                                              ' para un valor a cada extremo.', tags$br(),
                                              '  · ', tags$b(signif(grubbs.test(datFram$Resultado, type = 20)$p.value, 3)), 
                                              ' para los dos valores más ', 
                                              ifelse(word(grubbs.test(datFram$Resultado, type = 10)$alternative, 1) == 'highest', 'altos.', 'bajos.')),
                                      error = function(e) ''))
                                    ))
      x <- c(x, temp)
    }
    return(x)
  })
  output$TablasPorDia <- renderUI(TablasPorDia())
  
  
  titFilesSelectIndi <- reactive({
    x <- reactiveValuesToList(DataCompl)
    x <- x[names(x) %in% FileNames()]
    pos <- dplyr::case_when(especie == 'EDTA' ~ c(12, 11),
                     especie == 'Elem' ~ c(13, 13))
    Fechas <- as.factor(unlist(sapply(x, 
                                      function(x) {substr(as.character(x[[pos[1]]]), 
                                                          start = nchar(as.character(x[[pos[1]]])) - 18, 
                                                          stop = nchar(as.character(x[[pos[1]]])) - 3)})))
    VecMomento <- as.factor(unlist(sapply(x, function(x) {x[[pos[2]]]})))
    choices <- names(x)[order(VecMomento)]
    #browser()
    return(radioButtons(session$ns('titFilesSelectIndi'), label = tags$b("Archivo:"), 
                        choices = choices, #selected = character(0), 
                        width = '100%'))
  })
  output$titFilesSelectIndi <- renderUI(titFilesSelectIndi())
  
  meanValues <- reactive(
    NULL
  )
  
  output$printed <- renderPrint({
  #  req(input$TitFiles)
  #  (length(input$TitFiles$name))
  })
  
}
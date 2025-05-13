SolidSampleUI <- function(id, reagent, reagKey, explan, nu = FALSE) {
  ns <- NS(id)
  box(title = div(style = 'font-size:18px;valign="bottom"', tags$b('Muestra de ', reagent)), 
      width = 12, status = 'primary', collapsible = TRUE, collapsed = FALSE,
      h5(explan),
      radioButtons(ns('SourceOption'), label = '¿Qué desea hacer?', 
                   choices = list('Crear disolución nueva a partir del reactivo solido' = 'daCapo',
                                  "Subir un archivo '.dis' generado anteriormente" = 'archivo'), 
                   selected = 'archivo'),
      tags$hr(),
      conditionalPanel(
        condition = 'input.SourceOption == "daCapo"', ns = ns,
        column(12, tags$div(
          id = "inline", style = 'font-size:12px', 
          textInput(ns('IDMuestra'), label = 'Identificación muestra: .', value = paste0('Muestra_', reagKey), width = '100%'),
          textInput(ns('dscrMuestra'), label = 'Observaciones:  .', placeholder = '(Información adiciónal)', width = '100%')), 
          tags$hr()),
          box(title = div(style = 'font-size:14px', 'Preparacion de la disolucion'), 
              width = 12, collapsible = TRUE, collapsed = TRUE, status = 'primary',
              tags$b('Condiciones ambientales'),
              tags$div(id = "inline", style = 'font-size:12px',
                       splitLayout(cellWidths = c("75%", "25%"),
                                   numericInput(ns('Temp1'), label = 'Temperatura [$^o$C]: .', value = 18),
                                   numericInput(ns('u_Temp1'), label = '\u00B1', value = 1.8)),
                       splitLayout(cellWidths = c("75%", "25%"),
                                   numericInput(ns('BarPres1'), label = 'Presion [hPa]: .', value = 750),
                                   numericInput(ns('u_BarPres1'), label = '\u00B1', value = 2)),
                       splitLayout(cellWidths = c("75%", "25%"),
                                   numericInput(ns('relHum1'), label = 'Humedad relativa [%]: .', value = 45),
                                   numericInput(ns('u_relHum1'), label = '\u00B1', value = 3))),
              uiOutput(ns("NiceDensitAir")), tags$hr(),
              tags$b('Masa del reactivo sólido'),
              tags$div(id = "inline", style = 'font-size:12px', 
                       pickerInput(ns("CalibCertSample"), label = 'Balanza utilizada: .',
                                   choices = CalibCertShow, width = '100%', selected = 'MT XPE 205', multiple = FALSE)),
              tags$div(id = "inline", style = 'font-size:12px',
                       numericInput(ns('MasRec1'), label = 'Masa del recipiente [g]: .', value = 0),
                       numericInput(ns('MasSAMPLE1'), label = 'Masa del Reactivo sólido [g]: .', value = 0),
                       numericInput(ns('MasRecSAMPLE1'), label = 'Masa conjunta [g]: .', value = 0),
                       uiOutput(ns('deriMasaSAMPLE'))), tags$hr(),
              tags$b('Masa final de la disolucion'),
              uiOutput(ns('CalibCertDis')),
              tags$div(id = "inline", style = 'font-size:12px',
                       numericInput(ns('MasRec2'), label = 'Masa del recipiente [g]: .', value = 0),
                       numericInput(ns('MasDis1'), label = 'Masa final disolucion [g]: .', value = 0),
                       numericInput(ns('MasRecDis1'), label = 'Masa conjunta [g]: .', value = 0),
                       splitLayout(cellWidths = c("75%", "25%"),
                                   numericInput(ns('DensitDis'), label = 'Densidad disolucion [g cm$^{-3}$]: .', value = 1.000),
                                   numericInput(ns('u_DensitDis'), label = '\u00B1', value = 0.004)),
                       uiOutput(ns('deriMasaDisSAMPLE'))))),
      conditionalPanel(
        condition = 'input.SourceOption == "archivo"', ns = ns,
        fileInput(ns('DisFile'), label = 'Escoja el archivo', multiple = FALSE, accept = '.dis')),
      tags$hr(), tags$hr(), 
      uiOutput(ns('buttonCalc')), uiOutput(ns('brwz')), tags$br(), #tags$br(),
      uiOutput(ns('InfoDisBox'))
  )
}

SolidSampleServer <- function(input, output, session, reagKey, IDUsuario, DensitSAMPLE, molarWeight, devMode, fecha) {
  output$brwz <- renderUI(
    if(devMode()) return(actionButton(session$ns('brwz'), label = tags$b('Pausar módulo'))))
  observeEvent(input$brwz, browser())
  
  DensitAir <- reactive(c(airDensity(Temp = input$Temp1, p = input$BarPres1, h = input$relHum1),
                          uncertAirDensity(Temp = input$Temp1, p = input$BarPres1, h = input$relHum1, 
                                           u_Temp = input$u_Temp1, u_p = input$u_BarPres1, u_h = input$u_relHum1, printRelSD = FALSE)))
  
  derMassSAMPLE <- reactive(input$MasRecSAMPLE1 - input$MasSAMPLE1 - input$MasRec1)
  masSAMPLE <- reactive(mean(input$MasSAMPLE1, input$MasRecSAMPLE1 - input$MasRec1))
  derMassDis <- reactive(input$MasRecDis1 - input$MasDis1 - input$MasRec2)
  masDis <- reactive(mean(input$MasDis1, input$MasRecDis1 - input$MasRec2))
  CalibCertDis <- reactive(tags$div(id = "inline", style = 'font-size:12px', 
                                    pickerInput(session$ns("CalibCertDis"), 
                                                label = 'Balanza utilizada: .',
                                                choices = CalibCertShow, selected = input$CalibCertSample, width = '100%', multiple = FALSE)))
  
  convMassSAMPLE <- reactive(c(convMass(calibCert = CalibCertList[[input$CalibCertSample]], reading = masSAMPLE(), units = 'g'),
                            uncertConvMass(calibCert = CalibCertList[[input$CalibCertSample]], reading = masSAMPLE(), units = 'g')))
  BuoySAMPLE <- reactive(c(MABC(rho = DensitSAMPLE[1], rho_air = DensitAir()[1]),
                        uncertMABC(rho = DensitSAMPLE[1], rho_air = DensitAir()[1], 
                                   u_rho = DensitSAMPLE[2], u_rho_air = DensitAir()[2], printRelSD = FALSE)))
  
  convMassDis <- reactive(c(convMass(calibCert = CalibCertList[[input$CalibCertDis]], reading = masDis(), units = 'g'),
                            uncertConvMass(calibCert = CalibCertList[[input$CalibCertDis]], reading = masDis(), units = 'g')))
  BuoyDis <- reactive(c(MABC(rho = input$DensitDis, rho_air = DensitAir()[1]),
                        uncertMABC(rho = input$DensitDis, rho_air = DensitAir()[1], 
                                   u_rho = input$u_DensitDis, u_rho_air = DensitAir()[2], printRelSD = FALSE)))
  
  factorDilucion <- reactive({
    propagate(expr = expression((convMassDis * BuoyDis) / (convMassSAMPLE * BuoySAMPLE)),
              data = cbind(convMassSAMPLE = convMassSAMPLE(), BuoySAMPLE = BuoySAMPLE(), 
                           convMassDis = convMassDis(), BuoyDis = BuoyDis()),
              do.sim = FALSE)
  })
  
  AppMolConc <- reactive(convMassSAMPLE()[1] / molarWeight[1] / convMassDis()[1] * 1000000)
  
  infoDisSAMPLE <- eventReactive(input$buttonCalc, {
    if (input$SourceOption == "daCapo") {
      if (!is.na(factorDilucion()$prop[[1]] > 0) && !is.na(factorDilucion()$prop[[3]] > 0)) {
        return(list('Muestra' = reagKey,
                    'ID_Disolucion' = input$IDMuestra,
                    'Descripcion' = input$descrMuestra,
                    'Conc.Aprox [mmol/kg]' = AppMolConc(),
                    'Factor de dilucion 1:' = signif(factorDilucion()$prop[[1]], 7),
                    'Incertidumbre FD' = signif(factorDilucion()$prop[[3]], 4),
                    'Peso molar' = molarWeight,
                    'Persona responsable' = data.frame(Nombre = IDUsuario()[1],
                                                       Correo = IDUsuario()[2]),
                    'Fecha de preparacion' = fecha(),
                    'PropagateCompleto' = factorDilucion()))
      } else {
        return('Los datos ingresados no son validos!')
      }
    } else {
      dataFile <- readRDS(input$DisFile$datapath)
      if (names(dataFile)[1] != 'Muestra' || dataFile['Muestra'] != reagKey) {
        
        return(rbind('ERROR!!! ERROR!!! ERROR!!!', '',
                     'Asegurese de que está ingresando una disolución de una muestra',
                     'Por favor ingrese una disolucion de muestra de la especie apropiada'))
      } else {
        return(dataFile)
      }
      
    }})
  #paste0(signif(DisConc()$prop[1], 5), signif(DisConc()$prop[3], 3), collapse = ' \u00b1 '))})
  
  
  InfoDisBox <- eventReactive(input$buttonCalc, {
    trigger <- TRUE
    #printedStuff <- ifelse()
    box(title = div(style = 'font-size:14px', tags$b('Informacion de la disolución:')),
        width = 12, collapsible = TRUE, collapsed = FALSE,
        status = 'primary',#ifelse(trigger, 'success', 'danger'),
        renderPrint(tryCatch(infoDisSAMPLE(),
                             error = function(cond) {'Los datos ingresados no son validos!'})),
        if(input$SourceOption == "daCapo") {downloadButton(session$ns('DwnlDisFile'), 'Descargar archivo .dis')})
  })
  
  buttonCalc <- reactive(
    actionButton(session$ns('buttonCalc'), 
                 label = tags$b(ifelse(input$SourceOption == "daCapo", 'Crear disolución', 'Cargar disolucion'))))
  output$buttonCalc <- renderUI(buttonCalc())
  output$InfoDisBox <- renderUI(InfoDisBox())
  output$CalibCertDis <- renderUI(CalibCertDis())
  output$DwnlDisFile <- downloadHandler(
    filename = function() {paste0("Dis_MUESTRA_", reagKey, "_", input$IDMuestra, "_", paste0(fecha(), format(Sys.time(), '_%H-%M')), ".dis")}, 
    content = function(file) {saveRDS(infoDisSAMPLE(), file = file)}, contentType = NULL)
  
  # Messages
  NiceDensitAir <- eventReactive(input$buttonCalc,
                                 tags$div(style = 'font-size:11px',
                                          'La densidad local del aire calculada con la ecuacion CIMP2007 es ', 
                                          signif(DensitAir()[1], 7), ' \u00B1 ', signif(DensitAir()[2], 3), '[g cm^{-3}]'))
  deriMasaSAMPLE <- eventReactive(input$MasRecSAMPLE1, 
                               div(style = 'font-size:11px', 'La deriva en la medición de masa es ', signif(derMassSAMPLE() * 1000, 2), ' [mg]'))
  deriMasaDisSAMPLE <- eventReactive(input$MasRecDis1,
                                  div(style = 'font-size:11px', 'La deriva en la medición de masa es ', signif(derMassDis() * 1000, 2), ' [mg]'))
  
  output$NiceDensitAir <- renderUI(NiceDensitAir())
  output$deriMasaSAMPLE <- renderUI(deriMasaSAMPLE())
  output$deriMasaDisSAMPLE <- renderUI(deriMasaDisSAMPLE())
  
  return(list('infoDisSAMPLE' = infoDisSAMPLE))
}
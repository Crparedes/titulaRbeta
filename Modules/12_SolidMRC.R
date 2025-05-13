SolidMRCUI <- function(id, reagent, reagKey, explan, nu = FALSE) {
  ns <- NS(id)
  box(title = div(style = 'font-size:18px;valign="bottom"', tags$b('MRC de ', reagent)), 
      width = 12, status = 'primary', collapsible = TRUE, collapsed = FALSE,
      h5(explan),
      radioButtons(ns('SourceOption'), label = '¿Qué desea hacer?', 
                   choices = list('Crear disolución nueva utilizando un MRC' = 'daCapo',
                                  "Subir un archivo '.dis' generado anteriormente" = 'archivo'), 
                   selected = 'archivo'),
      tags$hr(),
      conditionalPanel(
        condition = 'input.SourceOption == "daCapo"', ns = ns,
        fluidRow(
          column(7, pickerInput(ns("MRCElected"), label = 'Seleccione el MRC: .',
                                choices = MRCs.ArchiveNames[[reagKey]], width = '100%',# inline = FALSE,
                                multiple = FALSE, selected = NULL)),
          column(5, tags$br(), uiOutput(ns("MRC_CertiFile")))),
        uiOutput(ns('InfoMrcBox')),
        box(title = div(style = 'font-size:14px', 'Preparación de la disolución'), 
            width = 12, collapsible = TRUE, collapsed = TRUE, status = 'primary',
            tags$b('Condiciones ambientales'),
            tags$div(id = "inline", style = 'font-size:12px',
                     splitLayout(cellWidths = c("75%", "25%"),
                                 numericInput(ns('Temp1'), label = 'Temperatura [°C]: .', value = 18),
                                 numericInput(ns('u_Temp1'), label = '\u00B1', value = 1.8)),
                     splitLayout(cellWidths = c("75%", "25%"),
                                 numericInput(ns('BarPres1'), label = 'Presion [hPa]: .', value = 750),
                                 numericInput(ns('u_BarPres1'), label = '\u00B1', value = 2)),
                     splitLayout(cellWidths = c("75%", "25%"),
                                 numericInput(ns('relHum1'), label = 'Humedad relativa [%]: .', value = 45),
                                 numericInput(ns('u_relHum1'), label = '\u00B1', value = 3))),
            uiOutput(ns("NiceDensitAir")), tags$hr(),
            tags$b('Masa del MRC'),
            tags$div(id = "inline", style = 'font-size:12px', 
                     pickerInput(ns("CalibCertMRC"), label = 'Balanza utilizada: .',
                                 choices = CalibCertShow, width = '100%', selected = 'MT XPE 205', multiple = FALSE)),
            tags$div(id = "inline", style = 'font-size:12px',
                     numericInput(ns('MasRec1'), label = 'Masa del recipiente [g]: .', value = 0),
                     numericInput(ns('MasMRC1'), label = 'Masa del MRC [g]: .', value = 0),
                     numericInput(ns('MasRecMRC1'), label = 'Masa conjunta [g]: .', value = 0),
                     uiOutput(ns('deriMasaMRC'))), tags$hr(),
            tags$b('Masa final de la disolución'),
            uiOutput(ns('CalibCertDis')),
            tags$div(id = "inline", style = 'font-size:12px',
                     numericInput(ns('MasRec2'), label = 'Masa del recipiente [g]: .', value = 0),
                     numericInput(ns('MasDis1'), label = 'Masa final disolución [g]: .', value = 0),
                     numericInput(ns('MasRecDis1'), label = 'Masa conjunta [g]: .', value = 0),
                     splitLayout(cellWidths = c("75%", "25%"),
                                 numericInput(ns('DensitDis'), label = 'Densidad disolución [g cm$^{-3}$]: .', 
                                              value = ifelse(reagKey == 'EDTA', 1.000, ifelse(reagKey == 'Pb', 1.007, 0))),
                                 numericInput(ns('u_DensitDis'), label = '\u00B1', 
                                              value = ifelse(reagKey == 'EDTA', 0.004, ifelse(reagKey == 'Pb', 0.006, 0)))),
                     uiOutput(ns('deriMasaDisMRC'))))),
      conditionalPanel(
        condition = 'input.SourceOption == "archivo"', ns = ns,
        fileInput(ns('DisFile'), label = 'Escoja el archivo', multiple = FALSE, accept = '.dis')),
      tags$hr(), 
      uiOutput(ns('buttonCalc')), uiOutput(ns('brwz')), tags$br(), #tags$br(),
      uiOutput(ns('InfoDisBox'))
  )
}

SolidMRCServer <- function(input, output, session, reagKey, IDUsuario, devMode, fecha) {
  output$brwz <- renderUI(
    if(devMode()) return(actionButton(session$ns('brwz'), label = tags$b('Pausar módulo'))))
  observeEvent(input$brwz, browser())
  
  fileDwnHTML <- reactive(a(href = paste0('CertMRC/', reagKey, '/', input$MRCElected, '.pdf'),
                            "Descargar certificado ", download = NA, target = "_blank"))
  dateMRC <- reactive(MRC.ExpiricyDates[[reagKey]][[input$MRCElected]])
  MassFrMRC <- reactive(MRCs.MassFraction[[reagKey]][[input$MRCElected]])
  MolWeiMRC <- reactive(MRC.At_MolWeigths[[reagKey]][[input$MRCElected]])
  DensitMRC <- reactive(MRC.densities[[reagKey]][[input$MRCElected]])
  
  DensitAir <- reactive(c(airDensity(Temp = input$Temp1, p = input$BarPres1, h = input$relHum1),
                          uncertAirDensity(Temp = input$Temp1, p = input$BarPres1, h = input$relHum1, 
                                           u_Temp = input$u_Temp1, u_p = input$u_BarPres1, u_h = input$u_relHum1, printRelSD = FALSE)))
  
  derMassMRC <- reactive(input$MasRecMRC1 - input$MasMRC1 - input$MasRec1)
  masMRC <- reactive(mean(input$MasMRC1, input$MasRecMRC1 - input$MasRec1))
  derMassDis <- reactive(input$MasRecDis1 - input$MasDis1 - input$MasRec2)
  masDis <- reactive(mean(input$MasDis1, input$MasRecDis1 - input$MasRec2))
  
  
  CalibCertDis <- reactive(tags$div(id = "inline", style = 'font-size:12px', 
                                    pickerInput(session$ns("CalibCertDis"), 
                                                label = 'Balanza utilizada: .',
                                                choices = CalibCertShow, selected = input$CalibCertMRC, width = '100%', multiple = FALSE)))
  
  convMassMRC <- reactive(c(convMass(calibCert = CalibCertList[[input$CalibCertMRC]], reading = masMRC(), units = 'g'),
                            uncertConvMass(calibCert = CalibCertList[[input$CalibCertMRC]], reading = masMRC(), units = 'g')))
  BuoyMRC <- reactive(c(MABC(rho = DensitMRC()[1], rho_air = DensitAir()[1]),
                        uncertMABC(rho = DensitMRC()[1], rho_air = DensitAir()[1], 
                                   u_rho = DensitMRC()[2], u_rho_air = DensitAir()[2], printRelSD = FALSE)))
  
  convMassDis <- reactive(c(convMass(calibCert = CalibCertList[[input$CalibCertDis]], reading = masDis(), units = 'g'),
                            uncertConvMass(calibCert = CalibCertList[[input$CalibCertDis]], reading = masDis(), units = 'g')))
  BuoyDis <- reactive(c(MABC(rho = input$DensitDis, rho_air = DensitAir()[1]),
                        uncertMABC(rho = input$DensitDis, rho_air = DensitAir()[1], 
                                   u_rho = input$u_DensitDis, u_rho_air = DensitAir()[2], printRelSD = FALSE)))
  
  DisConc <- reactive({
    #xx <- 
    propagate(expr = expression(convMassMRC * MassFrMRC * BuoyMRC / (MolWeiMRC * convMassDis * BuoyDis) * 1000000),
              data = cbind(convMassMRC = convMassMRC(), MassFrMRC = MassFrMRC(), BuoyMRC = BuoyMRC(), 
                           MolWeiMRC = MolWeiMRC(), convMassDis = convMassDis(), BuoyDis = BuoyDis()),
              do.sim = FALSE)
    #return(xx$prop[c(1, 3)])
  })
  
  
  infoDisMRC <- eventReactive(input$buttonCalc, {
    if (input$SourceOption == "daCapo") {
      if (!is.na(DisConc()$prop[[1]] > 0) && !is.na(DisConc()$prop[[3]] > 0)) {
        return(list('MRC empleado' = input$MRCElected,
                    'Fecha de vencimiento MRC' = dateMRC(),
                    'Especie ' = reagKey,
                    'Concentración [mmol/kg]' = signif(DisConc()$prop[[1]], 7),
                    'Incertidumbre [mmol/kg]' = signif(DisConc()$prop[[3]], 4),
                    'Persona responsable' = data.frame(Nombre = IDUsuario()[1],
                                                       Correo = IDUsuario()[2]),
                    'Fecha de preparación' = fecha(),
                    'PropagateCompleto' = DisConc()))
      } else {
        return('Los datos ingresados no son validos!')
      }
    } else {
      dataFile <- readRDS(input$DisFile$datapath)
      if (dataFile['Especie '] != reagKey) {
        return(rbind('ERROR!!! ERROR!!! ERROR!!!', 
                     'Por favor ingrese una disolución de la especie apropiada' ))
      } else {
        return(dataFile)
      }
      
    }})
  #paste0(signif(DisConc()$prop[1], 5), signif(DisConc()$prop[3], 3), collapse = ' \u00b1 '))})
  
  
  InfoMrcBox <- reactive({
    box(title = div(style = 'font-size:14px', 
                    ifelse(dateMRC() > fecha(), 'Resumen de información del MRC (vigente):', 'Resumen de información del MRC (VENCIDO):')), 
        width = 12, collapsible = TRUE, collapsed = TRUE,
        status = ifelse(dateMRC() > fecha(), 'success', 'danger'),
        div(style = 'font-size:12px',
            tags$b('Fecha de vencimiento:'), dateMRC(), tags$br(),
            tags$b('Fracción masica de ', reagKey, ':'), MassFrMRC()[1], '\u00B1', MassFrMRC()[2], tags$br(),
            tags$b('Masa molar de ', reagKey, ':'), MolWeiMRC()[1], '\u00B1', MolWeiMRC()[2], 'g mol', tags$sup('-1'),tags$br(),
            tags$b('Densidad estimada del MRC:'), DensitMRC()[1], '\u00B1', DensitMRC()[2], 'g cm', tags$sup('-3')))
  })
  
  InfoDisBox <- eventReactive(input$buttonCalc, {
    trigger <- TRUE
    #printedStuff <- ifelse()
    box(title = div(style = 'font-size:14px', 'Información de la disolución:'),
        width = 12, collapsible = TRUE, collapsed = FALSE,
        status = 'primary',#ifelse(trigger, 'success', 'danger'),
        renderPrint(tryCatch(infoDisMRC(),
                             error = function(cond) {'Los datos ingresados no son validos!'})),
        if(input$SourceOption == "daCapo") {downloadButton(session$ns('DwnlDisFile'), 'Descargar archivo .dis')})
  })
  
  buttonCalc <- reactive(actionButton(session$ns('buttonCalc'), 
                                      label = tags$b(ifelse(input$SourceOption == "daCapo", 
                                                            'Calcular concentración', 
                                                            'Cargar disolución'))))
  output$buttonCalc <- renderUI(buttonCalc())
  output$MRC_CertiFile <- renderUI(fileDwnHTML())
  output$InfoMrcBox <- renderUI(InfoMrcBox())
  output$InfoDisBox <- renderUI(InfoDisBox())
  output$CalibCertDis <- renderUI(CalibCertDis())
  output$DwnlDisFile <- downloadHandler(
    filename = function() {paste0("Disolucion_MRC_", reagKey, "_", paste0(fecha(), format(Sys.time(), '_%H-%M')), ".dis")}, 
    content = function(file) {saveRDS(infoDisMRC(), file = file)}, contentType = NULL)
  
  # Messages
  NiceDensitAir <- reactive(tags$div(style = 'font-size:11px',
                                     'Densidad local del aire (CIMP2007): ', 
                                     signif(DensitAir()[1], 7), ' \u00B1 ', signif(DensitAir()[2], 3), '[g cm', tags$sup('-3'), ']'))
  deriMasaMRC <- eventReactive(input$MasRecMRC1, 
                               div(style = 'font-size:11px', 'La deriva en la medición de masa es ', signif(derMassMRC() * 1000, 2), ' [mg]'))
  deriMasaDisMRC <- eventReactive(input$MasRecDis1,
                                  div(style = 'font-size:11px', 'La deriva en la medición de masa es ', signif(derMassDis() * 1000, 2), ' [mg]'))
  
  output$NiceDensitAir <- renderUI(NiceDensitAir())
  output$deriMasaMRC <- renderUI(deriMasaMRC())
  output$deriMasaDisMRC <- renderUI(deriMasaDisMRC())
  
  return(list('infoDisMRC' = infoDisMRC))
}
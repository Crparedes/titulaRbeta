rm(list=ls())
gc()            # Garbage collector

# titulaR


library(shiny)
library(car)
library(shinydashboard)
library(dashboardthemes) #https://cran.r-project.org/web/packages/dashboardthemes/vignettes/using_dashboardthemes.html
library(shinyWidgets)
library(shinycssloaders)
library(shinyjs) #to use hidden
library(ggplot2) #Grammar of graphics
library(ggfortify)
library(rhandsontable)
library(data.table)
library(stringr)
library(masscor)
library(propagate)
library(dplyr) # 
library(outliers)
library(writexl)
library(deming)
# icon("flask")

# Por lo general, los módulos_UI son llamados desde las funciones de`` layouts
pack_titRation  <- with(list(pt = 'RPackage/'), paste0(pt, list.files(path = pt)))
modules         <- with(list(pt = 'Modules/'), paste0(pt, list.files(path = pt))) # El primer m'odulo es el de variables globales  
layouts         <- with(list(pt = 'Layouts/'), paste0(pt, list.files(path = pt))) # functions in the client side
sapply(c(pack_titRation, modules, layouts), source)

ui <- function(request) {
  withMathJax()
  useShinyjs()
  dashboardPage(header = customHeader, sidebar = customSidebar, body = customBody,
                title = "titulaR - Instituto Nacional de Metrología de Colombia") #customStuff in ./Layouts
}

server <- function(input, output, session, devMode = TRUE) {
  devMode <- reactive(input$Desarrollador)
  showNotification("Se recomienda el uso de una pantalla de resolución 1900 x 1080 pixeles.", type = 'warning')
  fecha <- reactive(input$Fecha)
  output$brwz <- renderUI(
    if(devMode()) return(actionButton(inputId = 'brwz', label = tags$b('Pausar titulaR'), width = '90%')))
  observeEvent(input$brwz, browser())
  
  ## Inicializaci'on
  IDUsuario  <- reactive(c(input$nombre, input$correo))
  
  
  observeEvent(input$Start1, updateTabItems(inputId = 'tabs', selected = 'MRC_DisTab'))
  callModule(module = BalanceCalibCertServer, id = 'BalanceCalibCert')
  
  ## Disoluciones de MRCs o de candidatos a MRC
  DisEDTA_MRC <- callModule(module = SolidMRCServer, id = 'ModuloDisolucionEDTA', reagKey = 'EDTA', IDUsuario = IDUsuario, devMode = devMode, fecha = fecha)
  DisPb_MRC   <- callModule(module = SolidMRCServer, id = 'ModuloDisolucionPbNO3.2', reagKey = 'Pb', IDUsuario = IDUsuario, devMode = devMode, fecha = fecha)
  DisMuestraEDTA <- callModule(module = SolidSampleServer, id = 'ModuloDisolucionMuestraEDTA', reagKey = 'EDTA', IDUsuario = IDUsuario, 
                               DensitSAMPLE = c(0.860, 0.005), molarWeight = c(3.722368e+02, 6.332914e-03), devMode = devMode, fecha = fecha)
  callModule(module = LiquidMRCServer, id = 'ModuloDilucionCobre', reagKey = 'Cu', IDUsuario = IDUsuario)
  callModule(module = LiquidMRCServer, id = 'ModuloDilucionZinc', reagKey = 'Zn', IDUsuario = IDUsuario)
  
  
  ## Titulaciones disoluciones calibrantes monoelementales
  Logical_DisEDTA_MRC <- reactive(!("try-error" %in% class(try(DisEDTA_MRC$infoDisMRC()$`MRC empleado`))))
  output$PrintDisEDTA <- renderUI(
    box(width = 12, title = tags$b('Disolucion titulante de EDTA'), status = ifelse(Logical_DisEDTA_MRC(), 'primary', 'danger'), 
        solidHeader = !Logical_DisEDTA_MRC(),
        renderPrint(
          tryCatch(rbind(
            paste0('MRC: ', DisEDTA_MRC$infoDisMRC()$`MRC empleado`, ': '),
            paste0(' Concentración: ', signif(DisEDTA_MRC$infoDisMRC()$`Concentración [mmol/kg]`, 6), ' \u00B1 ',
                   signif(DisEDTA_MRC$infoDisMRC()$`Incertidumbre [mmol/kg]`, 2), ' [mmol/kg]'),
            paste0(' Preparación: ', DisEDTA_MRC$infoDisMRC()$`Fecha de preparación`, '.')),
            error = function(cond) {rbind('Sin informacion de la disolucion titulante.', 'Dirijase al modulo de MRCs y disoluciones.')}))))
  
  output$MonoElemInitTit <- renderUI(ifelse(Logical_DisEDTA_MRC(), 
                                            return(actionButton('MonoElemInitTit', label = tags$b('Iniciar una titulación'))), 
                                            return(actionButton('FAIL', label = ''))))
  
  #CalibMonoDelDia <- reactiveValues()
  observeEvent(input$MonoElemInitTit, {
    req(input$MonoElemInitTit > 0)
    Elemento <- input$Elemento
    LeadAM <- input$LeadAM
    u_LeadAM <- input$u_LeadAM
    sampleID <- input$sampleID
    dscrMuestraMonoelemTit <- input$dscrMuestraMonoelemTit 
    BalanzaMonoelemTit <- input$BalanzaMonoelemTit
    MonoElemNumber <- input$MonoElemInitTit
    # browser()
    #NameFile <- paste0(Elemento(), "_", sampleID(), ".", as.character(isolate(MonoElemNumber())), "_", format(Sys.time(), '%Y-%m-%d_%H-%M'), ".tit")
    #CalibMonoDelDia[[NameFile]] <- 
      isolate(callModule(module = CalibraMonoIndividualServer, id = isolate(paste0('monoElemTit', input$MonoElemInitTit)),
                 Elemento = Elemento, LeadAM = LeadAM, u_LeadAM = u_LeadAM,
                 sampleID = sampleID, dscrMuestraMonoelemTit = dscrMuestraMonoelemTit, 
                 BalanzaMonoelemTit = BalanzaMonoelemTit,
                 DisEDTA_MRC = DisEDTA_MRC, IDUsuario = IDUsuario, number = MonoElemNumber, devMode = devMode, fecha = fecha))
    prependTab(inputId = 'monoElemTabBox', CalibraMonoIndividualUI(id = paste0('monoElemTit', input$MonoElemInitTit)), select = TRUE)
    
  })
  callModule(module = CombinaServer, id = 'CalibraMonoComb1', IDUsuario = IDUsuario, especie = 'Elem', tol = 0.0005, devMode = devMode, fecha = fecha) 
  
  
  
  ## Titulación de EDTA sólido
  Logical_DisPb_MRC <- reactive(!("try-error" %in% class(try(DisPb_MRC$infoDisMRC()$`MRC empleado`))))
  Logical_DisEDTA_Muestra <- reactive(!("try-error" %in% class(try(DisMuestraEDTA$infoDisSAMPLE()$`Muestra`))))
  
  output$PrintDisPb <- renderUI(
    box(width = 12, title = tags$b('Disolucion titulante de ion plomo'), status = ifelse(Logical_DisPb_MRC(), 'primary', 'danger'), 
        solidHeader = !Logical_DisPb_MRC(),
        renderPrint(
          tryCatch(rbind(
            paste0('MRC: ', DisPb_MRC$infoDisMRC()$`MRC empleado`, ': '),
            paste0(' Concentración: ', signif(DisPb_MRC$infoDisMRC()$`Concentración [mmol/kg]`, 6), ' \u00B1 ',
                   signif(DisPb_MRC$infoDisMRC()$`Incertidumbre [mmol/kg]`, 2), ' [mmol/kg]'),
            paste0(' Preparación: ', DisPb_MRC$infoDisMRC()$`Fecha de preparación`, '.')),
            error = function(cond) {rbind('Sin informacion de la disolucion titulante.', 'Dirijase al modulo de MRCs y disoluciones.')}))))
  
  output$PrintDisEDTA.Sample <- renderUI(
    box(width = 12, title = tags$b('Disolucion de EDTA a titular'),status = ifelse(Logical_DisEDTA_Muestra(), 'primary', 'danger'), 
        solidHeader = !Logical_DisEDTA_Muestra(),
        renderPrint(
          tryCatch(rbind(
            paste0('Muestra: ', DisMuestraEDTA$infoDisSAMPLE()$`ID_Disolucion`, ': '),
            paste0(' Concentración aproximada: ', signif(DisMuestraEDTA$infoDisSAMPLE()$`Conc.Aprox [mmol/kg]`, 4), ' [mmol/kg]'),
            paste0(' Factor de dilucion muestra original: 1:', round(DisMuestraEDTA$infoDisSAMPLE()$`Factor de dilucion 1:`, 2)),
            paste0(' Preparación: ', DisMuestraEDTA$infoDisSAMPLE()$`Fecha de preparacion`, '.')),
            error = function(cond) {rbind('Sin informacion de la disolucion a titular.', 'Dirijase al modulo de MRCs y disoluciones.')}))))
  
  output$EDTA.InitTit <- renderUI({
    if (Logical_DisPb_MRC() && Logical_DisEDTA_Muestra()) return(actionButton('EDTA.InitTit', label = tags$b('Iniciar una titulación')))
  })
  
  observeEvent(input$EDTA.InitTit, {
    #browser()
    req(input$EDTA.InitTit > 0)
    BalanzaTitEDTA <- input$BalanzaTitEDTA
    EDTA.Number <- input$EDTA.InitTit
    # browser()
    #NameFile <- paste0(Elemento(), "_", sampleID(), ".", as.character(isolate(MonoElemNumber())), "_", format(Sys.time(), '%Y-%m-%d_%H-%M'), ".tit")
    #CalibMonoDelDia[[NameFile]] <- 
    isolate(callModule(module = EDTA.IndividualServer, id = isolate(paste0('EDTA.', input$EDTA.InitTit)),
                       BalanzaTitEDTA = BalanzaTitEDTA, DisPb_MRC = DisPb_MRC, DisMuestraEDTA = DisMuestraEDTA,
                       IDUsuario = IDUsuario, number = EDTA.Number, devMode = devMode, fecha = fecha))
    prependTab(inputId = 'EDTA.TabBox', EDTA.IndividualUI(id = paste0('EDTA.', input$EDTA.InitTit)), select = TRUE)
  })
  callModule(module = CombinaServer, id = 'EDTAComb1', IDUsuario = IDUsuario, especie = 'EDTA', tol = 0.001, devMode = devMode, fecha = fecha) 
  
  #callModule(module = EDTACombServer, id = 'EDTAComb1', IDUsuario = IDUsuario, brwzMDL = brwzMDL) 
  
  callModule(module = GenericCurveServer, id = 'Generic', devMode = devMode)
  
}

#runApp(list(ui=ui, server=server))#, host="0.0.0.0", port=1234)
shinyApp(ui = ui, server = server, enableBookmarking = "url")

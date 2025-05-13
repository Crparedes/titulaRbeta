convertMenuItem <- function(mi, tabName) {
  mi$children[[1]]$attribs['data-toggle'] = "tab"
  mi$children[[1]]$attribs['data-value']  = tabName
  mi
}
convertMenuItem2 <- function(mi, tabName) {
  mi$children[[1]]$attribs['data-toggle'] = "tab"
  mi$children[[1]]$attribs['data-value'] = tabName
  if(length(mi$attribs$class) > 0 && mi$attribs$class == "treeview") mi$attribs$class = NULL
  mi
}

customSidebar <- dashboardSidebar(tags$style(".left-side, .main-sidebar {padding-top: 110px;}"), # font-size: larger
                     width = 300, withMathJax(),
                     sidebarMenu(id = "tabs", # tags$br(), tags$br(),
                       menuItem("Inicio y configuraciones", tabName = "inicio", icon = icon("info-circle")), #tags$br(),  tags$br(),
                       menuItem("MRCs y disoluciones", tabName = "MRC_DisTab", icon = icon("fill-drip")), tags$br(), tags$br(),
                       tags$b(HTML('&ensp;'), 'Complejometrías'),
                       menuItem("Disoluciones calibrantes monoelementales", tabName = "CalibrantesTab", icon = icon("bong")),
                       menuItem("EDTA, sal disódica dihidratada", tabName = "EdtaTab", icon = icon("stroopwafel")), tags$br(), tags$br(),
                       tags$b(HTML('&ensp;'), 'Ácido-Base'),
                       menuItem("(Ninguna implementada)", tabName = "Acido1", icon = icon("bong")),
                       tags$hr(),
                       menuItem("Curva de titulación genérica", tabName = "Generica", icon = icon("bong")),
                       tags$hr(),
                       HTML('<h6 style="color: #dddddd; font-size:9px;">
                            &ensp;&ensp;&ensp;Aplicativo desarrollado por <b>Cristhian Paredes</b></h6>'),
                       tags$br(), tags$br(), tags$br(), tags$br(),
                       tags$br(), tags$br(), tags$br(), tags$br(),
                       tags$br(), tags$br(), tags$br(), tags$br(),
                       #tags$br(), tags$br(), tags$br(), tags$br(),
                       materialSwitch('Desarrollador', 'Modo de desarrollador', status = 'primary'),
                       uiOutput('brwz')##,
                       #actionButton(inputId = 'brwzMDL', label = tags$b('Detener modulo (No oprimir!)'), width = '90%')
                       #convertMenuItem(menuItem("Configuración general", tabName = "config", icon = icon("cog"),
                        #                        tags$b('Archivos descargables:'),
                        #                        radioButtons("Format", label = "Formato",
                        #                                     choices = list("PDF (gráfico de vectores)" = ".pdf", 
                        #                                                    "PNG (mapa de bits 300 ppi)" = ".png")),
                        #                        sliderInput("plotsH", label = "Altura (mm)", min = 40, max = 300, value = 60), 
                        #                        sliderInput("plotsW", label = "Anchura(mm)", min = 40, max = 300, value = 80)),
                        #                        tabName = "config"), tags$br()
                       ))
customBody <- dashboardBody(
  tags$script(HTML("$('body').addClass('fixed');")),
  tags$head(tags$script('
                        var width = 1;
                        $(document).on("shiny:connected", function(e) {
                          width = window.innerWidth/1900;
                          document.body.style.zoom = width;
                        });
                        ')),
  tags$head(
    tags$style(HTML('.wrapper {height: auto !important; position:relative; overflow-x:hidden; overflow-y:hidden}
                     .shiny-notification {position:fixed; top: calc(50% - 150px); left: calc(50% - 150px); 
                                         height: auto !important; opacity:0.98; margin-right:500px}
                     .btn-box-tool {color: #001848; font-size: 15px}')),
    tags$style(type = "text/css", "#inline label{ display: table-cell; text-align: right; vertical-align: middle; } 
               #inline .form-group {display: table-row;}"),
    tags$style(type = "text/css", "#inlineTOP label{ display: table-cell; text-align: right; vertical-align: top; } 
               #inlineTOP .form-group {display: table-row;}"),
    tags$style(type = "text/css", "#inlineBOT label{ display: table-cell; text-align: right; vertical-align: bottom; } 
               #inlineBOT .form-group {display: table-row;}")
  ), 
  #shinyDashboardThemes(theme = "blue_gradient"),
  customTheme,... = # Look for it in Layouts/aa_ ...
  tags$style(tags$style(HTML('
        /* ligth-blue */
        .bg-light-blue {background-color: #34B1C9!important;}
    ')) #This is not working
    #type = 'text/css', 
    #'.bg-light-blue {background-color: #34B1C9!important; }'
  ),
  withMathJax(),
  tabItems(
    tabItem(tabName = "inicio", tags$hr(), tags$hr(),inicioLy),
    tabItem(tabName = "MRC_DisTab", tags$hr(), tags$hr(), MrcDisLy),
    tabItem(tabName = "CalibrantesTab", tags$hr(), tags$hr(), CalibraMonoLy),
    tabItem(tabName = "EdtaTab", tags$hr(), tags$hr(), EdtaLy),
    tabItem(tabName = "Generica", tags$hr(), tags$hr(), GenericCurveUI(id = 'Generic'))
    #tabItem(tabName = "bibliogr", h2("Bibliografía"))
  )
)

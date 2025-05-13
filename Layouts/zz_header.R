customHeader <- dashboardHeader(tags$li(class = "dropdown",
                            tags$style(".main-header {max-height: 100px}"),
                            tags$style(".main-header .logo {height: 100px}")),
                    titleWidth = 1850, 
                    title = tags$div(HTML('<table text-align = left cellspacing = -20 cellPadding=0>
    <tr><th rowspan = 2><a id = "logo" href = "http://www.inm.gov.co"
    title = "titulaR - Instituto Nacional de Metrología" data-height="80">
    </a>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
    <img src = "INMgrayAncho.png" height = "90" alt = "INM de Colombia" style = "margin-top: 5px">
    &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;</th>
    <th  height = "80"><h1 style="LINE-HEIGHT:5px; color: #dddddd; margin-bottom: -5px; font-size:45px;">
          <b>Aplicativo titulaR</b>
          <b style="LINE-HEIGHT:5px; color: #dddddd; font-size:14px;">v.0.7.1</b>
    </h1></th></tr>
    <tr><th><h3 style="LINE-HEIGHT:0px; color: #dddddd; margin-top: 1px;">
    Soporte para las titulaciones gravimétricas de la <b>SMQB - INM</b>
    </h3></th></tr>
    </table>')))

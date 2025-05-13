# INSPIRED IN poor_mans_flatly from https://github.com/nik01010/dashboardthemes/blob/master/R/Theme_PoorMansFlatly.R

# Theme: Poor Man's Flatly ------------------------------------------------------------------------------
customTheme <- shinyDashboardThemeDIY(
  ### general
  appFontFamily = "Arial"
  ,appFontColor = "rgb(0,0,0)"
  ,primaryFontColor = "rgb(0,0,0)"
  ,infoFontColor = "rgb(0,0,0)"
  ,successFontColor = "rgb(0,0,0)"
  ,warningFontColor = "rgb(0,0,0)"
  ,dangerFontColor = "rgb(0,0,0)"
  ,bodyBackColor = "rgb(221,221,221)"
  
  ### header
  ,logoBackColor = "rgb(0, 0, 0)"
  
  ,headerButtonBackColor = "rgb(221,221,221)"
  ,headerButtonIconColor = "rgb(75,75,75)"
  ,headerButtonBackColorHover = "rgb(21,21,21)"
  ,headerButtonIconColorHover = "rgb(0,0,0)"
  
  ,headerBackColor = "rgb(23,28,28)"
  ,headerBoxShadowColor = "#000000"
  ,headerBoxShadowSize = "3px 3px 3px"
  
  ### sidebar
  ,sidebarBackColor = "rgb(50, 50, 50)"
  ,sidebarPadding = 0
  
  ,sidebarMenuBackColor = "transparent"
  ,sidebarMenuPadding = 0
  ,sidebarMenuBorderRadius = 0
  
  ,sidebarShadowRadius = "3px 5px 5px"
  ,sidebarShadowColor = "#aaaaaa"
  
  ,sidebarUserTextColor = "rgb(221,221,2@1)"
  
  ,sidebarSearchBackColor = "rgb(55,72,80)"
  ,sidebarSearchIconColor = "rgb(153,153,153)"
  ,sidebarSearchBorderColor = "rgb(55,72,80)"
  
  ,sidebarTabTextColor = "rgb(221,221,221)"
  ,sidebarTabTextSize = 12
  ,sidebarTabBorderStyle = "none none solid none"
  ,sidebarTabBorderColor = "rgb(90,90,90)"
  ,sidebarTabBorderWidth = 1
  
  ,sidebarTabBackColorSelected = cssGradientThreeColors(
    direction = "right"
    ,colorStart = "rgb(221, 221, 221)"
    ,colorMiddle = "rgb(200, 200, 200)"
    ,colorEnd = "rgb(100,100,100)"
    ,colorStartPos = 0
    ,colorMiddlePos = 38
    ,colorEndPos = 80
  )
  ,sidebarTabTextColorSelected = "rgb(0,0,0)"
  ,sidebarTabRadiusSelected = "0px 20px 20px 0px"
  
  ,sidebarTabBackColorHover = cssGradientThreeColors(
    direction = "right"
    ,colorStart = "rgb(248, 248, 248)"
    ,colorMiddle = "rgb(220, 220, 220)"
    ,colorEnd = "rgb(150,150,150)"
    ,colorStartPos = 0
    ,colorMiddlePos = 40
    ,colorEndPos = 70
  )
  ,sidebarTabTextColorHover = "rgb(50,50,50)"
  ,sidebarTabBorderStyleHover = "none none solid none"
  ,sidebarTabBorderColorHover = "rgb(75,126,151)"
  ,sidebarTabBorderWidthHover = 1
  ,sidebarTabRadiusHover = "0px 20px 20px 0px"
  
  ### boxes
  ,boxBackColor = "rgb(245,245,245)"
  ,boxBorderRadius = 5
  ,boxShadowSize = "0px 1px 1px"
  ,boxShadowColor = "rgba(0,0,0,.4)"
  ,boxTitleSize = 16
  ,boxDefaultColor = "rgb(210,214,220)"
  ,boxPrimaryColor = "rgb(1, 1, 1)"
  ,boxInfoColor = "rgb(210,214,220)"
  ,boxSuccessColor = "rgba(0,166,90,1)"
  ,boxWarningColor = "rgb(52, 177, 201)"
  ,boxDangerColor = "rgb(221,75,57)"
  
  ,tabBoxTabColor = "rgb(248,248,248)"
  ,tabBoxTabTextSize = 14
  ,tabBoxTabTextColor = "rgb(0,0,0)"
  ,tabBoxTabTextColorSelected = "rgb(0,0,0)"
  ,tabBoxBackColor = "rgb(248,248,248)"
  ,tabBoxHighlightColor = "rgb(44,62,80)"
  ,tabBoxBorderRadius = 5
  
  ### inputs
  ,buttonBackColor = "rgb(1, 1, 1)"
  ,buttonTextColor = "rgb(221 ,221, 221)"
  ,buttonBorderColor = "rgb(200,200,200)"
  ,buttonBorderRadius = 5
  
  ,buttonBackColorHover = "rgb(25,25,25)"
  ,buttonTextColorHover = "rgb(200,200,200)"
  ,buttonBorderColorHover = "rgb(200,200,200)"
  
  ,textboxBackColor = "rgb(255,255,255)"
  ,textboxBorderColor = "rgb(200,200,200)"
  ,textboxBorderRadius = 5
  ,textboxBackColorSelect = "rgb(245,245,245)"
  ,textboxBorderColorSelect = "rgb(200,200,200)"
  
  ### tables
  ,tableBackColor = "rgb(255,255,255)"
  ,tableBorderColor = "rgb(240,240,240)"
  ,tableBorderTopSize = 1
  ,tableBorderRowSize = 1
)
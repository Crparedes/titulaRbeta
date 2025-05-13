MrcDisLy <- fluidRow(
    column(12, 
      box(title = div(style = 'font-size:23px', tags$b('MRCs y disoluciones para complejometrías')), 
          width = 12, status = 'primary', collapsible = TRUE, collapsed = FALSE,
          
      fluidRow(
        column(4,
               SolidMRCUI('ModuloDisolucionEDTA', reagent = 'EDTA - sal disodica dihidratada', reagKey = 'EDTA',
                 explan = 'Para asignar valor de fraccion masica de iones metalicos en disoluciones calibrantes monoelementales 
                 y para realizar aseguramiento de la calidad de los resultados de fraccion masica de EDTA en la sal disodica dihidratada'
                 )),
        column(4,
               SolidMRCUI('ModuloDisolucionPbNO3.2', reagent = 'nitrato de plomo', reagKey = 'Pb',
                 explan = 'Para asignar valor de fraccion masica de EDTA en la sal disodica dihidratada y
                 para realizar aseguramiento de la calidad de los resultados de fraccion masica de iones metalicos en disoluciones 
                 calibrantes monoelementales'
                 )),
        column(4,
               SolidSampleUI('ModuloDisolucionMuestraEDTA', reagent = 'EDTA - sal disodica dihidratada', reagKey = 'EDTA',
                          explan = 'Para asignar valor de fraccion masica de EDTA en la sal disodica dihidratada'
               ))),
      tags$hr(),
      fluidRow(
        column(4,
               LiquidMRCUI('ModuloDilucionCobre', reagent = 'disolucion calibrante de cobre', reagKey = 'Cu',
                           explan = 'Para usar como indicadora en las titulaciones complejometricas por retroceso')),
        column(4,
               LiquidMRCUI('ModuloDilucionZinc', reagent = 'disolucion calibrante de zinc', reagKey = 'Zn',
                           explan = 'Para usar como disolucion titulante en las titulaciones complejometricas por retroceso')
        ))
      ),
      box(title = NULL, width = 12, h6('Para solicitar la inclusión de más MRCs por favor mande un correo a caparedes@inm.gov.co.'))
    )
)

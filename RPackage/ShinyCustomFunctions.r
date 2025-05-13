dwldhndlr <- function(name, formatP, dimensP, plt) {
  return(downloadHandler(filename = function(){paste0(name, formatP())},
                         content = function(file){
                           if (formatP() == '.pdf') {
                             pdf(file, width = dimensP()[1], height = dimensP()[2])
                           } else {
                             png(file, width = dimensP()[1], height = dimensP()[2], units = 'in', res = 300)
                           }
                           print(plt())
                           dev.off()
                         }
  ))
}



#install.packages("shiny")
#install.packages("ggplot2")
#install.packages("ggthemes")
#install.packages("shinyExt")
#install.packages('devtools')
library(shiny)
library(ggplot2)
library(ggthemes)
#library(devtools)
#library(shinyExt)

projectDir = getwd()

codeDir = file.path(projectDir, 'R')
#Inventorybased <- read.csv("knitr/R/Inventorybased.csv")
Inventorybased <- read.csv(file.path(codeDir,'Inventorybased.csv'))

shinyServer(function(input, output, session) {
  
  output$piePlot <- renderPlot({
    
    RelativeFrequency_of_Inventorycheck<-Inventorybased$Inv_frequency_Percentage
    Inventory<-Inventorybased$Inventory
    graphdatapie <- data.frame(RelativeFrequency_of_Inventorycheck)
    
    #View(graphdatapie)
    #library(ggplot2)
    #Indicators <-rownames(graphdatapie)
    #Ref url -> https://groups.google.com/forum/#!topic/shiny-discuss/4FlaRSuk3js
    #c <- ggplot(graphdatapie, aes(x=Indicators,y=RelativeFrequency_of_Inventorycheck,fill = RelativeFrequency_of_Inventorycheck),environment=environment()) + geom_bar(width = 1,stat="identity")
    
    c <- ggplot(graphdatapie, aes(x=Inventory,y=RelativeFrequency_of_Inventorycheck,fill = RelativeFrequency_of_Inventorycheck),environment=environment()) + geom_bar(width = 1,stat="identity")
    print(c + coord_polar(theta = "y"))
    
  })
  
  output$histPlot <- renderPlot({
    
    RelativeFrequency_of_Inventorycheck<-Inventorybased$Inv_frequency_Percentage*100
    Inventory<-Inventorybased$Inventory
    
    graphdatahist <- as.data.frame(RelativeFrequency_of_Inventorycheck)
    graphdatahist
    c <- ggplot(graphdatahist, aes(x=Inventory,y=RelativeFrequency_of_Inventorycheck,fill =RelativeFrequency_of_Inventorycheck),environment=environment())+coord_flip()
    
    print(c + geom_bar(width = 1,stat="identity")+ xlab("Inventory")+ylab("RelativeFrequency_of_Inventorycheck")+theme_bw())
    
  })
}
)
#find_rtools()
#devtools::install_github('rstudio/shinyapps')
#shinyapps::setAccountInfo(name='bulls-eye',
#                          token='FAFF3408B4BB97AE395F2D95C7F0E0D0',
#                          secret='GjO8M60qrblYKZQpJGKhAGzNcL+XNuqswJFQ48yR')
#library(shinyapps)
#shinyapps::deployApp('C:/Users/Administrator/Documents/R')

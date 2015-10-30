#install.packages("shiny")
#install.packages("sqldf")
#install.packages("plyr")
#install.packages("tcltk2")
#library(shiny)
library(sqldf)
library(plyr)
library(tcltk2)

#data_po <- read.csv("C:/PROJECT/data_po/data_po.csv")
projectDir = getwd()

codeDir <- file.path(projectDir, 'R')

data_po <- read.csv(file.path(codeDir,'data_po.csv'))

View(data_po)

data_po$Timediff <- as.numeric(difftime(data_po$Complete.Timestamp,data_po$Start.Timestamp,units="hours"))

Rolebased <- sqldf("select Role,sum(Timediff) as Duration,count(Role) as Frequency from data_po group by Role")

num <- sqldf("select sum(Frequency) from Rolebased")
num <- sum(Rolebased$Frequency)
freq <- Rolebased$Frequency


Rolebased$Frequency_Percentage <- freq/num

View(Rolebased)

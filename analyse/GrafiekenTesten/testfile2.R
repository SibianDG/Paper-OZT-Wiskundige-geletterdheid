library(ggplot2)
library(tidyverse)
library(plyr)
library(dplyr)
library(data.table)
#data <- read.csv("C:\\Users\\sibia\\OneDrive - Hogeschool Gent\\2e bach\\Onderzoekstechnieken\\NPE\\ozt-npe-2021-ma-di-09\\data\\npe-1920-wiskundige-geletterdheid.csv", sep = ",")
data <- read.csv("data/npe-1920-wiskundige-geletterdheid.csv", sep = ",")

data


nrow(data)

if (nrow(data) >= 30){
  print("Grote steekproef")
} else {
  print("Kleine steekproef")
}

onafhankelijke_Variabelen <- c("NativeLanguage - kwalitatief-Nominaal", "OtherLanguages - kwalitatief-Nominaal", "BirthCountry - kwalitatief-Nominaal")
afhankelijke_Variabelen <- c("TestQ01-TestQ05 - kwantitatief-ratio", "TestQ06-TestQ10 - kwantitatief-ratio", "TestQ11-TestQ15 - kwantitatief-ratio", "GradeMath4IT - kwantitatief-ratio")

nederlands <- data[data$NativeLanguage == "Nederlands", ]
andersTaligen <- data[data$NativeLanguage != "Nederlands",]




nrow(nederlands)

sprintf("Aantal Nederlandstaligen: %s",nrow(nederlands))
sprintf("Aantal niet Nederlandstaligen: %s",nrow(andersTaligen))

#NA eruit filteren
data2 <- data[, c("NativeLanguage", "GradeMath4IT")]
data2 <- data2 %>% filter_all(all_vars(!is.na(.)))
#data2 %>% filter_all(all_vars(complete.cases(.)))

#data2[data2$NativeLanguage != "Nederlands"] <- "Overige"

data2$NativeLanguage = replace(x = data2$NativeLanguage, 
                               list =  !data2$NativeLanguage %in% c('Nederlands'), 
                               values =  'Other')

mu <- ddply(data2, "NativeLanguage", summarise, grp.mean=mean(GradeMath4IT))

ggplot(data2, aes(x=GradeMath4IT, y=NativeLanguage)) + 
  geom_boxplot()
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

data2

# https://www.sthda.com/english/wiki/ggplot2-histogram-plot-quick-start-guide-r-software-and-data-visualization
#ggplot(data2, aes(x=GradeMath4IT)) + geom_histogram()
#ggplot(data2, aes(x=GradeMath4IT, color=NativeLanguage)) +
#  geom_histogram(fill="white", alpha=0.5, position="identity")

ggplot(data2, aes(x=GradeMath4IT, color=NativeLanguage, fill=NativeLanguage)) +
  geom_histogram(aes(y=..density..), position="identity", alpha=0.3)+
  geom_density(alpha=0.4)+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=NativeLanguage),
             linetype="dashed")+
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  labs(title="Grades",x="Grades for Math4IT", y = "Density")+
  theme_classic()

points <- c(0:20)

t <- table(data2)
t
t2 <- as.data.frame.matrix(t) 
t2
t2[0]

t3 <- as_data_frame(table(data2))
t3

t3 <- sum(t3[t3$n])


ggplot(data = t3, mapping = aes(x = reorder(NativeLanguage, as.numeric(n), FUN = median), color = NativeLanguage, y = as.numeric(GradeMath4IT))) +
  geom_boxplot() +
  geom_jitter(width = 0.35) +
  coord_flip()

ggplot(data = data2, mapping = aes(x = reorder(NativeLanguage, GradeMath4IT, FUN = median), color = NativeLanguage, y = GradeMath4IT)) +
  geom_boxplot() +
  geom_jitter(width = 0.35) +
  coord_flip() +
  xlab ("Native Language") +
  ylab ("Grade Math4IT") +
  ggtitle("Boxplot for Grade Math4IT ~ Native Language")
  

#ggplot(data = t3, mapping = aes(x = as.numeric(GradeMath4IT), y = as.numeric(n), color = NativeLanguage)) +
#  geom_point() +
#  geom_smooth(method = "lm", formula = y ~ x)


#ggplot(data = data2, mapping = aes(x = points, y = GradeMath4IT, color = NativeLanguage)) +
#  geom_point() +
#  geom_smooth(method = "lm", formula = y ~ x)





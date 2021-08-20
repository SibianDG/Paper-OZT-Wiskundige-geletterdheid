t <- function(alf, alt, x, y){
  tRes <- t.test(x, y, alternative = alt, mu = 0, conf.level = 1-alf)
  p <- tRes$p.value
  if (p < alfa){
    sprintf("p (%s) is kleiner dan alfa (%s), dus we kunnen de H0 WEL verwerpen.", p, alfa)
  } else {
    sprintf("p (%s) is groter dan alfa (%s), dus we kunnen de H0 NIET verwerpen.", p, alfa)
  }
}

data <- read.csv("data/npe-1920-wiskundige-geletterdheid.csv", sep = ",")

neededData <- data[, c("NativeLanguage", "BirthCountry" ,"OtherLanguages", "GradeMath4IT")]
neededData <- neededData %>% filter_all(all_vars(!is.na(.)))


neededData$NativeLanguage <- replace(x = neededData$NativeLanguage,
                                list =  !neededData$NativeLanguage %in% c('Nederlands'), 
                                values =  'Other')
neededData

neededData$Nederlands <- replace(x = neededData$OtherLanguages,
                                     list =  neededData$OtherLanguages %in% c('Nederlands'), 
                                     values =  'Yes')

neededData$Nederlands[neededData$NativeLanguage == "Nederlands"] <- "Yes"
neededData$Nederlands <- ifelse(grepl("Nederlands", neededData$Nederlands), "Yes", neededData$Nederlands)
neededData$Nederlands[neededData$Nederlands != "Yes"] <- "No"

neededData$BirthCountry <- ifelse(grepl("België", neededData$BirthCountry), "Belgium", "Other")

neededData

#Hypothese opstellen
# H0_NL: Gemiddelde van X-Y = 0, waarbij X = NativeLanguage Nederlands is, Y = NativeLanguage Other is
# H1_NL: Gemiddelde van X-Y > 0, waarbij X = NativeLanguage Nederlands is, Y = NativeLanguage Other is

# H0_BC: Gemiddelde van X-Y = 0, waarbij X = BirthCountry Belgi?? is, Y = BirthCountry Other is
# H1_BC: Gemiddelde van X-Y > 0, waarbij X = BirthCountry Belgi?? is, Y = BirthCountry Other is

# H0_OL: Gemiddelde van X-Y = 0, waarbij X = OtherLanguages ThuisNederlandsOfNativeLanguageNederlands is, Y = OtherLanguages GeenExtraNederlands is
# H1_OL: Gemiddelde van X-Y > 0, waarbij X = OtherLanguages ThuisNederlandsOfNativeLanguageNederlands is, Y = OtherLanguages GeenExtraNederlands is


# 2. Significantie niveau
alfa = 0.05 #?

# 3. Toetsing
# Math4IT <-> Native Language
t(alfa, "greater", neededData$GradeMath4IT[neededData$NativeLanguage == 'Nederlands'], neededData$GradeMath4IT[neededData$NativeLanguage == 'Other'])


# Math4IT <-> Birth Country
t(alfa, "greater", neededData$GradeMath4IT[neededData$BirthCountry == 'Belgium'], neededData$GradeMath4IT[neededData$BirthCountry == 'Other'])

# Math4IT <-> Other Languages
t(alfa, "greater", neededData$GradeMath4IT[neededData$Nederlands == 'Yes'], neededData$GradeMath4IT[neededData$Nederlands == 'No'])




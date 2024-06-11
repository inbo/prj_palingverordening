########################################
####### INLEZEN DATA 2018 en 2021 #########
########################################

#LET OP DIT IS ANDERS DAN RAPPORTAGE IN DIE JAREN
#ER WORDEN BEKKEN-TYPE COMBINAties bij cpue gewijzigd om die allemaal te laten overeenkomen met dfOppBekTyp
library(tidyverse)
library(readxl)

### >>> vertalingssets

morfonaam_vertaling <-
  data.frame(MORFONAAM = c("bronbeken","getijrivier (brak)","getijrivier (zoet)","grote beek", "grote beek Kempen",
                           "grote rivier (> 20m)", "kanaal", "kleine beek", "kleine beek Kempen",
                           "kunstmatige waterloop (brak)", "kunstmatige waterloop (zoet)", "Maas", "rivier (< 20m)"),
             riviertype = c("Rk", "Mlz_O1", "Mlz_O1", "Bg", "BgK",
                            "Rg", "CANA", "Bk", "BkK",
                            "Pb", "Pz", "Rzg", "Rk"), stringsAsFactors = FALSE)

bekken_vertaling <-
  data.frame(BEKKEN = c("Beneden-Schelde", "Boven-Schelde", "Demer", "Dender", "Dijle", "Gentse Kanalen",
                        "Ijzer", "IJzer", "Leie", "Maas", "Nete", "Polders", "Zenne"),
             bekken = c("Beneden-Schelde", "Boven-Schelde", "Demer", "Dender", "Dijle", "Gentse kanalen",
                        "IJzer", "IJzer", "Leie", "Maas", "Nete", "Polders", "Dijle"), stringsAsFactors = FALSE)

bekken_vertaling_LG =
  data.frame(Bekken = c("Bekken Brugse polders", "Bekken Gentse Kanalen", "Benedenscheldebekken",
                        "Bovenscheldebekken", "Demerbekken", "Denderbekken",
                        "Dijlebekken","IJzerbekken", "Leiebekken", "Maasbekken", "Netebekken", "Stilstaand"),
             bekken =  c("Polders", "Gentse kanalen", "Beneden-Schelde",
                         "Boven-Schelde", "Demer", "Dender",
                         "Dijle", "IJzer", "Leie", "Maas", "Nete", "Stilstaand"), stringsAsFactors = FALSE)

#----------------------

### >>> Inlezen oppervlaktedataset (dfOppBekTyp_2018)

dfOppWL_2018 <-
  read_excel("Data/2018/Oppervlakte_WL_Palingbeheerplan_v20-05-15.xlsx", sheet = 1) %>% #neem deze van 2015
  left_join(morfonaam_vertaling) %>%
  left_join(bekken_vertaling) %>%
  mutate(hoofdbekken = ifelse(bekken == "Maas", "Maas", "Schelde"),
         area_ha = OPPERVL/10000)
dfOppWL_2021 <- dfOppWL_2018
saveRDS(dfOppWL_2018, "interim/df_area_2018.RDS")
saveRDS(dfOppWL_2021, "interim/df_area_2021.RDS")

tmpBekTyp2018 <-
  dplyr::select(dfOppWL_2018, hoofdbekken, bekken, riviertype, area_ha) %>%
  group_by(hoofdbekken, bekken, riviertype) %>%
  summarize(area_ha = sum(area_ha, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(fractie_area = area_ha/sum(area_ha))
tmpBekTyp2021 <- tmpBekTyp2018

dfOppBekTyp_2018 <-
  tmpBekTyp2018 %>%
  left_join(group_by(tmpBekTyp2018, hoofdbekken) %>% summarise(fractie_hoofdbekken = sum(fractie_area))) %>%
  left_join(group_by(tmpBekTyp2018, bekken) %>% summarise(fractie_bekken = sum(fractie_area))) %>%
  left_join(group_by(tmpBekTyp2018, riviertype) %>% summarise(fractie_riviertype = sum(fractie_area)))
dfOppBekTyp_2021 <- dfOppBekTyp_2018
saveRDS(dfOppBekTyp_2018, "interim/df_area_bek_typ_2018.RDS")
saveRDS(dfOppBekTyp_2021, "interim/df_area_bek_typ_2021.RDS")

#controle
table(dfOppWL_2018$riviertype, dfOppWL_2018$bekken) #toont het aantal records per combinatie
any(is.na(dfOppWL_2018$bekken)) #moet FALSE zijn
any(is.na(dfOppWL_2018$riviertype)) #moet FALSE zijn
sum(dfOppWL_2018$area_ha) #moet de volledige oppervlakte zijn in hectare
sum(dfOppBekTyp_2018$area_ha) #vergelijk met dfOppWL

#--------------------

### >>> Inlezen natuurlijke productie

dfProductie_2018 <-
  read_excel("Data/2018/Mortaliteit_en_Productie2018.xlsx", sheet = "productie") %>%
  filter(oorzaak == "natuurlijk", jaar == 2018) %>%
  left_join(dfOppBekTyp_2018, by = "hoofdbekken") %>%
  mutate(prod_zilver_nat = area_ha * waarde) %>%
  dplyr::select(hoofdbekken, bekken, riviertype, prod_zilver_nat)
dfProductie_2021 <- dfProductie_2018
saveRDS(dfProductie_2018, "interim/df_productie_2018.RDS")
saveRDS(dfProductie_2021, "interim/df_productie_2021.RDS")

#--------------------

### >>> Inlezen mortaliteiten aalscholvers en visserij

dfMortaliteit_2018 <-
  read_excel("Data/2018/Mortaliteit_en_Productie2018.xlsx", sheet = "mortaliteit") %>%
  filter(jaar == 2018) %>%
  left_join(dfOppBekTyp_2018, by = "hoofdbekken") %>%
  mutate(mortaliteit = waarde * fractie_area / fractie_hoofdbekken) %>%
  dplyr::select(hoofdbekken, bekken, riviertype, oorzaak, mortaliteit) %>%
  spread(key = oorzaak, value = mortaliteit) %>%
  dplyr::select(-pompgemalen) #Deze worden nu anders berekend, via een ander bestand
dfMortaliteit_2021 <- dfMortaliteit_2018
saveRDS(dfMortaliteit_2018, "interim/df_mortaliteit_2018.RDS")
saveRDS(dfMortaliteit_2021, "interim/df_mortaliteit_2021.RDS")

### >>> Inlezen mortaliteiten pompgemalen

dfMortPomp_2018 <-
  read_excel("Data/2018/Mortaliteit_Pompgemalen2018.xlsx", sheet = 1) %>%
  filter(!is.na(Bekken2018)) %>%
  transmute(bekken = Bekken2018,
            pompgemalen = `Pomp en turbineschade (Kg)`
)
dfMortPomp_2021 <- dfMortPomp_2018
saveRDS(dfMortPomp_2018, "interim/df_mortaliteit_pomp_2018.RDS")
saveRDS(dfMortPomp_2021, "interim/df_mortaliteit_pomp_2021.RDS")

#--------------------

### >>> Inlezen CPUE

dfCPUE_2018 <-
  read_excel("Data/2018/CPUE_2015-2017_v2018-04-14.xlsx", sheet = 1) %>%
  filter(substring(Methodenaa, 1, 4) == "Elek",
         soort == "paling",
         !is.na(CPUE)) %>%
  transmute(projectnaam = Projectnaa,
            waarneming = Waarneming,
            datum = Einddatum,
            jaar = lubridate::year(datum),
            gemeente = Gemeentena,
            bekken = BEKKEN_1,
            waterloop = NAAM,
            x = LambertX,
            y = LambertY,
            vhas = VHAS,
            riviertype = ifelse(CodeTypeWaterlichaam %in% c("MLZ", "Mlz", "O1"), "Mlz_O1", CodeTypeWaterlichaam),
            transectbreedte = BreedteTra,
            transectlengte = ifelse(LengteTraj == 0, 100, LengteTraj),
            transectarea = transectbreedte * transectlengte,
            rivierbreedte = BreedteRiv,
            cpue = CPUE) %>%
  mutate(bekken = gsub("bekken", "", bekken),
         bekken = gsub("Bekken Brugse P", "P", bekken),
         bekken = gsub("Bekken Gentse Kanalen", "Gentse kanalen", bekken),
         bekken = gsub("Ijzer", "IJzer", bekken),
         hoofdbekken = ifelse(bekken == "Maas", "Maas", "Schelde"))
#Extra filter (1 verdwijnt)
#dfCPUE_2018 <- filter(dfCPUE_2018, !(bekken == "Beneden-Schelde" & riviertype == "CANA"))

#Herdefiniëren zoals in 2015 (heeft geen impact, geen CPUE metingen in de data voor deze cases)
dfCPUE_2018$riviertype[dfCPUE_2018$riviertype == "Rg" & dfCPUE_2018$bekken %in% c("Demer", "Dijle")] <- "Rk"
dfCPUE_2018$riviertype[dfCPUE_2018$riviertype == "Rk" & dfCPUE_2018$bekken == "Gentse kanalen"] <- "Pz"
dfCPUE_2018$riviertype[dfCPUE_2018$riviertype == "Bk" & dfCPUE_2018$bekken == "Nete"] <- "BkK"
dfCPUE_2018$riviertype[dfCPUE_2018$riviertype == "Rg" & dfCPUE_2018$bekken == "Maas"] <- "CANA"
dfCPUE_2018$riviertype[dfCPUE_2018$riviertype == "Pz" & dfCPUE_2018$bekken == "IJzer"] <- "Bk"

saveRDS(dfCPUE_2018, "interim/df_cpue_2018.RDS")


#controle (NA waarden waar geen vissen gevangen zijn)
dfCPUE_2018 %>%
  group_by(riviertype, bekken, hoofdbekken) %>%
  summarise(aantal = n()) %>%
  full_join(dplyr::select(dfOppBekTyp_2018, riviertype, bekken, area_ha)) %>%
  print(n = 100)

###

dfCPUE_2021 <-
  read_excel("Data/2021/CPUE_2018-2020_v2021-05-19.xlsx", sheet = 1) %>%
  filter(substring(Methodenaam, 1, 4) == "Elek",
         #soort == "paling", vermoedelijk enkel palingen in CPUE file
         !is.na(CPUE)) %>%
  transmute(projectnaam = Projectnaam,
            waarneming = Waarneming,
            datum = Einddatum,
            jaar = lubridate::year(datum),
            gemeente = Gemeentenaam,
            bekken = Bekken,
            waterloop = TAG, #niet correct, maar anders geen waterloop te vinden
            x = LambertX,
            y = LambertY,
            vhas = VHAS,
            riviertype = ifelse(Verbeterd %in% c("MLZ", "Mlz", "O1"), "Mlz_O1", Verbeterd),
            transectbreedte = BreedteTra,
            transectlengte = ifelse(LengteTraj == 0, 100, LengteTraj),
            transectarea = transectbreedte * transectlengte,
            rivierbreedte = BreedteRiv,
            cpue = CPUE) %>%
  mutate(bekken = gsub("bekken ", "", bekken),
         bekken = gsub("Bekken ", "", bekken),
         bekken = gsub("bekken", "", bekken),
         bekken = gsub("Brugse p", "P", bekken),
         bekken = gsub("Gentse Kanalen", "Gentse kanalen", bekken),
         bekken = gsub("Ijzer", "IJzer", bekken),
         bekken = gsub("Benedenschelde", "Beneden-Schelde", bekken),
         bekken = gsub("Bovenschelde", "Boven-Schelde", bekken),
         hoofdbekken = ifelse(bekken == "Maas", "Maas", "Schelde"))
#Extra filter (6 verdwijnen)
#dfCPUE_2021 <- filter(dfCPUE_2021, !(bekken == "Beneden-Schelde" & riviertype == "CANA"))

#heeft geen impact, de CPUE-excel is ofwel al voorgefilterd, ofwel zijn er geen waarnemingen
dfCPUE_2021$riviertype[dfCPUE_2021$riviertype == "Rg" & dfCPUE_2021$bekken %in% c("Demer", "Dijle")] <- "Rk"
dfCPUE_2021$riviertype[dfCPUE_2021$riviertype == "Rk" & dfCPUE_2021$bekken == "Gentse kanalen"] <- "Pz"
dfCPUE_2021$riviertype[dfCPUE_2021$riviertype == "Bk" & dfCPUE_2021$bekken == "Nete"] <- "BkK"
dfCPUE_2021$riviertype[dfCPUE_2021$riviertype == "Rg" & dfCPUE_2021$bekken == "Maas"] <- "CANA"
dfCPUE_2021$riviertype[dfCPUE_2021$riviertype == "Pz" & dfCPUE_2021$bekken == "IJzer"] <- "Bk"

saveRDS(dfCPUE_2021, "interim/df_cpue_2021.RDS")

#controle
bind_cols(sort(unique(dfCPUE_2021$bekken)), sort(unique(dfOppBekTyp_2021$bekken)))

dfCPUE_2021 %>%
  group_by(riviertype, bekken, hoofdbekken) %>%
  summarise(aantal = n()) %>%
  full_join(dplyr::select(dfOppBekTyp_2021, riviertype, bekken, area_ha)) %>%
  arrange(bekken, hoofdbekken, riviertype) %>%
  filter(!is.na(aantal)) %>%
  print(n = 100)

#probleemgevallen (geen gevallen aanwezig)
dfCPUE_2021 %>%
  filter(riviertype == "Rg" & (bekken == "Maas" | bekken == "Dijle" | bekken == "Demer")) %>%   select(waarneming, datum, bekken, riviertype, cpue) %>%
  write_csv2(file = "cpue_zonder_bekken_oppervlakte.csv")


#--------------------

### >>> Inlezen LG-data

##2018

dfLG_2018 <-
  read_excel("Data/2018/LengteGewichten_2015-2017_v2018-03-21.xlsx", sheet = 1, guess_max = 5000) %>%
  filter(Soort == "paling",
         !is.na(TAXONGEW),
         !is.na(TAXONLEN),
         !is.na(Bekken),
         Bekken != "Stilstaand",
         TAXONAANTAL == 1) %>%
  left_join(bekken_vertaling_LG) %>%
  transmute(hoofdbekken = ifelse(bekken == "Maas", "Maas", "Schelde"),
            bekken,
            waarneming = WaarnemingKey,
            gewicht_g = TAXONGEW,
            gewicht_kg = gewicht_g / 1000,
            lengte_cm = TAXONLEN)
saveRDS(dfLG_2018, "interim/df_lg_2018.RDS")

dfLG_2018 %>%
  group_by(hoofdbekken, bekken) %>%
  summarise(aantal = n()) %>%
  full_join(group_by(dfOppBekTyp_2018, bekken) %>% summarise(area_ha = sum(area_ha))) %>%
  print(n = 20)

##2021

dfLG_2021 <-
  read_excel("Data/2021/LengteGewichten_2018-2020_v2021-05-04.xlsx", sheet = 1, guess_max = 5000) %>%
  filter(Soort == "paling",
         !is.na(TAXONGEW),
         !is.na(TAXONLEN),
         !is.na(Bekken),
         Bekken != "Stilstaand",
         TAXONAANTAL == 1) %>%
  left_join(bekken_vertaling_LG) %>%
  transmute(hoofdbekken = ifelse(bekken == "Maas", "Maas", "Schelde"),
            bekken,
            waarneming = WaarnemingKey,
            gewicht_g = TAXONGEW,
            gewicht_kg = gewicht_g / 1000,
            lengte_cm = TAXONLEN)
saveRDS(dfLG_2021, "interim/df_lg_2021.RDS")

dfLG_2021 %>%
  group_by(hoofdbekken, bekken) %>%
  summarise(aantal = n()) %>%
  full_join(group_by(dfOppBekTyp_2021, bekken) %>% summarise(area_ha = sum(area_ha))) %>%
  print(n = 20)

#==========================================


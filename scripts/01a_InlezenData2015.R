
require(readxl)
require(reshape2)
require(dplyr)

#############################################################################################################


morfonaam_vertaling <-
  data.frame(MORFONAAM = c("bronbeken","getijrivier (brak)","getijrivier (zoet)","grote beek", "grote beek Kempen",
                           "grote rivier (> 20m)", "kanaal", "kleine beek", "kleine beek Kempen",
                           "kunstmatige waterloop (brak)", "kunstmatige waterloop (zoet)", "Maas", "rivier (< 20m)"),
             riviertype = c("Rk", "Mlz_O1", "Mlz_O1", "Bg", "BgK",
                            "Rg", "CANA", "Bk", "BkK",
                            "Pb", "Pz", "Rzg", "Rk"), stringsAsFactors = FALSE)

bekken_vertaling_2015 <-
  data.frame(BEKKEN = c("Beneden-Schelde", "Boven-Schelde", "Demer", "Dender", "Dijle", "Gentse Kanalen",
                        "Ijzer", "IJzer", "Leie", "Maas", "Nete", "Polders", "Zenne"),
             bekken = c("Beneden-Schelde", "Boven-Schelde", "Demer", "Dender", "Dijle", "Gentse kanalen",
                        "IJzer", "IJzer", "Leie", "Maas", "Nete", "Polders", "Dijle"), stringsAsFactors = FALSE)

bekken_vertaling_LG_2015 =
  data.frame(Bekken = c("Bekken Brugse polders", "Bekken Gentse kanalen", "Beneden-Scheldebekken",
                        "Boven-Scheldebekken", "Demerbekken", "Denderbekken",
                        "Dijlebekken","Ijzerbekken", "Leiebekken", "Maasbekken", "Netebekken", "Stilstaand"),
             bekken =  c("Polders", "Gentse kanalen", "Beneden-Schelde",
                         "Boven-Schelde", "Demer", "Dender",
                         "Dijle", "IJzer", "Leie", "Maas", "Nete", "Stilstaand"), stringsAsFactors = FALSE)


dfOppWL_2015 <-
  read_excel("Data/2018/Oppervlakte_WL_Palingbeheerplan_v20-05-15.xlsx", sheet = 1) %>% #neem deze van 2015
  left_join(morfonaam_vertaling) %>%
  left_join(bekken_vertaling_2015) %>%
  mutate(hoofdbekken = ifelse(bekken == "Maas", "Maas", "Schelde"),
         area_ha = OPPERVL/10000)
saveRDS(dfOppWL_2015, "interim/df_area_2015.RDS")

tmpBekTyp2015 <-
  dplyr::select(dfOppWL_2015, hoofdbekken, bekken, riviertype, area_ha) %>%
  group_by(hoofdbekken, bekken, riviertype) %>%
  summarize(area_ha = sum(area_ha, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(fractie_area = area_ha/sum(area_ha))

dfOppBekTyp_2015 <-
  tmpBekTyp2015 %>%
  left_join(group_by(tmpBekTyp2015, hoofdbekken) %>% summarise(fractie_hoofdbekken = sum(fractie_area))) %>%
  left_join(group_by(tmpBekTyp2015, bekken) %>% summarise(fractie_bekken = sum(fractie_area))) %>%
  left_join(group_by(tmpBekTyp2015, riviertype) %>% summarise(fractie_riviertype = sum(fractie_area)))
saveRDS(dfOppBekTyp_2015, "interim/df_area_bek_typ_2015.RDS")

#### >>>> Oppervlaktes
#
# conn1 <- "Data/2015/Oppervlaktes_model2015.xlsx"
# DataOppAll2015 <- read_excel(conn1, "R_IMPORT")
# DataOppAll2015 <- subset(DataOppAll2015, !is.na(BEKNAAM))
# DataOppAll2015$"Mlz_O1" <- DataOppAll2015$O1 + DataOppAll2015$Mlz
# DataOppAll2015$O1 <- DataOppAll2015$Mlz <- NULL
#
# DataOppM2015 <- melt(DataOppAll2015, id.vars = "BEKNAAM", value.name = "Area_ha", variable.name = "RIVIERTYPE", factorsAsStrings = TRUE)
# names(DataOppM2015) <- c("bekken", "riviertype", "area_ha")
# DataOppM2015$hoofdbekken <- ifelse(DataOppM2015$bekken == "Maasbekken", "Maas", "Schelde")
# DataOppM2015$fractie_area <- DataOppM2015$area_ha / sum(DataOppM2015$area_ha)
# DataOppM2015 <- na.omit(DataOppM2015)
# DataOppM2015 <- DataOppM2015[DataOppM2015$area_ha > 0, ]
# sum(DataOppM2015$area_ha)
# DataOppM2015 <- merge(DataOppM2015, bekken_vertaling_2015, by.x = "bekken", by.y = "bekken2015")

### >>> Oppervlaktes: toch deze van 2018 gebruiken



### >>> vISSERIJmortaliteit

conn2 <- "Data/2015/Mortaliteit_visserij_model2015.xlsx"
DataMortVis2015 <- read_excel(conn2, "Mortaliteit visserij")
DataMortVisM2015 <- melt(DataMortVis2015, id.vars = "BEKNAAM", value.name  = "MortVisserij", variable.name = "RIVIERTYPE")
DataMortVisM2015 <- merge(DataMortVisM2015, bekken_vertaling_LG_2015, by.x = "BEKNAAM", by.y = "Bekken", all.x = TRUE)[,-1]

names(DataMortVisM2015) <- c("riviertype", "sportvisserij", "bekken")
sum(DataMortVisM2015$sportvisserij)
table(is.na(DataMortVisM2015$bekken))

### >>> Aalscholvermortaliteit

conn3 <- "Data/2015/Mortaliteit_aalscholvers_model2015.xlsx"
DataMortAal2015 <- read_excel(conn3, "Mortaliteit aalscholver")
DataMortAalM2015 <- melt(DataMortAal2015, id.vars = "BEKNAAM", value.name  = "MortAalscholver", variable.name = "RIVIERTYPE")
DataMortAalM2015 <- merge(DataMortAalM2015, bekken_vertaling_LG_2015, by.x = "BEKNAAM", by.y = "Bekken", all.x = TRUE)[,-1]
names(DataMortAalM2015) <- c("riviertype", "aalscholvers", "bekken")
table(is.na(DataMortAalM2015$bekken))
sum(DataMortAalM2015$aalscholvers)

### >>> Dataset aalscholver en visserijmortaliteit

DataMortGeel2015 <- merge(DataMortVisM2015, DataMortAalM2015, by = c("bekken", "riviertype"))
saveRDS(DataMortGeel2015, "interim/df_mortaliteit_2015.RDS")

### >>> Pompgemaalmortaliteit

#-- Oude Data die niet meer gebruikt wordt
# conn4 <- "Data/2015/Mortaliteit_pompgemalen_modelNieuw2015.xlsx"
# DataMortPomp <- read_excel(conn4, "Mortaliteit pompgemalen")
# DataMortPompM <- melt(DataMortPomp, id.vars = "BEKNAAM", value.name = "MortPomp", variable.name = "RIVIERTYPE")
# names(DataMortPompM) <- c("bekken", "riviertype", "pompgemalen")
# sum(DataMortPompM$pompgemalen)

DataMortPomp2015 <-
  read_excel("Data/2015/Mortaliteit_Pompgemalen_Nieuw2015.xlsx", sheet = 1) %>%
  filter(!is.na(Bekken2015)) %>%
  transmute(bekken = Bekken2018,
            pompgemalen = `Pomp en turbineschade (Kg)`
  )
saveRDS(DataMortPomp2015, "interim/df_mortaliteit_pomp_2015.RDS")

### Productie
dfProductie_2015 <-
  read_excel("Data/2015/Productie2015.xlsx", sheet = "productie") %>%
  filter(oorzaak == "natuurlijk", jaar == 2015) %>%
  left_join(dfOppBekTyp_2015) %>%
  mutate(prod_zilver_nat = area_ha * waarde) %>%
  dplyr::select(hoofdbekken, bekken, riviertype, prod_zilver_nat)
saveRDS(dfProductie_2015, "interim/df_productie_2015.RDS")


##################################################################################################################

#### >>>> DataZoet

conn1 <- "Data/2015/CPUE_model2015.xlsx"
DataOrig2015 <- read_excel(conn1, "data")
DataZoetAll2015 <- subset(DataOrig2015,
                      (Meetnet == "Zoetwatermeetnet") & (Methode == "Elektrovisserij") & (PBPwaters == 1) & !is.na(CPUE) &
                        !(CodeTypeWaterlichaam %in% c("Ai")))
DataZoetAll2015$riviertype <- DataZoetAll2015$CodeTypeWaterlichaam
DataZoetAll2015$riviertype[DataZoetAll2015$riviertype %in% c("O1", "Mlz")] <- "Mlz_O1"
DataZoetAll2015$cpue <- DataZoetAll2015$CPUE
DataZoetAll2015$transectarea <- DataZoetAll2015$AfgevisteOpp
DataZoetAll2015$transectbreedte <- DataZoetAll2015$BreedteTransect
DataZoetAll2015$transectlengte <- DataZoetAll2015$transectarea / DataZoetAll2015$transectbreedte
DataZoetAll2015$rivierbreedte <- DataZoetAll2015$BreedteRivier
DataZoetAll2015$waarneming <- DataZoetAll2015$WaarnemingKey

DataZoetAll2015 <- merge(DataZoetAll2015, bekken_vertaling_LG_2015, by.x = "BEKNAAM", by.y = "Bekken", all.x = TRUE)
DataZoetAll2015$hoofdbekken <- ifelse(DataZoetAll2015$bekken == "Maas", "Maas", "Schelde")

### DATA MANIPULATIES OP BASIS VAN GEVONDEN FOUTEN
DataZoetAll2015$riviertype[DataZoetAll2015$riviertype == "Rg" & DataZoetAll2015$bekken %in% c("Demer", "Dijle")] <- "Rk"
DataZoetAll2015$riviertype[DataZoetAll2015$riviertype == "Rk" & DataZoetAll2015$bekken == "Gentse kanalen"] <- "Pz"
DataZoetAll2015$riviertype[DataZoetAll2015$riviertype == "Bk" & DataZoetAll2015$bekken == "Nete"] <- "BkK"
DataZoetAll2015$riviertype[DataZoetAll2015$riviertype == "Rg" & DataZoetAll2015$bekken == "Maas"] <- "CANA"
DataZoetAll2015$riviertype[DataZoetAll2015$riviertype == "Pz" & DataZoetAll2015$bekken == "IJzer" &
                           DataZoetAll2015$WaarnemingKey == 4548] <- "Pb"
DataZoetAll2015$riviertype[DataZoetAll2015$riviertype == "Pz" & DataZoetAll2015$bekken == "IJzer"] <- "Bk"
dim(DataZoetAll2015)
table(is.na(DataZoetAll2015$bekken))

#Exta filter na onderzoek vorige keer, omdat er te weinig data is voor CANA in benedenschelde in 2018.
#LET OP: ER VERDWIJNEN SOWIESO HEEL WAT RIJEN DOOR CPUE TELLINGEN DIE NIET IN DE OPPERVLAKTEDATASET VOORKOMEN
DataZoet2015 <-
  merge(DataZoetAll2015, dfOppBekTyp_2015, all.x = TRUE) 
# DataZoet2015 <- DataZoet2015 %>%
#   filter(!(bekken == "Beneden-Schelde" & riviertype == "CANA"))

dim(DataZoet2015)

dfCPUE_2015 <- DataZoet2015 %>%
  transmute(projectnaam = Projectnaam, waarneming = WaarnemingKey, datum, jaar = lubridate::year(datum),
            gemeente = Gemeentenaam, bekken, waterloop = Waterloop,
            x = LambertX, y = LambertY, vhas = VHAS, riviertype,
            transectbreedte = BreedteTransect, transectlengte = LengteTraject,
            transectarea = transectbreedte * transectlengte,
            rivierbreedte,
            cpue, hoofdbekken)
saveRDS(DataZoet2015, "interim/df_cpue_2015.RDS")


##################################################################################################################

#### >>>> Lengte/Gewicht

conn <-  "Data/2015/LengteGewichten_model2015.xlsx"
DataL_Orig2015 <- read_excel(conn, "LengteGewichten", guess_max = 1000 )
DataL_Orig2015$riviertype <- as.character(DataL_Orig2015$CodeTypeWaterlichaam)
DataL_Orig2015 <- merge(DataL_Orig2015, bekken_vertaling_LG_2015, all.x = TRUE, by.x = "BEKNAAM", by.y = "Bekken")

DataL_Orig2015$gewicht_kg <- as.numeric(as.character(gsub(",", ".", DataL_Orig2015$Gcorr))) / 1000
DataL_Orig2015$lengte_cm <- as.numeric(as.character(gsub(",", ".", DataL_Orig2015$Lcorr)))
DataL_Orig2015$waarneming <- DataL_Orig2015$WaarnemingKey
DataL_Orig2015$hoofdbekken <- ifelse(DataL_Orig2015$bekken == "Maas", "Maas", "Schelde")

DataL2015 <- subset(DataL_Orig2015, TAXONAANTAL == 1 & (!is.na(bekken)) & (!is.na(riviertype)) & (bekken != "") & (riviertype != ""))
DataL2015$riviertype[DataL2015$riviertype %in% c('BkL', 'BkZ')] <- "Bk"
DataL2015$riviertype[DataL2015$riviertype == "MLZ"] <- "Mlz"
DataL2015 <- subset(DataL2015, !is.na(gewicht_kg) & !is.na(lengte_cm))
DataL2015$gewicht_g <- DataL2015$gewicht_kg * 1000

table(DataL2015$riviertype)

dfLG_2015 <- DataL2015 %>%
  transmute(hoofdbekken, bekken, waarneming = WaarnemingKey,
            gewicht_g, gewicht_kg, lengte_cm)
saveRDS(dfLG_2015, "interim/df_lg_2015.RDS")


##################################################################################################################



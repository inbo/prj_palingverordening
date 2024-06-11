library(tidyverse)
library(DBI)
library(readxl)
library(glue)
source("scripts/00_Functies.R")
conflicted::conflicts_prefer(dplyr::filter)
conflicted::conflicts_prefer(dplyr::select)

source_typology <- "data/2024/Karakterisering_VL+L1 met laatste wijziging 20januari20.xlsx"
xlvars <- c("CODE", "NAAM", "CATEGORIE", "TYPE", "STROOMGEBIED", "BEKKEN", "LENGTE (M)", "AFSTROOMGEBIED (M²)", "STATUUT")
riviertypes <- c("Bg", "BgK", "Bk", "BkK", "CANA", "Mlz", "Pb", "Pz", "Rg", "Rk", "Rzg")
bekkens <- c(`Bekken Brugse polders` = "Polders", `Bekken Gentse kanalen` = "Gentse kanalen",
             `Beneden-Scheldebekken` = "Beneden-Schelde", `Boven-Scheldebekken` = "Boven-Schelde",
             Demerbekken = "Demer",Denderbekken = "Dender",Dijlebekken = "Dijle",Ijzerbekken = "IJzer",
             Leiebekken = "Leie", Maasbekken = "Maas" ,Netebekken = "Nete")



############################################################
### INLEZEN CPUE en LENGTEGEWICHT DATA
############################################################

#Gegevens inlezen in het originele VIS DWH
#Wegens gebrek aan informatie is dit vervangen door het VIS2 DWH te bevragen
qry_wrn <- "
select dW.Projectcode
  , dW.WaarnemingKey
  , dW.WaarnemingID
  , dW.GebiedKey
  , dW.Einddatum
  , transectbreedte = dW.BreedteTransect
  , transectlengte = dW.LengteTraject
  , rivierbreedte = dW.BreedteRivier
  , dW.GebiedID
  , dg.GebiedCode 
  , dG.Gebiednaam
  , dG.LambertX
  , LambertY = dG.lambertY
  , dG.Gemeentenaam
  , dG.Provincienaam
  , dG.VHAG
  , dG.VHAS
  , bekken = dG.Bekken
  , wl = dGI.WaterlichaamCode
  , dGI.KRWTypologie
  , riviertype = dGI.CodeTypeWaterlichaam
  , waterlooptype = dGI.Waterlooptype
from dimWaarneming dW
inner join dimGebied dG on dG.GebiedKey = dW.GebiedKey
left join dimGebiedInfo dGI on dGI.Gebiedkey = dG.GebiedKey
where dW.Einddatum between '2021-01-01' and '2024-01-01'
and dW.MethodeCode like  'E%'
and dG.isCurrent = 1
"



#let op, lange kolom CPUEparameters moet achteraan anders "Descriptor index error"
qry_wrn2 <- "
SELECT Projectcode = dp.ProjectCode
    , dw.WaarnemingWID
    , dV.VispuntID 
    , datum = dd.Datum
    , dw.WaarnemingStatusCode
    , dV.VispuntBreedte
    , dV.VispuntCode
    , dv.VispuntX
    , dv.VispuntY
    , dv.VHAVispuntGemeente
    , dv.VHAVispuntProvincie
    , dV.VHAVispuntWatervlak
    , VHAS = dV.VHAVispuntWaterloopSegment
    , dV.VHAVispuntWaterloopGewestCode
    , bekken = dV.VHAVispuntBekkenNaam
    , dv.VHAVispuntVMMWaterlichaam
    , wl = dv.VHAVispuntVMMWaterlichaamCode
    , dv.VHAVispuntNaam
    , dw.WaarnemingCPUEParameters
FROM dimWaarneming dW
    inner join DimProject dP ON dP.ProjectWID = dW.WaarnemingProjectWID
    inner join DimDatum dd ON dd.DatumWID = dw.WaarnemingDatumWID
    inner join DimVispunt dv ON dv.VispuntID = dw.WaarnemingVispuntID
WHERE dd.Datum between '2021-01-01' and '2024-01-01'
and dW.WaarnemingStatusCode not in ( 'DEL' )
and dW.WaarnemingMethodeCode like 'E%'
order by WaarnemingStatusCode
"

qry_catch <- "select 
    fmp.WaarnemingKey
  , dW.ProjectCode
  , dW.GebiedID 
  , dW.Einddatum
  , aantal = fmp.TAXONAANTAL
  , gewicht_g = fmp.TAXONGEW
  , lengte_cm = fmp.TAXONLEN
  , fmp.TAXONTOTGEW
  , dT.Soort
from FactMeting_Pivot fmp
inner join dimWaarneming dW on dW.WaarnemingKey = fmp.WaarnemingKey
inner join dimTaxon dT on dT.TaxonKey = fmp.TaxonKey
where dW.Einddatum between '2021-01-01' and '2024-01-01'
and dW.MethodeCode like  'E%'
and fmp.isCurrent = 1
and fmp.TaxonKey = 673"

con <- DBI::dbConnect(odbc::odbc(),
                      Driver = "SQL Server",
                      Server = "inbo-sql08-prd.inbo.be",
                      port = 1433, # toegevoegd voor vpn,
                      Database = "W0001_00_Vis",
                      Trusted_Connection = "True"
)

con2 <- DBI::dbConnect(odbc::odbc(),
                      Driver = "SQL Server",
                      Server = "inbo-sql08-prd.inbo.be",
                      port = 1433, # toegevoegd voor vpn,
                      Database = "W0001_10_Vis",
                      Trusted_Connection = "True"
)

df_typologie <- 
  bind_rows(
    read_excel(source_typology, sheet = "VL") %>% select(all_of(xlvars)),
    read_excel(source_typology, sheet = "L1") %>% select(all_of(xlvars))
  ) %>% 
  transmute(wl = CODE,
            riviertype_excel = ifelse(STATUUT == "Kunstmatig" & TYPE == "Rg", "CANA", TYPE),
            bekken_check = BEKKEN) %>% 
  na.omit()

df_moments_dwh1 <- dbGetQuery(con, qry_wrn) %>%
  mutate(datum = as.Date(Einddatum)) %>% 
  select(Projectcode, WaarnemingKey, datum, GebiedID, VHAS, transectbreedte, transectlengte, rivierbreedte, riviertype1 = riviertype, bekken1 = bekken)  
#slechts voor 243 op 733 waarnemingen is het riviertype gekend via gebiedInfo
#voor 557 op 733 is VHAS gekend waarvan er 551 gekend zijn in VIS2

df_moments_dwh2 <- dbGetQuery(con2, qry_wrn2) %>% 
  mutate(datum = as.Date(datum))
#voor 546 van de 740 is VHASegment gekend voor 659 is waterlichaam gekend
#544 op 546 zijn gekend in VIS1


df_moments_dwh1$VHAS[! df_moments_dwh1$VHAS %in% df_moments_dwh2$VHAS]
df_moments_dwh2$VHAS[! df_moments_dwh2$VHAS %in% df_moments_dwh1$VHAS]

df_moments <- df_moments_dwh1 %>% 
  left_join(
    df_moments_dwh2 %>% 
      select(Projectcode, WaarnemingWID, VispuntID, datum, bekken2 = bekken, 
             wl, LambertX = VispuntX, LambertY = VispuntY, 
             WaarnemingCPUEParameters),
            by = c("Projectcode", "GebiedID" = "VispuntID", "datum")) %>% 
  left_join(df_typologie, by = "wl") %>% 
  mutate(riviertype = ifelse(!is.na(riviertype_excel), riviertype_excel, riviertype1),
         bekken = bekkens[bekken1],
         bekken = ifelse(WaarnemingKey == "19092", "Beneden-Schelde", bekken), #FOUT IN DB
         hoofdbekken = ifelse(bekken == "Maas", "Maas", "Schelde"),
         periode = "2021-2023") %>% 
  filter(!is.na(bekken), 
         !is.na(riviertype), 
         !riviertype_excel %in% c("Ami", "Awe", "Awom"), 
         !is.na(transectbreedte), !is.na(transectlengte), !is.na(rivierbreedte)) %>% 
  mutate(riviertype = replace(riviertype, #23
                              riviertype == "Rg" & bekken %in% c("Demer", "Dijle"),
                              "Rk"),
         riviertype = replace(riviertype, #6
                              riviertype == "Rk" & bekken == "Gentse kanalen",
                              "Pz"),
         riviertype = replace(riviertype, #0
                              riviertype == "Bk" & bekken == "Nete",
                              "BkK"),
         riviertype = replace(riviertype, #0
                              riviertype == "Rg" & bekken == "Maas",
                              "CANA"),
         riviertype = replace(riviertype, #2
                              riviertype == "Pz" & bekken == "IJzer",
                              "Bk"))


#alle vangstdata in een periode ongeacht elektrisch of niet en waarvoor een gekende waarneming is
df_catch_orig <- dbGetQuery(con, qry_catch) %>% 
  mutate(datum = as.Date(Einddatum), 
         Einddatum = NULL) %>% 
  filter(WaarnemingKey %in% df_moments$WaarnemingKey)

#toevoegen nulmetingen aan catch data
df_catch_expanded <- df_catch_orig %>% 
  bind_rows(df_moments %>% 
              transmute(WaarnemingKey, Projectcode, GebiedID, datum,
                        aantal = 0, gewicht_g = NA, lengte_cm = NA, 
                        TAXONTOTGEW = NA, Soort = "Paling", added = TRUE) %>% 
              filter(!WaarnemingKey %in% df_catch_orig$WaarnemingKey))

df_all <- df_catch_expanded %>% 
  inner_join(df_moments %>% 
               select(WaarnemingKey, transectbreedte, transectlengte, periode, 
                      rivierbreedte, riviertype, bekken, hoofdbekken, wl,
                      LambertX, LambertY),
             by = "WaarnemingKey")


#wat indien lengte van het traject niet gekend is? Deze data krijgt geen CPUE
df_cpue_2024 <- df_all %>% 
  group_by(periode, WaarnemingKey, bekken, hoofdbekken, riviertype, 
           transectlengte, transectbreedte, rivierbreedte,
           LambertX, LambertY) %>% 
  filter(!is.na(transectlengte)) %>% 
  summarise(Aantal = sum(aantal), cpue = sum(100 * aantal / transectlengte))
saveRDS(df_cpue_2024, "interim/df_cpue_2024.RDS")

df_lg_2024 <- df_all %>% 
  filter(aantal == 1, !is.na(gewicht_g), !is.na(lengte_cm))
saveRDS(df_lg_2024, "interim/df_lg_2024.RDS")

#####################################################################
### GEBIEDSGEGEVENS
#####################################################################

df_namen <- read_csv2("Data/2024/naamvertalingen.csv")

#wat met bronbeken? Die worden niet in beschouwing genomen?
df_area_2024 <-
  read_excel("Data/2024/Oppervlakte_WL_Palingbeheerplan_v20-05-15.xlsx",
             sheet = 1) %>% 
  mutate(bekken = BEKKEN,
         bekken = ifelse(bekken == "Ijzer", "IJzer", bekken),
         bekken = ifelse(bekken == "Zenne", "Dijle", bekken),
         riviertype = MORFONAAM, 
         hoofdbekken = ifelse(bekken == "Maas", "Maas", "Schelde"),
         area_ha = OPPERVL/10000) 

df_area_2024 <- df_area_2024 %>% 
  left_join(df_namen %>% filter(.data$type == "riviertype"), 
            by = c("riviertype" = "beschrijving")) %>% 
  left_join(df_namen %>% 
              filter(.data$type == "bekkenLG") %>% 
              rename(coderingB = codering),
            by = c("bekken" = "beschrijving")) %>% 
  mutate(riviertype = codering, 
         bekken = coderingB) %>% 
  select(-type.x, -type.y, -codering, -coderingB)
#Zennebekken nog wegfilteren?
saveRDS(df_area_2024, "interim/df_area_2024.RDS")

tmp_area_bek_typ_2024 <- df_area_2024 %>% 
  group_by(hoofdbekken, bekken, riviertype) %>% 
  summarize(area_ha = sum(area_ha, na.rm = TRUE), .groups = "drop") %>%
  mutate(fractie_area = area_ha/sum(area_ha))

df_area_bek_typ_2024 <- tmp_area_bek_typ_2024 %>% 
  left_join(tmp_area_bek_typ_2024 %>% 
              group_by(hoofdbekken) %>% 
              summarise(fractie_hoofdbekken = sum(fractie_area))) %>% 
  left_join(tmp_area_bek_typ_2024 %>% 
              group_by(bekken) %>% 
              summarise(fractie_bekken = sum(fractie_area))) %>%
  left_join(tmp_area_bek_typ_2024 %>% 
              group_by(riviertype) %>% 
              summarise(fractie_riviertype = sum(fractie_area)))
saveRDS(df_area_bek_typ_2024, "interim/df_area_bek_typ_2024.RDS")

#####################################################################
### PRODUCTIE EN MORTALITEIT
#####################################################################

### >>> Inlezen natuurlijke productie (gegevens uit)

df_productie_2024 <-
  read_excel("Data/2024/Mortaliteit_en_Productie2018.xlsx", 
             sheet = "productie") %>%
  filter(oorzaak == "natuurlijk", jaar == 2018) %>%
  left_join(df_area_bek_typ_2024, by = "hoofdbekken") %>%
  mutate(prod_zilver_nat = area_ha * waarde) %>%
  dplyr::select(hoofdbekken, bekken, riviertype, prod_zilver_nat)
saveRDS(df_productie_2024, "interim/df_productie_2024.RDS")

#--------------------

### >>> Inlezen mortaliteiten aalscholvers en visserij (gegevens uit 2018)

df_mortaliteit_2024 <-
  read_excel("Data/2024/Mortaliteit_en_Productie2018.xlsx", 
             sheet = "mortaliteit") %>%
  filter(jaar == 2018) %>%
  left_join(df_area_bek_typ_2024, 
            by = "hoofdbekken", 
            relationship = "many-to-many") %>%
  mutate(mortaliteit = waarde * fractie_area / fractie_hoofdbekken) %>%
  dplyr::select(hoofdbekken, bekken, riviertype, oorzaak, mortaliteit) %>%
  spread(key = oorzaak, value = mortaliteit) %>%
  dplyr::select(-pompgemalen) 
saveRDS(df_mortaliteit_2024, "interim/df_mortaliteit_2024.RDS")

### >>> Inlezen mortaliteiten pompgemalen (data uit 2018)

df_mortaliteit_pomp_2024 <-
  read_excel("Data/2024/Mortaliteit_Pompgemalen2018.xlsx", sheet = 1) %>%
  filter(!is.na(Bekken2018)) %>%
  transmute(bekken = Bekken2018,
            pompgemalen = `Pomp en turbineschade (Kg)`)
saveRDS(df_mortaliteit_pomp_2024, "interim/df_mortaliteit_pomp_2024.RDS")


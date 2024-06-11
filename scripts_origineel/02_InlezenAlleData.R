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
bekkens <- c(`Bekken Brugse polders` = "Brugse Polders", `Bekken Gentse kanalen` = "Gentse Kanalen",
             `Beneden-Scheldebekken` = "Beneden-Schelde", `Boven-Scheldebekken` = "Boven-Schelde",
             Demerbekken = "Demer",Denderbekken = "Dender",Dijlebekken = "Dijle",Ijzerbekken = "IJzer",
             Leiebekken = "Leie", Maasbekken = "Maas" ,Netebekken = "Nete")


###########################################################################
### INLEZEN ALLE DATA
###########################################################################

#mortaliteit en productie
###########################


df_cpue <- df_lg <- df_area <- df_area_bek_typ <- 
  df_mortaliteit <- df_mortaliteit_pomp <- df_productie <- NULL
for (year in c(2015, 2018, 2021, 2024)) {
  print(year)
  #path <- ifelse(year != 2024, glue("data/{year}/"), "interim/")
  path <- "interim/"
  df_mortaliteit <- df_mortaliteit %>% 
    bind_rows(
      readRDS(glue("{path}df_mortaliteit_{year}.RDS")) %>% 
        mutate(jaar = year,
               riviertype = as.character(riviertype)) %>% 
        clean_bek_rtyp()) 
  
  df_mortaliteit_pomp <- df_mortaliteit_pomp %>% 
    bind_rows(
      readRDS(glue("{path}df_mortaliteit_pomp_{year}.RDS")) %>% 
        mutate(jaar = year) %>%  
        clean_bek_rtyp())
  
  df_productie <- df_productie %>% 
    bind_rows(
      readRDS(glue("{path}df_productie_{year}.RDS")) %>% 
        mutate(jaar = year,
               riviertype = as.character(riviertype))) %>% 
    clean_bek_rtyp()
}


#################
#CPUE
#################

df_cpue_2015 <- readRDS("interim/df_cpue_2015.RDS") %>% mutate(jaar = 2015)
df_cpue_2018 <- readRDS("interim/df_cpue_2018.RDS") %>% mutate(jaar = 2018)
df_cpue_2021 <- readRDS("interim/df_cpue_2021.RDS") %>% mutate(jaar = 2021)
df_cpue_2024 <- readRDS("interim/df_cpue_2024.RDS") %>% mutate(jaar = 2024)

df_cpue <- df_cpue_2015 %>% 
  select(projectnaam = Projectnaam, waarneming = WaarnemingKey,
         datum, jaar, gemeente = Gemeentenaam, bekken, waterloop = Waterloop,
         x = LambertX, y = LambertY, riviertype, 
         transectbreedte, transectlengte, transectarea, rivierbreedte,
         cpue, hoofdbekken) %>% 
  bind_rows(df_cpue_2018 %>% select(-vhas)) %>% 
  bind_rows(df_cpue_2021 %>% select(-vhas)) %>% 
  bind_rows(df_cpue_2024 %>% ungroup() %>% 
              rename(x = LambertX, y = LambertY, waarneming = WaarnemingKey) %>%
              select(-periode, -Aantal)) %>% 
  clean_bek_rtyp() %>% 
  mutate(jaar = factor(jaar))

#Die gegevens werden er in 2018 uitgegooid, 
#en dus zorgen we dat dit hier voor alle periodes het geval is
#dit wil wel zeggen dat de palingen voor deze gevallen geimputeerd zullen worden uit de andere gegevens
df_cpue <- df_cpue %>% filter(!(bekken == "Beneden-Schelde" & riviertype == "CANA"))


#####################
#LENGTE EN GEWICHTEN
#####################

df_lg_2015 <- readRDS("data/2015/df_lg_2015.RDS") %>% mutate(jaar = 2015)
df_lg_2018 <- readRDS("data/2018/df_lg_2018.RDS") %>% mutate(jaar = 2018)
df_lg_2021 <- readRDS("data/2021/df_lg_2021.RDS") %>% mutate(jaar = 2021)
df_lg_2024 <- readRDS("interim/df_lg_2024.RDS") %>% 
  transmute(hoofdbekken, bekken, waarneming = WaarnemingKey, gewicht_g, 
            gewicht_kg = gewicht_g / 1000, lengte_cm, jaar = 2024)
df_lg <- bind_rows(df_lg_2015, df_lg_2018, df_lg_2021, df_lg_2024) %>% 
  clean_bek_rtyp() %>% 
  mutate(jaar = factor(jaar))

#########################
#AREA
##########################

df_area_2015 <- readRDS("data/2015/df_area_2015.RDS") %>% 
  mutate(jaar = 2015)
df_area_2018 <- readRDS("data/2018/df_area_2018.RDS") %>% 
  mutate(jaar = 2018)
df_area_2021 <- readRDS("data/2021/df_area_2021.RDS") %>% 
  mutate(jaar = 2021)
df_area_2024 <- readRDS("interim/df_area_2024.RDS") %>% 
  mutate(jaar = 2024)
df_area <- bind_rows(df_area_2015, df_area_2018, 
                             df_area_2021, df_area_2024) %>% 
  clean_bek_rtyp() %>% 
  mutate(jaar = factor(jaar))


df_area_bek_typ_2015 <- readRDS("data/2015/df_area_bek_typ_2015.RDS") %>% 
  mutate(jaar = 2015)
df_area_bek_typ_2018 <- readRDS("data/2018/df_area_bek_typ_2018.RDS") %>% 
  mutate(jaar = 2018)
df_area_bek_typ_2021 <- readRDS("data/2021/df_area_bek_typ_2021.RDS") %>% 
  mutate(jaar = 2021)
df_area_bek_typ_2024 <- readRDS("interim/df_area_bek_typ_2024.RDS") %>% 
  mutate(jaar = 2024)
df_area_bek_typ <- bind_rows(df_area_bek_typ_2015, df_area_bek_typ_2018, 
                             df_area_bek_typ_2021, df_area_bek_typ_2024) %>% 
  clean_bek_rtyp() %>% 
  mutate(jaar = factor(jaar))

###########################################
### CHECKS
###########################################

test <- df_cpue %>% 
  group_by(bekken, riviertype, jaar) %>% 
  summarise(aantal = n(), 
            aantal_met = sum(cpue > 0),
            mcpue = round(mean(cpue),2)) %>% 
  left_join(df_area_bek_typ %>% filter(jaar == 2024) %>% 
              select(bekken, riviertype, area_ha, fractie_area))

test %>% arrange(jaar) %>% pivot_wider(id_cols = c(bekken, riviertype, area_ha), values_from = aantal, names_from = jaar, values_fill = 0) %>% arrange(bekken, riviertype) %>% view()

test %>% arrange(jaar) %>% pivot_wider(id_cols = c(bekken, riviertype, area_ha), values_from = aantal_met, names_from = jaar, values_fill = 0) %>% arrange(bekken, riviertype) %>% view()

test %>% arrange(jaar) %>%  pivot_wider(id_cols = c(bekken, riviertype, area_ha), values_from = mcpue, names_from = jaar, values_fill = 0) %>% arrange(bekken, riviertype) %>% view()

#Niet gedefinieerd in het palingbeheerplanoppervlaktes
chk1 <- df_cpue_2024 %>% 
  filter((riviertype == 'Rg' & bekken == "Demer") | 
         (riviertype == 'Rg' & bekken == "Dijle") |
         (riviertype == "Pz" & bekken == 'IJzer') |
         (riviertype == "Rk" & bekken == "Gentse kanalen") |
         (riviertype == "Rk" & bekken == "Polders")
        )
wrns <- paste0("(", paste(chk1$WaarnemingKey, collapse = ","), ")")
  



library(tidyverse)
library(DBI)
library(readxl)
library(pscl)
library(INBOtheme)
library(glue)
source("scripts/00_Functies.R")
source("scripts/02_InlezenAlleData.R")

conflicted::conflicts_prefer(dplyr::filter)
conflicted::conflicts_prefer(dplyr::select)

fw <- 7
fh <- 5
dpi <- 300
prodnat <- 10 #natuurlijke productie paling

################################
### PRELIMINAIRE VERONDERSTELLINGEN
###############################

### >>> Model van Dekker

#illustratie van het probleem waar de geslachten 50/50 verdeeld zijn onder de 46cm en vanaf 46cm 100% vrouwtjes
#Vrouwtjes hebben een veel lagere zilverwordingskans, omdat ze vele jaren in onze watern blijven,
# en een vrouwtje van 46cm is in tegenstelling tot de mannetjes nog totaal niet zilverrijp.
ggplot(data.frame(Lengte = 10:80, Z = model_dekker(10:80)[,"Z"]), aes(x = Lengte, y = Z)) + geom_line() +
  ylab("Kans op zilverwording")
ggsave("output/illustratie_kans_op_zilver.png", width = fw, height = fh, dpi = dpi)

### >>> Illustratie Vangst- en Oevercorrectie

#invloed van de totale rivierbreedte (50m, 8m en vijvers van 500m) voor scenario "Old"

test <- data.frame(Bt = c(2:50,2:50), Br = 52, cpue = 1, scenario = "Old", Cd = rep(c(1,1.5), rep(49,2)))
test$cpueNew <-  rivier_en_vangstcorrectie_return_cpue(test$cpue, test$Br, test$Bt, test$scenario, test$Cd)
ggplot(test, aes(x = Bt, y = cpueNew, color = factor(Cd))) + geom_line() +
  xlab("Transectbreedte") + ylab("Correctiefactor (Ct * Cd)") + ggtitle("rivierbreedte = 50m")
ggsave("output/illustratie_vangstcorrectie_correctiefactor_050m.png", width = fw, height = fh, dpi = dpi)

test <- data.frame(Bt = c(2:8,2:8), Br = 8, cpue = 1, scenario = "Old", Cd = rep(c(1,1.5), rep(7,2)))
test$cpueNew <-  rivier_en_vangstcorrectie_return_cpue(test$cpue, test$Br, test$Bt, test$scenario, test$Cd)
ggplot(test, aes(x = Bt, y = cpueNew, color = factor(Cd))) + geom_line() +
  xlab("Transectbreedte") + ylab("Correctiefactor (Ct * Cd)") + ggtitle("rivierbreedte = 8m")
ggsave("output/illustratie_vangstcorrectie_correctiefactor_008m.png", width = fw, height = fh, dpi = dpi)

test <- data.frame(Bt = c(3:500,3:500), Br = 500, cpue = 1, scenario = "Old", Cd = rep(c(1,1.5), rep(498,2)))
test$cpueNew <-  rivier_en_vangstcorrectie_return_cpue(test$cpue, test$Br, test$Bt, test$scenario, test$Cd)
ggplot(test, aes(x = Bt, y = cpueNew, color = factor(Cd))) + geom_line() +
  xlab("Transectbreedte") + ylab("Correctiefactor (Ct * Cd)") + ggtitle("vijverstraal = 500m")

ggsave("output/illustratie_vangstcorrectie_correctiefactor_500m.png", width = fw, height = fh, dpi = dpi)



#################################################
#Relatieve oppervlaktes
#################################################

df_area %>%
  filter(jaar == 2024, !is.na(riviertype)) %>% #bronbeken zijn NA
  group_by(riviertype) %>%
  summarise(opp = sum(area_ha)) %>%
  ggplot(aes(x = riviertype, y = opp)) +
  geom_bar(stat = "identity") +
  xlab("") + ylab("Oppervlakte (ha)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("Output/opp_riviertype.png", width = fw, height = fh, dpi = dpi)

df_area %>%
  filter(jaar == 2024, !is.na(riviertype)) %>% 
  group_by(bekken) %>%
  summarise(opp = sum(area_ha)) %>%
  ggplot(aes(x = bekken, y = opp)) +
  geom_bar(stat = "identity") +
  xlab("") + ylab("Oppervlakte (ha)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("Output/opp_bekken.png", width = fw, height = fh, dpi = dpi)

df_area %>%
  filter(jaar == 2024, !is.na(riviertype)) %>% 
  group_by(hoofdbekken) %>%
  summarise(opp = sum(area_ha)) %>%
  ggplot(aes(x = hoofdbekken, y = opp)) +
  geom_bar(stat = "identity") +
  xlab("") + ylab("Oppervlakte (ha)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("Output/opp_hoofdbekken.png", width = fw, height = fh, dpi = dpi)

##################################################################
# AANTAL VISMOMENTEN
##################################################################

df_cpue <- df_cpue %>% filter(!(riviertype == "CANA" & bekken == "Beneden-Schelde"))

df_cpue %>%
  group_by(jaar, riviertype) %>%
  summarize(aantal = n()) %>%
  ggplot(aes(x = riviertype, y = aantal, fill = jaar)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single")) +
  labs(x = "", y = "aantal visbestandsopnames", fill = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.5))
ggsave("Output/opnames_riviertype.png", width = fw, height = fh, dpi = dpi)

df_cpue %>%
  group_by(jaar, riviertype) %>%
  summarize(aantal = n()) %>%
  left_join(df_area_bek_typ[df_area_bek_typ$jaar==2024,] %>% group_by(riviertype) %>% summarize(area_ha = sum(area_ha))) %>%
  mutate(n_cpue_ha = aantal / area_ha) %>%
  ggplot(aes(x = riviertype, y = n_cpue_ha, fill = jaar)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "", y = "aantal visbestandsopnames per hectare", fill = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.5))
ggsave("Output/opnames_ha_riviertype.png", width = fw, height = fh, dpi = dpi)

df_cpue %>%
  group_by(jaar, bekken) %>%
  summarize(aantal = n()) %>%
  ggplot(aes(x = bekken, y = aantal, fill = jaar)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "", y = "aantal visbestandsopnames", fill = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.5))
ggsave("Output/opnames_bekken.png", width = fw, height = fh, dpi = dpi)

df_cpue %>%
  group_by(jaar, bekken) %>%
  summarize(aantal = n()) %>%
  left_join(df_area_bek_typ[df_area_bek_typ$jaar==2024,] %>% group_by(bekken) %>% summarize(area_ha = sum(area_ha))) %>%
  mutate(n_cpue_ha = aantal / area_ha) %>%
  ggplot(aes(x = bekken, y = n_cpue_ha, fill = jaar)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "", y = "aantal visbestandsopnames per hectare", fill = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.5))
ggsave("Output/opnames_ha_bekken.png", width = fw, height = fh, dpi = dpi)


df_cpue %>%
  group_by(jaar, hoofdbekken) %>%
  summarize(aantal = n()) %>%
  ggplot(aes(x = hoofdbekken, y = aantal, fill = jaar)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "", y = "aantal visbestandsopnames", fill = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.5))
ggsave("Output/opnames_hoofdbekken.png", width = fw, height = fh, dpi = dpi)

#########################################################
# Lengtes en gewichten
#########################################################


modlg_2015 <- lm(log10(gewicht_g) ~ log10(lengte_cm), data = df_lg %>% filter(jaar == 2015))
modlg_2018 <- lm(log10(gewicht_g) ~ log10(lengte_cm), data = df_lg %>% filter(jaar == 2018))
modlg_2021 <- lm(log10(gewicht_g) ~ log10(lengte_cm), data = df_lg %>% filter(jaar == 2021))
modlg_2024 <- lm(log10(gewicht_g) ~ log10(lengte_cm), data = df_lg %>% filter(jaar == 2024))
modlgcoefs <-
  tibble( jaar = rep(c("2015","2018", "2021","2024"),2),
          param = rep(c("A", "B_pow_X"),rep(4,2)),
          value = c(A = 10^c(coef(modlg_2015)[1], coef(modlg_2018)[1], 
                   coef(modlg_2021)[1], coef(modlg_2024)[1]), 
                  c(coef(modlg_2015)[2], coef(modlg_2018)[2], 
                   coef(modlg_2021)[2], coef(modlg_2024)[2])))

summary(modlg_2024)#10^-2.95183x^3.13245= 0.001117301x^3.13245 R2=0.957

ggplot((df_lg[df_lg$jaar=="2024",]), aes(x = lengte_cm, y = gewicht_g)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), col = "blue") +
  ggtitle("Scatter plot with Polynomial Regression Line") +
  theme_minimal()

lgstats <- df_lg %>% 
  group_by(jaar) %>% 
  do(lg_summary_stats(.)) %>% 
  bind_rows(modlgcoefs)

lgstats %>% pivot_wider(id_cols = param, names_from = jaar) %>% 
  write_csv2(file = "output/lg_stats.csv")

linedata <- expand.grid(lengte_cm = 0:100, 
                        jaar = factor(c(2015, 2018, 2021, 2024))) %>% 
  group_by(jaar) %>% 
  do({
    Y <- .$jaar[1]
    A <- lgstats %>% 
      filter(jaar == Y, param == "A") %>% pull(value)
    B <- lgstats %>% 
      filter(jaar == Y, param == "B_pow_X") %>% pull(value)
    rv <- .
    estim <- A * rv$lengte_cm ^ B
    rv$gewicht_g <- A *rv$lengte_cm ^ B
    rv$formula <- paste0(round(A,4), " ", "X", "^", round(B,3))
    rv
  })

ggplot(df_lg, aes(x = lengte_cm, y = gewicht_g, color = jaar)) +
  geom_point() +
  geom_line(data = linedata, aes(group = jaar), color = inbo_palette()[5]) + 
  geom_text(data = linedata, aes(x = 0, y = 3000, group = jaar, label = formula), nudge_x = 30) +
  facet_wrap(~jaar) + 
  labs(x = "Lengte (cm)", y = "Gewicht (g)")
ggsave(file = paste0("Output/lg_overview.png"), width = fw, height = fh, dpi = dpi)

ggplot(df_lg, aes(x = jaar, y = lengte_cm)) + 
  #see::geom_violinhalf(fill = "grey60")  + 
  geom_violin(fill = "grey60")+
  geom_boxplot(width = 0.2) +
  facet_wrap(~ bekken  )

ggplot(data = df_lg[df_lg$jaar == "2024",], aes(x = lengte_cm)) +
  geom_histogram() + facet_wrap(~bekken, scales = "free_y") +
  labs(x = "Lengte (cm)", y = "aantal palingen")
ggsave(file = paste0("Output/lg_bek_2024.png"), width = fw, height = fh, dpi = dpi)


# Aantal vrouwtjes
(aantal_vr_bek <- df_lg %>%
    group_by(jaar, bekken) %>%
    summarize(aantal = n(),
              aantalgd46 = sum(lengte_cm >= 46),
              pctgd46 = mean(lengte_cm >= 46) * 100) %>%
    mutate(pctvrouwtjes = (aantalgd46 + (aantal - aantalgd46)/2) / aantal * 100)) %>%
  write_excel_csv2("output/aandeelvrouwtjes_per_bekken.csv")

ggplot(aantal_vr_bek, aes(x = bekken, y = pctvrouwtjes, fill = jaar)) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(x = "", y = "geschat percentage vrouwtjes (%)", fill = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
ggsave(file = paste0("Output/aandeel_vrouwtjes.png"), width = fw, height = fh, dpi = dpi)

############################################################
#MODELBEREKENINGEN
############################################################

### >>> Rapportage 2024

#proportie vrouwtjes, gewicht geel en zilver
lg_berekening <- df_lg %>% 
  group_by(jaar) %>% 
  do(calc_lg(., 40, 60, 0.05))

#imputaties
cpue_impute <- df_cpue %>% 
  group_by(jaar) %>% 
  do({
    y <- .$jaar[1]
    print(y)
    abt <- df_area_bek_typ %>% filter(jaar == y)
    calc_cpue(., abt)
  })

zilver <- cpue_impute %>% 
  group_by(jaar) %>% 
  do({
    y <- .$jaar[1]
    abt <- df_area_bek_typ %>% filter(jaar == y)
    print(y)
    calc_zilver(., 
                lg_berekening %>% filter(jaar == y),
                df_mortaliteit %>% filter(jaar == y),
                df_mortaliteit_pomp %>% filter(jaar == y), 
                prodnat = prodnat)
  })  

pct_ontsnapt_hb <- zilver %>%
  group_by(jaar, hoofdbekken) %>% 
  summarize(
    pct_ontsnapt = sum(gew_zilver_netto) / sum(gew_natuurlijke_productie)
)
ggplot(pct_ontsnapt_hb, aes(x = hoofdbekken, y = 100 * pct_ontsnapt, fill = jaar)) + 
  geom_col(position = position_dodge()) + ylab("percentage ontsnapt")
ggsave("output/ontsnapping_per_hoofdbekken.png", width = fw, height = fh, dpi = dpi)

###########################################################################################
### Eindevaluatie
###########################################################################################

allresults <- zilver %>% 
  dplyr::select(jaar, hoofdbekken, bekken, riviertype, area_ha, n_obs_cpue = N_obs,
                cpue_mean, cpue_ha_nocorr = cpue_ha_raw, cpue_ha_corr = cpue_ha_corr, cpue_ha_fit = fit,
                cpue_ha, aantal_geel_tot = aantal_geel,
                n_obs_LG = Aantal, gewicht_geel_pp = Gew_geel_pp, p_female = P_female,
                aantalfractie_zilver = aandeel_zilver_aantal, gewichtsfractie_zilver = aandeel_zilver_gewicht,
                gewicht_geel_bruto = gew_geel_bruto, surv_vissers_aalscholvers = fractie_mort_geel,
                gewicht_geel_netto = gew_geel_netto,
                gewicht_zilver_bruto = gew_zilver_bruto, surv_pompgemalen = fractie_pompgemalen,
                gewicht_zilver_netto = gew_zilver_netto,
                gewicht_zilver_natuur = gew_natuurlijke_productie,
                fractie_ontsnapt = pct_ontsnapt
  ) %>%
  arrange(jaar, hoofdbekken, bekken, riviertype)


allresultsHB <-
  group_by(allresults, jaar, hoofdbekken) %>%
  summarize(gewicht_zilver_netto = sum(gewicht_zilver_netto),
            fractie_ontsnapt = sum(gewicht_zilver_netto) / sum(gewicht_zilver_natuur),
            gewicht_zilver_natuur = sum(gewicht_zilver_natuur),
            n_obs_cpue = sum(n_obs_cpue, na.rm = TRUE),
            cpue_ha  = sum(aantal_geel_tot) / sum(area_ha)) %>%
  mutate(gewicht_zilver_gemist = gewicht_zilver_natuur - gewicht_zilver_netto)
allresultsHB

allresultsVl <-
  group_by(allresults, jaar) %>%
  summarize(gewicht_zilver_netto = sum(gewicht_zilver_netto),
            fractie_ontsnapt = sum(gewicht_zilver_netto) / sum(gewicht_zilver_natuur),
            gewicht_zilver_natuur = sum(gewicht_zilver_natuur),
            n_obs_cpue = sum(n_obs_cpue, na.rm = TRUE),
            cpue_ha  = sum(aantal_geel_tot) / sum(area_ha)) %>%
  mutate(gewicht_zilver_gemist = gewicht_zilver_natuur - gewicht_zilver_netto)
allresultsVl


allresultsB <- group_by(allresults, jaar, bekken) %>%
  summarize(gewicht_zilver_netto = sum(gewicht_zilver_netto),
            fractie_ontsnapt = sum(gewicht_zilver_netto) / sum(gewicht_zilver_natuur),
            gewicht_zilver_natuur = sum(gewicht_zilver_natuur),
            n_obs_cpue = sum(n_obs_cpue, na.rm = TRUE),
            cpue_ha  = sum(aantal_geel_tot) / sum(area_ha)) %>%
  mutate(gewicht_zilver_gemist = gewicht_zilver_natuur - gewicht_zilver_netto)

allresultsR <- group_by(allresults, jaar, riviertype) %>%
  summarize(gewicht_zilver_netto = sum(gewicht_zilver_netto),
            fractie_ontsnapt = sum(gewicht_zilver_netto) / sum(gewicht_zilver_natuur),
            gewicht_zilver_natuur = sum(gewicht_zilver_natuur),
            n_obs_cpue = sum(n_obs_cpue, na.rm = TRUE),
            cpue_ha  = sum(aantal_geel_tot) / sum(area_ha)) %>%
  mutate(gewicht_zilver_gemist = gewicht_zilver_natuur - gewicht_zilver_netto)

write.csv2(allresults, file = "Output/Resultaten_Ruw.csv")
write.csv2(allresultsHB, file = "Output/Resultaten_Ruw_HB.csv")


#Hier een probleem voor 2024
allresultsHB %>%
  group_by(jaar, hoofdbekken) %>%
  summarize(natuurlijk = mean(gewicht_zilver_natuur)) %>%
  ggplot(aes(x = hoofdbekken, y = natuurlijk, fill = jaar)) +
  geom_bar(stat = "identity",  position = position_dodge()) + 
  ylab("natuurlijke productie") + xlab("")
ggsave("output/natuurlijke_productie_hoofdbekken_bis.png", width = fw, height = fh, dpi = dpi)



allresultsHB %>%
  ggplot(aes(x = hoofdbekken, y = gewicht_zilver_netto, fill = jaar)) +
  geom_bar(stat = "identity", position = "dodge") + 
  ylab("netto productie zilverpaling (kg)") + xlab("")
# ggsave("output/gewicht_zilver_netto_per_hoofdbekken.png", width = fw, height = fh, dpi = dpi)

allresultsB %>%
  group_by(jaar, bekken) %>%
  summarize(natuurlijk = mean(gewicht_zilver_natuur)) %>%
  ggplot(aes(x = bekken, y = natuurlijk, fill = jaar)) +
  geom_bar(stat = "identity", position = "dodge") + 
  ylab("natuurlijke productie") + xlab("") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
ggsave("output/natuurlijke_productie_bekken_bis.png", width = fw, height = fh, dpi = dpi)

allresultsR %>%
  group_by(jaar, riviertype) %>%
  summarize(natuurlijk = mean(gewicht_zilver_natuur)) %>%
  ggplot(aes(x = riviertype, y = natuurlijk, fill = jaar)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single")) + 
  ylab("natuurlijke productie") + xlab("") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
ggsave("output/natuurlijke_productie_riviertype_bis.png", width = fw, height = fh, dpi = dpi)


allresultsR %>%  select(riviertype, gewicht_zilver_netto, gewicht_zilver_gemist) %>%
  pivot_longer(cols = c(gewicht_zilver_netto, gewicht_zilver_gemist)) %>%
  ggplot(aes(x = riviertype, y = value, fill = name)) + geom_bar(stat = "identity") + facet_wrap(~jaar) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

allresultsB %>%  select(bekken, gewicht_zilver_netto, gewicht_zilver_gemist) %>%
  pivot_longer(cols = c(gewicht_zilver_netto, gewicht_zilver_gemist)) %>%
  ggplot(aes(x = bekken, y = value, fill = name)) + geom_bar(stat = "identity") + facet_wrap(~jaar) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


###

allresultsHB %>% bind_rows(allresultsVl %>% mutate(hoofdbekken = "Totaal")) %>%
  ggplot(aes(x = hoofdbekken, y = fractie_ontsnapt, fill = jaar)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "", y = "fractie ontstnapt", fill = "")
ggsave(filename = "output/fractie_ontsnapt_hoofdbekken_vl.png",
       width = fw, height = fh, dpi = dpi)

vars <- c("gewicht_zilver_natuur", "gewicht_zilver_netto", "fractie_ontsnapt", "n_obs_cpue", "cpue_ha")
varlabs <- c("natuurlijke productie (kg)", "netto productie zilverpaling (kg)", "fractie zilverpaling ontsnapt",
             "aantal observaties", "aantal palingen per hectare")

for (i in 1:length(vars)) {
  var = vars[i]
  print(var)
  ylabel = varlabs[i]
  ggplot(allresultsB, aes(x = bekken, y = !!sym(var), fill = jaar)) +
    geom_bar(position = "dodge", stat = "identity") + ylab(ylabel) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = rel(0.8)))
  ggsave(file = paste0("Output/final_result_",var,"_per_bekken.png"), width = fw, height = fh, dpi = dpi)
  
  ggplot(allresultsR, aes(x = riviertype, y = !!sym(var), fill = jaar)) +
    geom_bar(position = "dodge", stat = "identity") + ylab(ylabel) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = rel(0.8))) 
  ggsave(file = paste0("Output/final_result_",var,"_per_riviertype.png"), width = fw, height = fh, dpi = dpi)
  
  ggplot(allresultsHB, aes(x = hoofdbekken, y = !!sym(var), fill = jaar)) +
    geom_bar(position = "dodge", stat = "identity") + ylab(ylabel) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = rel(0.8))) 
  ggsave(file = paste0("Output/final_result_",var,"_per_hoofdbekken.png"), width = fw, height = fh, dpi = dpi)
  
  ggplot(allresults, aes(x = riviertype, y = !!sym(var), fill = jaar)) +
    geom_bar(position = "dodge", stat = "identity") + facet_wrap(~bekken, ncol = 3) +
    theme(axis.text.x = element_text(angle = 90, size = rel(0.8))) + ylab(ylabel)
  ggsave(file = paste0("Output/final_result_",var, "_per_riviertype_bekken.png"), width = fw, height = fh, dpi = dpi)
  
  ggplot(allresults, aes(x = riviertype, y = !!sym(var), fill = jaar)) +
    geom_bar(position = "dodge", stat = "identity") + facet_wrap(~bekken, scales = "free_y", ncol = 3) +
    theme(axis.text.x = element_text(angle = 90, size = rel(0.8))) + ylab(ylabel)
  ggsave(file = paste0("Output/final_result_",var,"_per_riviertype_bekken_geschaald.png"), width = fw, height = fh, dpi = dpi)
}


ggplot(allresults, aes(x = as.numeric(jaar), y = fractie_ontsnapt, color = riviertype)) + geom_line() + facet_wrap(~bekken) + scale_color_manual(values = rainbow(12)) + xlab(jaar)

ggplot(allresultsHB, aes(x = as.numeric(jaar), y = Gewi)


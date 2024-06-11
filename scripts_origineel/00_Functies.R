clean_bek_rtyp <- function(x) {
  y <- x %>% 
    mutate(bekken = str_squish(str_replace(pattern = "bekken|Bekken", 
                                           replacement = "", 
                                           bekken)),
           bekken = str_replace(pattern = "Brugse polders", 
                                replacement = "Polders", 
                                bekken),
           hoofdbekken = ifelse(bekken == "Maas", "Maas", "Schelde"))
  
  if("riviertype" %in% colnames(y) ) {
    y <- y %>% 
      mutate(riviertype = ifelse(riviertype %in% c("MLZ","Mlz", "O1"), "Mlz_O1", riviertype ))    
  }
  y
}

#########################################################################

model_dekker <- function(L, LschierM =40, LschierF = 60, Mort = 0.05){
  P_fem <- ifelse(L >= 46, 1, 0.5)
  S_fem <- 0.12 /  (1 + exp(-(L-LschierF) / 2.4))
  S_male <- 1 / (1 + exp(-(L-LschierM) / 2.8))
  Z_male <- S_male / (Mort + S_male) * (1 - exp(-Mort - S_male))
  Z_fem <- S_fem / (Mort + S_fem) * (1 - exp(-Mort - S_fem))
  Z_alg <- P_fem * Z_fem + (1 - P_fem) * Z_male
  cbind(Pf = P_fem, Z = Z_alg,  Zm = Z_male, Zf = Z_fem)
}

#######################################################################

rivier_en_vangstcorrectie_return_cpue <- function(cpue, Br, Bt, scenario, Cd){

  RivierCorrLogit <- function(Br, Bt, params){
    #Ct = D + (C * e^(A*x + B)/(e^(A*x + B) + 1))
    x <- Br / Bt
    y <- params["D"] + params["C"] * plogis(params["A"] * x + params["B"])
    return(y)
  }

  if (length(scenario) == 1) scenario <-  rep(scenario, length(cpue))
  if (length(Cd) == 1) Cd <- rep(Cd, length(cpue))


  Ct <- ifelse(scenario == "Old",
               1 - pnorm(Br/2 - Bt/2, mean = Br/2, sd = sqrt(Br/2)),
               ifelse(scenario == "None",
                      rep(1, length(cpue)),
                      ifelse(scenario == "A",
                             RivierCorrLogit(Br, Bt, params = c(A = -6.5, B = 9.0, C = 0.05, D = 0.95)),
                             ifelse(scenario == "B",
                                    RivierCorrLogit(Br, Bt, params = c(A = -2.5, B = 6.3, C = 0.52, D = 0.48)),
                                    ifelse(scenario == "C",
                                           RivierCorrLogit(Br, Bt, params = c(A = -2.3, B = 6.0, C = 0.70, D = 0.30)),
                                           ifelse(scenario == "S",
                                                  RivierCorrLogit(Br, Bt, params = c(A = -2.4, B = 5.5, C = 0.30, D = 0.70)),
                                                  rep(1, length(cpue))))))))
  return(cpue * Ct * Cd)
}

###########################################################################

calc_lg <- function(lg, LschierM, LschierF, Mort) {
  df <- dplyr::select(lg, bekken, gewicht_g, lengte_cm) %>% mutate(sim = 0)
  lgcoefs <- coef(summary(lm(log10(gewicht_g) ~ log10(lengte_cm), data = df)))

  dekdata <- model_dekker(L = df$lengte_cm, LschierM = LschierM, LschierF = LschierF,
                          Mort = Mort)[,c("Pf", "Z", "Zm", "Zf")]
  Zdata <- cbind(df, dekdata)

  calcdata <-
    group_by(Zdata, bekken) %>%
    summarize(Aantal = n(),
              Gew_geel_pp = mean(gewicht_g) / 1000,
              Gew_indien_zilver = mean(gewicht_g / 1000 * Z) / mean(Z),
              P_female = mean(Pf),
              aandeel_zilver_aantal = mean(Z),
              aandeel_zilver_gewicht = mean(gewicht_g * Z)/mean(gewicht_g),
              lgIntercept = 10^lgcoefs[1,1],
              lgSlope = lgcoefs[2,1]) %>%
    mutate(Gew_avg_kg_zilver = aandeel_zilver_gewicht * Gew_geel_pp)
  calcdata
}

############################################################################

calc_cpue <- function(cpue, oppBT, model_missings = TRUE) {
  cpueW <-
    mutate(cpue,
           cpue_ha_raw = cpue / (100 * transectbreedte) * 10000, #delen door transectarea is niet correct als L != 100
           cpue_ha = rivier_en_vangstcorrectie_return_cpue(cpue_ha_raw,
                                                           Br = rivierbreedte,
                                                           Bt = transectbreedte,
                                                           scenario = "Old",
                                                           Cd = 1.5),
           cpue_ha_rounded = round(cpue_ha))
  
  #newdata <- expand.grid(bekken = unique(cpueW$bekken), riviertype = unique(cpueW$riviertype))
  newdata <- unique(dplyr::select(oppBT, bekken, riviertype))
  remove_missing_levels <- which(!(newdata$riviertype %in% cpueW$riviertype))
  if (length(remove_missing_levels)) {
    newdata <- newdata %>% slice(-remove_missing_levels)    
  }
  if (model_missings) {
    model <- zeroinfl(data = cpueW,
                      cpue_ha_rounded ~ bekken + riviertype  | bekken,
                      dist = "negbin", link = "logit")
    newdata$fit <- predict(model, newdata = newdata)
  } else {
    fitted <- cpueW %>% group_by(bekken, riviertype) %>% summarize(fit = mean(cpue_ha))
    newdata <- inner_join(newdata, fitted)
  }
  dfcpue <-
    group_by(cpueW, bekken, riviertype) %>%
    summarize(N_obs = sum(!is.na(cpue_ha)),
              cpue_mean = mean(cpue),
              cpue_ha_raw = mean(cpue_ha_raw),
              cpue_ha_corr = mean(cpue_ha)) %>%
    right_join(oppBT) %>%
    inner_join(newdata) %>%
    mutate(cpue_ha = ifelse(!is.na(cpue_ha_corr), cpue_ha_corr, fit),
           aantal_geel = cpue_ha * area_ha) %>%
    dplyr::select(hoofdbekken, bekken, riviertype, N_obs, area_ha, fractie_area,
                  cpue_mean, fit, cpue_ha_raw, cpue_ha_corr, cpue_ha, aantal_geel)
  plot(dfcpue$fit, dfcpue$cpue_ha, col = 1 + is.na(dfcpue$cpue_ha_corr)); abline(a=0,b=1)
  dfcpue
}

###########################################################################################

calc_zilver <- function(cpuecalc, lgcalc, mort, mortpomp,  prodnat) {
  #LET OP: WERKT ENKEL MET VOLLEDIGE DATASET
  dfgeg <-
    left_join(cpuecalc, lgcalc, by = c("bekken")) %>%
    mutate(gew_geel_bruto = aantal_geel * Gew_geel_pp)

  totaalgewicht_geel <- sum(dfgeg$gew_geel_bruto)
  mortAalsch <- sum(mort$aalscholvers)
  mortVissers <- sum(mort$sportvisserij)
  pct_aalsch_en_visserij <- (totaalgewicht_geel - mortAalsch - mortVissers) / totaalgewicht_geel

  dfgeg <- mutate(dfgeg,
                  fractie_mort_geel = pct_aalsch_en_visserij,
                  gew_geel_netto = gew_geel_bruto * fractie_mort_geel, #is gelijk voor elk bekken
                  gew_zilver_bruto = gew_geel_netto * aandeel_zilver_gewicht)  #verschilt per bekken

  dfgegbek <-
    inner_join(dfgeg, mortpomp) %>%
    group_by(bekken) %>%
    summarize(gew_zilver_bruto = sum(gew_zilver_bruto),
              pompgemalen = mean(pompgemalen)) %>%
    mutate(fractie_pompgemalen = (gew_zilver_bruto - pompgemalen)/gew_zilver_bruto) %>%
    dplyr::select(bekken, fractie_pompgemalen)

  dfgeg <-
    left_join(dfgeg, dfgegbek, by = "bekken") %>%
    mutate(gew_zilver_netto = gew_zilver_bruto * fractie_pompgemalen,
           gew_natuurlijke_productie = prodnat * area_ha,
           pct_ontsnapt = gew_zilver_netto / gew_natuurlijke_productie)

  dfgeg
}

#########################################################################################################

lg_summary_stats <- function(data) {
  nlen <- length(!is.na(data$lengte_cm))
  avglen <- mean(data$lengte_cm, na.rm = TRUE)
  qlen <- quantile(data$lengte_cm, na.rm = TRUE, probs = c(0.5,0,1))
  ngew <- length(!is.na(data$gewicht_g))
  avggew <- mean(data$gewicht_g, na.rm = TRUE)
  qgew <- quantile(data$gewicht_g, na.rm = TRUE, probs = c(0.5,0,1))
  rv <- tibble(param = c("avg_len", "med_len", "min_len", "max_len","n_len", 
                         "avg_gew", "med_gew", "min_gew", "max_gew","n_gew"),
               value = c(avglen, qlen[1], qlen[2], qlen[3], nlen, 
                         avggew, qgew[1], qgew[2], qgew[3], ngew))
  rv
}




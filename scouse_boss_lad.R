# =============================================================================
# Proyecto:        Análisis paddy pimblett vs justin gaethje
# Fecha:                         23/01/2026
# Método:                 Regresión logística binaria
# =============================================================================

# ----librerías----

library(pacman)
p_load(tidyverse, ciTools, forcats, MASS, here, brglm2, janitor)

# ---- Cargamos los datos ----

estats_fighters <- read_csv(here("Fighters Stats.csv"))

historico_peleas <- read_csv(here("Fights.csv"))


# ---- limpiamos los datos ----

# primero generamos la base con los datos de las peleas de manera histórica 
# solo con los datos que nos interesan

datos_ent <- historico_peleas %>% 
  dplyr::select("id_pelea" = "Fight_Id", 
                "id_peleador1" = "Fighter_Id_1",
                "id_peleador2"  = "Fighter_Id_2",
                "peleador1" = "Fighter_1",
                "peleador2" = "Fighter_2",
                "resultado1" = "Result_1",
                "resultado2" =  "Result_2")

# limpiamos la base de las estats históricas de los peleadores

estats_fighters <- estats_fighters %>% 
  rename(id_peleador = Fighter_Id,
         peleador = `Full Name`,
         apodo = Nickname)


estats_fighters <- estats_fighters %>% 
  dplyr::select("peleador","apodo","id_peleador","KD", "STR",
                "TD","SUB","Ctrl", "Sub. Att","Sig. Str. %",
                "KO Rate","Clinch_%", "DEC Rate")

# un pequeño merge para completar la base final

datos_entrenamiento <- datos_ent %>% 
  left_join(estats_fighters, by = c("id_peleador1" = "id_peleador")) %>% 
  rename_with(~paste0(., "p_1"), .cols = KD:`DEC Rate`)

datos_entrenamiento <- datos_entrenamiento %>% 
  left_join(estats_fighters, by = c("id_peleador2" = "id_peleador")) %>% 
  rename_with(~paste0(., "p_2"), .cols = KD:`DEC Rate`)

# generamos las diferencias con las que vamos a estimar los datos 



datos_entrenamiento <- datos_entrenamiento %>% 
  mutate(
    gana_p1 = ifelse(resultado1 == "W", 1, 0),
    diff_kd = KDp_1 - KDp_2,
    diff_str = STRp_1 - STRp_2,
    diff_td = TDp_1 - TDp_2,
    diff_sub = SUBp_1 - SUBp_2,
    diff_ctrl = Ctrlp_1 - Ctrlp_2,
    diff_sigstr = `Sig. Str. %p_1` - `Sig. Str. %p_2`,
    diff_clinch = `Clinch_%p_1` - `Clinch_%p_2`,
    diff_ko = `KO Ratep_1` - `KO Ratep_2`,
    diff_dec_rate = `DEC Ratep_1` - `DEC Ratep_2`
  ) %>% 
  filter(!is.na(diff_str))



# ajustamos el modelo logístico para los datos de entrenamiento

log_model_final <- glm(gana_p1 ~ diff_kd + diff_str + 
                         diff_td + diff_sub + diff_sigstr + 
                         diff_clinch + diff_ko + diff_dec_rate,
                       data = datos_entrenamiento, 
                       family = binomial)

summary(log_model_final)


# sacamos los datos específicos para los peleadores que queremos

pelea_estimar <- estats_fighters %>% 
  filter(id_peleador %in% c("e1248941344b3288",
                            "f166e93d04a8c274")) %>% 
  clean_names()

# generamos la diferencia de estadísticas entre ambos

datos_predict <- tibble(
    diff_kd = pelea_estimar$kd[1] - pelea_estimar$kd[2],
    diff_str = pelea_estimar$str[1] - pelea_estimar$str[2],
    diff_td = pelea_estimar$td[1] - pelea_estimar$td[2],
    diff_sub = pelea_estimar$sub[1] - pelea_estimar$sub[2],
    diff_sigstr = pelea_estimar$sig_str_percent[1] - pelea_estimar$sig_str_percent[2],
    diff_ko = pelea_estimar$ko_rate[1] - pelea_estimar$ko_rate[2],
    diff_clinch = pelea_estimar$clinch_percent[1] - pelea_estimar$clinch_percent[2],
    diff_dec_rate = pelea_estimar$dec_rate[1] - pelea_estimar$dec_rate[2])

prob_estimada<- predict(log_model_final, 
                      datos_predict,
                      type = "response")

print(paste0("Probabilidad de que gane Diego Lopes: ", round(prob_estimada * 100, 2), "%"))


# AUC// confusion matrix // Curva ROC
# promedio del error cuadrado





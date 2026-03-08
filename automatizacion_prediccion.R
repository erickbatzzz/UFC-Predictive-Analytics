# =============================================================================
# Proyecto: Análisis para determinar probabilidades de victoria en peleas UFC
# Fecha:                         23/01/2026
# Método:                 Regresión logística binaria
# Tarea alterna:      Automatización de resultados de victoria
# =============================================================================

# Es necesario tener instalado en el directorio de trabajo las bases de datos 
# de estadísticas de peleador e histórico de peleas, pueden encontrarse en kaggle

# ---- 1. Cargamos las librerías necesarias para ejecutar la función ----

install.packages(pacman)
library(pacman)
p_load(here, tidyverse, MASS, ciTools, janitor)

# --- 2. funciones auxiliares -----
# en construcción: vamos a crear una función auxiliar a la función principal
# para hacer más rápida la diferencia del rendimiento, además de arreglar el 
# bug de determinar las probabilidades a partir de los peleadores y no de su 
# posición

calcular_diferencias <- function(df_estats, px_1, px_2){
  
  idx_p1 <- which(df_stats$peleador == px_1)
  idx_p2 <- which(df_stats$peleador == px_2)
  
  
}



# ---- 3. función principal para la automatización ----

ufc_predict <- function(){

  peleador_1 <- readline(prompt = "Inserta el nombre del primer peleador: ")
  peleador_2 <- readline(prompt = "Inserta el nombre del segundo peleador: ")
  
  if (!exists("estats_fighters")){
    estats_fighters <<- read_csv(here("Fighters Stats.csv"),
                                 show_col_types = FALSE)
} 
  if (!exists("historico_peleas")){
    historico_peleas <<- read_csv(here("Fights.csv"), show_col_types = FALSE) 
  } 

  set.seed(123)
  
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
  
  # limpiamos los nombres sucios y seleccionamos las variables que nos interesan
  
  estats_fighters <- estats_fighters %>% 
    clean_names() %>% 
    dplyr::select("peleador","apodo","id_peleador","kd", "str",
                  "td","sub","ctrl", "sub_att","sig_str_percent",
                  "ko_rate","clinch_percent", "dec_rate")
  
  # un pequeño merge para completar la base final
  
  datos_entrenamiento <- datos_ent %>% 
    left_join(estats_fighters, by = c("id_peleador1" = "id_peleador")) %>% 
    rename_with(~paste0(., "p_1"), .cols = kd:dec_rate)
  
  datos_entrenamiento <- datos_entrenamiento %>% 
    left_join(estats_fighters, by = c("id_peleador2" = "id_peleador")) %>% 
    rename_with(~paste0(., "p_2"), .cols = kd:dec_rate)
  
  # generamos las diferencias con las que vamos a estimar los datos 
  
  datos_entrenamiento <- datos_entrenamiento %>% 
    mutate(
      gana_p1 = ifelse(resultado1 == "W", 1, 0),
      diff_kd = kdp_1 - kdp_2,
      diff_str = strp_1 - strp_2,
      diff_td = tdp_1 - tdp_2,
      diff_sub = subp_1 - subp_2,
      diff_ctrl = ctrlp_1 - ctrlp_2,
      diff_sigstr = sig_str_percentp_1 - sig_str_percentp_2,
      diff_clinch = clinch_percentp_1 - clinch_percentp_2,
      diff_ko = ko_ratep_1 - ko_ratep_2,
      diff_dec_rate = dec_ratep_1 - dec_ratep_2
    ) %>% 
    filter(!is.na(diff_str))
  
  # ajustamos el modelo logístico para los datos de entrenamiento
  
  log_model_final <- glm(gana_p1 ~ diff_kd + diff_str + 
                           diff_td + diff_sub + diff_sigstr + 
                           diff_clinch + diff_ko + diff_dec_rate,
                         data = datos_entrenamiento, 
                         family = binomial)
  

  
  # sacamos los datos específicos para los peleadores que queremos
  
  pelea_estimar <- estats_fighters %>% 
    clean_names() %>% 
    filter(peleador %in% c(as.character(peleador_1),
                           as.character(peleador_2))) %>% 
    mutate(orden = if_else(peleador == peleador_1, 1, 2)) %>% 
    arrange(orden) %>% 
    dplyr::select(-orden)
    
 
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
  cat("La probabilidad de que gane", peleador_1, "es de: ", prob_estimada * 100,"%")
  
  cat("\n=== DEBUG ===\n")
  cat("Peleador en posición 1:", pelea_estimar$peleador[1], "\n")
  cat("Peleador en posición 2:", pelea_estimar$peleador[2], "\n")
  cat("diff_str:", datos_predict$diff_str, "\n")
  cat("Probabilidad calculada:", prob_estimada * 100, "%\n")
  cat("Inversa:", (1 - prob_estimada) * 100, "%\n")
  

  
}

ufc_predict()


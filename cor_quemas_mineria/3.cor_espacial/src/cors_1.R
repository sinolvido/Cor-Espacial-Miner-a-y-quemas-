pacman::p_load(spdep, sf, argparse, dplyr, here, arrow, purrr, assertr, glue, stringr,
               stringi, janitor, readr, future, writexl, tidyverse, readxl, openxlsx, scales)

parser <- ArgumentParser()
parser$add_argument("--base",
                    default = here("/Users/emespinosa/Desktop/sin_olvido/2026/cor_quemas_mineria/2.acomodar_tabla/output/base.shp"))
parser$add_argument("--salida",
                    default = here("/Users/emespinosa/Desktop/sin_olvido/2026/cor_quemas_mineria/3.cor_espacial/output/base.shp"))
args <- parser$parse_args()


base <- st_read(args$base)

# ============================================
# PASO 1: Limpiar NAs antes de estandarizar
# ============================================
base <- base %>%
  filter(!is.na(n_quems) & !is.na(n_lcncs))

# Verificar que no hay infinitos
print(paste("NAs en n_quems:", sum(is.na(base$n_quems))))
print(paste("NAs en n_lcncs:", sum(is.na(base$n_lcncs))))

# ============================================
# PASO 2: Estandarizar (ahora sin NAs)
# ============================================
base <- base %>%
  mutate(
    n_quems_std = scale(n_quems)[,1],
    n_lcncs_std = scale(n_lcncs)[,1]
  )

# Verificar que quedó bien
print(paste("NAs en n_quems_std:", sum(is.na(base$n_quems_std))))
print(paste("NAs en n_lcncs_std:", sum(is.na(base$n_lcncs_std))))

# ============================================
# PASO 3: Crear pesos (si aún no lo hiciste)
# ============================================
library(sf)
library(spdep)

base <- base %>% st_make_valid()

vecinos <- poly2nb(base, queen = TRUE)
pesos <- nb2listw(vecinos, style = "W", zero.policy = TRUE)

# ============================================
# PASO 4: Calcular lag espacial
# ============================================
base <- base %>%
  mutate(
    lag_quems = lag.listw(pesos, n_quems_std),
    lag_lcncs = lag.listw(pesos, n_lcncs_std)
  )

# ============================================
# PASO 5: Indicador local bivariado
# ============================================
base <- base %>%
  mutate(
    LISA_quems_lag_lcncs = n_quems_std * lag_lcncs,
    LISA_lcncs_lag_quems = n_lcncs_std * lag_quems,
    indice_bivar = (LISA_quems_lag_lcncs + LISA_lcncs_lag_quems) / 2
  )

# ============================================
# PASO 6: Clasificar relación espacial
# ============================================
base <- base %>%
  mutate(
    relacion_espacial = case_when(
      LISA_quems_lag_lcncs > 0 & LISA_lcncs_lag_quems > 0 ~ 
        "Concordancia (+ +)",
      LISA_quems_lag_lcncs < 0 & LISA_lcncs_lag_quems < 0 ~ 
        "Discordancia (- -)",
      LISA_quems_lag_lcncs < 0 & LISA_lcncs_lag_quems > 0 ~ 
        "Conflictiva (Quemas+ / Lic-)",
      LISA_quems_lag_lcncs > 0 & LISA_lcncs_lag_quems < 0 ~ 
        "Conflictiva (Quemas- / Lic+)"
    )
  )

# ============================================
# PASO 7: Ver resultados
# ============================================
head(base %>% st_drop_geometry() %>%
       select(MPIO_CN, n_quems, n_lcncs, indice_bivar, relacion_espacial))

st_write(base, args$salida)


print("Distribución de relaciones:")
table(base$relacion_espacial)
pacman::p_load(spdep, sf, argparse, dplyr, here, arrow, purrr, assertr, glue, stringr,
               stringi, janitor, readr, future, writexl, tidyverse, readxl, openxlsx, scales)

parser <- ArgumentParser()
parser$add_argument("--base",
                    default = here("/Users/emespinosa/Desktop/sin_olvido/2026/cor_quemas_mineria/2.acomodar_tabla/output/base.shp"))
parser$add_argument("--salida",
                    default = here("/Users/emespinosa/Desktop/sin_olvido/2026/cor_quemas_mineria/3.cor_espacial/output/base.shp"))
args <- parser$parse_args()


base <- st_read(args$base)

# Geometrías limpias
base <- base %>% st_make_valid()

# OPCIÓN A: Eliminar NAs
base <- base %>%
  filter(!is.na(n_quems) & !is.na(n_lcncs))

# OPCIÓN B: O reemplazar por 0
# base <- base %>%
#   mutate(
#     n_quems = replace_na(n_quems, 0),
#     n_lcncs = replace_na(n_lcncs, 0)
#   )

# Crear vecinos y pesos
vecinos <- poly2nb(base, queen = TRUE)
pesos <- nb2listw(vecinos, style = "W", zero.policy = TRUE)

# Moran Tests
moran_quems <- moran.test(base$n_quems, pesos, zero.policy = TRUE)
moran_lcncs <- moran.test(base$n_lcncs, pesos, zero.policy = TRUE)

print("--- MORAN QUEMAS ---")
print(moran_quems)
print("--- MORAN LICENCIAS ---")
print(moran_lcncs)

# LISA
lisa_quems <- localmoran(base$n_quems, pesos, zero.policy = TRUE)
lisa_lcncs <- localmoran(base$n_lcncs, pesos, zero.policy = TRUE)

base <- base %>%
  mutate(
    I_quems = lisa_quems[, "Ii"],
    pval_quems = lisa_quems[, "Pr(z != E(Ii))"],
    I_lcncs = lisa_lcncs[, "Ii"],
    pval_lcncs = lisa_lcncs[, "Pr(z != E(Ii))"],
    cluster_quems = case_when(
      pval_quems > 0.05 ~ "No significativo",
      n_quems > mean(n_quems) & I_quems > 0 ~ "Hot spot (Alto-Alto)",
      n_quems < mean(n_quems) & I_quems > 0 ~ "Cold spot (Bajo-Bajo)",
      n_quems > mean(n_quems) & I_quems < 0 ~ "Outlier (Alto-Bajo)",
      n_quems < mean(n_quems) & I_quems < 0 ~ "Outlier (Bajo-Alto)"
    ),
    cluster_lcncs = case_when(
      pval_lcncs > 0.05 ~ "No significativo",
      n_lcncs > mean(n_lcncs) & I_lcncs > 0 ~ "Hot spot (Alto-Alto)",
      n_lcncs < mean(n_lcncs) & I_lcncs > 0 ~ "Cold spot (Bajo-Bajo)",
      n_lcncs > mean(n_lcncs) & I_lcncs < 0 ~ "Outlier (Alto-Bajo)",
      n_lcncs < mean(n_lcncs) & I_lcncs < 0 ~ "Outlier (Bajo-Alto)"
    )
  )

# Ver resultados
head(base %>% st_drop_geometry() %>% 
       select(MPIO_CN, n_quems, cluster_quems, n_lcncs, cluster_lcncs))


st_write(base, args$salida)

# Contar municipios por tipo de cluster
table(base$cluster_quems)
table(base$cluster_lcncs)
# Contar municipios por tipo de cluster
table(base$cluster_quems)
table(base$cluster_lcncs)
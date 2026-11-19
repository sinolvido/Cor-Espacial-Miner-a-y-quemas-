pacman::p_load(sf, argparse, dplyr, here, arrow, purrr, assertr, glue, stringr,
               stringi, janitor, readr, future, writexl, tidyverse, readxl, openxlsx, scales)

parser <- ArgumentParser()
parser$add_argument("--municipios",
                    default = here("/Users/emespinosa/Desktop/mpio/Municipios/Municipios.shp"))
parser$add_argument("--quemas",
                    default = here("/Users/emespinosa/Desktop/sin_olvido/2026/cor_quemas_mineria/1.import/input/datos_incendios.xlsx"))

parser$add_argument("--salida_quemas",
                    default = here("/Users/emespinosa/Desktop/sin_olvido/2026/cor_quemas_mineria/1.import/output/salida_quemas.xlsx"))
parser$add_argument("--salida_licencias",
                    default = here("/Users/emespinosa/Desktop/sin_olvido/2026/cor_quemas_mineria/1.import/output/salida_licencias.xlsx"))
args <- parser$parse_args()

quemas <- read.xlsx(args$salida_quemas)
licencias <- read.xlsx(args$salida_licencias)
municipios <- st_read(args$municipios)


quemas <- quemas %>%
  filter(!is.na(DPTO_CCDGO))

quemas_conteo <- quemas %>%
  group_by(DPTO_CCDGO, MPIO_CCDGO, departamento_mpio, municipio) %>%
  summarise(n_quemas = n(), .groups = "drop")



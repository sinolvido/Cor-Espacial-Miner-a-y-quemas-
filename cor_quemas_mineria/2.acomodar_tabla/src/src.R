pacman::p_load(sf, argparse, dplyr, here, arrow, purrr, assertr, glue, stringr,
               stringi, janitor, readr, future, writexl, tidyverse, readxl, openxlsx, scales)

parser <- ArgumentParser()
parser$add_argument("--licencias",
                    default = here("/Users/emespinosa/Desktop/sin_olvido/2026/cor_quemas_mineria/1.import/output/salida_licencias.xlsx"))
parser$add_argument("--quemas",
                    default = here("/Users/emespinosa/Desktop/sin_olvido/2026/cor_quemas_mineria/1.import/output/quemas.xlsx"))
parser$add_argument("--municipios",
                    default = here("/Users/emespinosa/Desktop/mpio/Municipios/Municipios.shp"))
parser$add_argument("--base",
                    default = here("/Users/emespinosa/Desktop/sin_olvido/2026/cor_quemas_mineria/2.acomodar_tabla/output/base.shp"))
args <- parser$parse_args()

quemas <- read.xlsx(args$quemas)
licencias <- read.xlsx(args$licencias)

quemas_mun <- quemas %>%
  group_by(DPTO_CCDGO, MPIO_CCDGO) %>%
  summarise(n_quemas = n())

licencias_mun <- licencias %>%
  group_by( dpto_ccdgo, mpio_ccdgo) %>%
  summarise(n_licencias = n()) %>%
  filter(!is.na(dpto_ccdgo)) %>%
  rename(MPIO_CCDGO=mpio_ccdgo) %>%
  rename(DPTO_CCDGO=dpto_ccdgo)

base <- quemas_mun  %>%
  full_join(licencias_mun)
  
municipios <- st_read(args$municipios)

base_1 <- base  %>%
  left_join(municipios)

st_write(base_1, args$base)





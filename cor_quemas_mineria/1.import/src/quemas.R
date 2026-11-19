pacman::p_load(sf, argparse, dplyr, here, arrow, purrr, assertr, glue, stringr,
               stringi, janitor, readr, future, writexl, tidyverse, readxl, openxlsx, scales)

parser <- ArgumentParser()
parser$add_argument("--quemas",
                    default = here("/Users/emespinosa/Desktop/sin_olvido/2026/cor_quemas_mineria/1.import/input/MODIS.csv"))
parser$add_argument("--quemas1",
                    default = here("/Users/emespinosa/Desktop/sin_olvido/2026/cor_quemas_mineria/1.import/input/VIIRS_NOAA20.csv"))
parser$add_argument("--quemas2",
                    default = here("/Users/emespinosa/Desktop/sin_olvido/2026/cor_quemas_mineria/1.import/input/VIIRS_NOAA21.csv"))
parser$add_argument("--quemas3",
                    default = here("/Users/emespinosa/Desktop/sin_olvido/2026/cor_quemas_mineria/1.import/input/VIIRS_SUOMI.csv"))
parser$add_argument("--municipios",
                    default = here("/Users/emespinosa/Desktop/mpio/Municipios/Municipios.shp"))
parser$add_argument("--salida",
                    default = here("/Users/emespinosa/Desktop/sin_olvido/2026/cor_quemas_mineria/1.import/output/quemas.xlsx"))
args <- parser$parse_args()

quemas <- read.csv(args$quemas)
quemas1 <- read.csv(args$quemas1)
quemas2 <- read.csv(args$quemas2)
quemas3 <- read.csv(args$quemas3)

# Convierte todas las columnas problemáticas al mismo tipo
quemas <- quemas %>% mutate(confidence = as.integer(confidence))
quemas1 <- quemas1 %>% mutate(confidence = as.integer(confidence))
quemas2 <- quemas2 %>% mutate(confidence = as.integer(confidence))
quemas3 <- quemas3 %>% mutate(confidence = as.integer(confidence))

# Ahora sí haz el join
union <- quemas %>%
  full_join(quemas1) %>%
  full_join(quemas2) %>%
  full_join(quemas3)

municipios <- st_read(args$municipios)

# 1. Convierte union a objeto sf (simple features)
quemas_sf <- union %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

# 2. Asegúrate de que municipios tenga el mismo CRS
municipios <- municipios %>% st_transform(4326)

# 3. Spatial join: cada punto de quema con su municipio
municipios <- municipios %>% st_make_valid()

# Ahora reintentar el join
quemas_con_municipio <- quemas_sf %>%
  st_join(municipios %>% 
            select(MPIO_CNMBR, DEPTO, DPTO_CCDGO, MPIO_CCDGO), 
          left = TRUE)
# 4. Convierte de vuelta a data.frame si lo necesitas
resultado <- quemas_con_municipio %>%
  st_drop_geometry() %>%  # Elimina la geometría
  as_tibble()

resultado <- resultado %>%
  filter(!is.na(MPIO_CCDGO))

write.xlsx(resultado, args$salida)



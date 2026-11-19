pacman::p_load(sf, argparse, dplyr, here, arrow, purrr, assertr, glue, stringr,
               stringi, janitor, readr, future, writexl, tidyverse, readxl, openxlsx, scales)

parser <- ArgumentParser()
parser$add_argument("--municipios",
                    default = here("/Users/emespinosa/Desktop/mpio/Municipios/Municipios.shp"))
parser$add_argument("--quemas",
                    default = here("/Users/emespinosa/Desktop/sin_olvido/2026/cor_quemas_mineria/1.import/input/datos_incendios.xlsx"))
parser$add_argument("--licencias",
                    default = here("/Users/emespinosa/Desktop/sin_olvido/2026/cor_quemas_mineria/1.import/input/solicitudes.gpkg"))
parser$add_argument("--salida_quemas",
                    default = here("/Users/emespinosa/Desktop/sin_olvido/2026/cor_quemas_mineria/1.import/output/salida_quemas.xlsx"))
parser$add_argument("--salida_licencias",
                    default = here("/Users/emespinosa/Desktop/sin_olvido/2026/cor_quemas_mineria/1.import/output/salida_licencias.xlsx"))
args <- parser$parse_args()

quemas <- read.xlsx(args$quemas)
licencias <- st_read(args$licencias)
municipios <- st_read(args$municipios)

# ---------------------------------------------------------------------
# 0. Corrige topología de municipios ANTES de cualquier unión espacial
# ---------------------------------------------------------------------
sum(!st_is_valid(municipios))          # diagnóstico: cuántas geometrías inválidas
municipios <- st_make_valid(municipios)
stopifnot(all(st_is_valid(municipios))) # aborta si aún queda alguna inválida

# ---------------------------------------------------------------------
# 1. Limpieza de columnas duplicadas "lat"
# ---------------------------------------------------------------------
colnames(quemas)[colnames(quemas) == "lat"] <- make.unique(colnames(quemas)[colnames(quemas) == "lat"])
quemas <- quemas %>%
  select(-lat.1, -lat.2, -lat.3)

quemas$lon <- as.numeric(quemas$lon)
quemas$lat <- as.numeric(quemas$lat)

# ---------------------------------------------------------------------
# 2. Corrige la escala de las coordenadas (divide por 100.000)
# ---------------------------------------------------------------------
quemas <- quemas %>%
  mutate(
    lon_dd = lon / 1e5,
    lat_dd = lat / 1e5
  )

# Diagnóstico rápido de rango esperado para Colombia (lon ~ -82 a -66, lat ~ -4 a 13)
summary(quemas$lon_dd)
summary(quemas$lat_dd)

# ---------------------------------------------------------------------
# 3. Convierte quemas a objeto espacial (puntos)
# ---------------------------------------------------------------------
quemas_sf <- st_as_sf(quemas, coords = c("lon_dd", "lat_dd"), crs = 4326, remove = FALSE)

# ---------------------------------------------------------------------
# 4. Verifica CRS y homogeniza si es necesario
# ---------------------------------------------------------------------
if (st_crs(quemas_sf) != st_crs(municipios)) {
  municipios <- st_transform(municipios, st_crs(quemas_sf))
}
stopifnot(st_crs(quemas_sf) == st_crs(municipios))

# ---------------------------------------------------------------------
# 5. Unión espacial principal (st_intersects: más robusto que st_within
#    para puntos justo en el límite de un municipio)
# ---------------------------------------------------------------------
quemas_join <- st_join(quemas_sf, municipios %>% select(DEPTO, MPIO_CNMBR, DPTO_CCDGO,MPIO_CCDGO ), join = st_intersects) %>%
  rename(departamento_mpio = DEPTO, municipio = MPIO_CNMBR)

n_na_inicial <- sum(is.na(quemas_join$municipio))
message(glue("Registros sin municipio tras st_intersects: {n_na_inicial}"))

# ---------------------------------------------------------------------
# 6. Rescate por vecino más cercano para los que sigan en NA
#    (útil para puntos costeros / justo fuera del polígono por precisión)
# ---------------------------------------------------------------------
if (n_na_inicial > 0) {
  
  pendientes_idx <- which(is.na(quemas_join$municipio))
  pendientes <- quemas_join[pendientes_idx, ]
  
  nearest_idx <- st_nearest_feature(pendientes, municipios)
  dist_m <- st_distance(pendientes, municipios[nearest_idx, ], by_element = TRUE)
  
  message(glue("Distancia al municipio más cercano (m) — resumen:"))
  print(summary(as.numeric(dist_m)))
  
  # Umbral de tolerancia: ajusta según tu contexto (ej. 2000 m)
  umbral_m <- 2000
  
  asignar <- as.numeric(dist_m) <= umbral_m
  
  quemas_join$departamento_mpio[pendientes_idx[asignar]] <- municipios$DEPTO[nearest_idx[asignar]]
  quemas_join$municipio[pendientes_idx[asignar]]         <- municipios$MPIO_CN[nearest_idx[asignar]]
  
  message(glue("Registros rescatados dentro de {umbral_m} m: {sum(asignar)}"))
  message(glue("Registros que permanecen en NA (posible dato con coordenada inválida): {sum(!asignar)}"))
}

# ---------------------------------------------------------------------
# 7. Resultado final
# ---------------------------------------------------------------------
quemas_final <- st_drop_geometry(quemas_join)


write.xlsx(quemas_final, args$salida_quemas)

# lisencias 


# ---------------------------------------------------------------------
# 1. Limpieza de columnas duplicadas "lat"
# ---------------------------------------------------------------------
colnames(licencias)[colnames(licencias) == "lat"] <- make.unique(colnames(quemas)[colnames(quemas) == "lat"])
quemas <- quemas %>%
  select(-lat.1, -lat.2, -lat.3)

quemas$lon <- as.numeric(quemas$lon)
quemas$lat <- as.numeric(quemas$lat)

# ---------------------------------------------------------------------
# 2. Corrige la escala de las coordenadas (divide por 100.000)
# ---------------------------------------------------------------------
quemas <- quemas %>%
  mutate(
    lon_dd = lon / 1e5,
    lat_dd = lat / 1e5
  )

# Diagnóstico rápido de rango esperado para Colombia (lon ~ -82 a -66, lat ~ -4 a 13)
summary(quemas$lon_dd)
summary(quemas$lat_dd)

# ---------------------------------------------------------------------
# 3. Convierte quemas a objeto espacial (puntos)
# ---------------------------------------------------------------------
quemas_sf <- st_as_sf(quemas, coords = c("lon_dd", "lat_dd"), crs = 4326, remove = FALSE)

# ---------------------------------------------------------------------
# 4. Verifica CRS y homogeniza si es necesario
# ---------------------------------------------------------------------
if (st_crs(quemas_sf) != st_crs(municipios)) {
  municipios <- st_transform(municipios, st_crs(quemas_sf))
}
stopifnot(st_crs(quemas_sf) == st_crs(municipios))

# ---------------------------------------------------------------------
# 5. Unión espacial principal (st_intersects: más robusto que st_within
#    para puntos justo en el límite de un municipio)
# ---------------------------------------------------------------------
quemas_join <- st_join(quemas_sf, municipios %>% select(DEPTO, MPIO_CNMBR, DPTO_CCDGO,MPIO_CCDGO ), join = st_intersects) %>%
  rename(departamento_mpio = DEPTO, municipio = MPIO_CNMBR)

n_na_inicial <- sum(is.na(quemas_join$municipio))
message(glue("Registros sin municipio tras st_intersects: {n_na_inicial}"))

# ---------------------------------------------------------------------
# 6. Rescate por vecino más cercano para los que sigan en NA
#    (útil para puntos costeros / justo fuera del polígono por precisión)
# ---------------------------------------------------------------------
if (n_na_inicial > 0) {
  
  pendientes_idx <- which(is.na(quemas_join$municipio))
  pendientes <- quemas_join[pendientes_idx, ]
  
  nearest_idx <- st_nearest_feature(pendientes, municipios)
  dist_m <- st_distance(pendientes, municipios[nearest_idx, ], by_element = TRUE)
  
  message(glue("Distancia al municipio más cercano (m) — resumen:"))
  print(summary(as.numeric(dist_m)))
  
  # Umbral de tolerancia: ajusta según tu contexto (ej. 2000 m)
  umbral_m <- 2000
  
  asignar <- as.numeric(dist_m) <= umbral_m
  
  quemas_join$departamento_mpio[pendientes_idx[asignar]] <- municipios$DEPTO[nearest_idx[asignar]]
  quemas_join$municipio[pendientes_idx[asignar]]         <- municipios$MPIO_CN[nearest_idx[asignar]]
  
  message(glue("Registros rescatados dentro de {umbral_m} m: {sum(asignar)}"))
  message(glue("Registros que permanecen en NA (posible dato con coordenada inválida): {sum(!asignar)}"))
}

# ---------------------------------------------------------------------
# 7. Resultado final
# ---------------------------------------------------------------------
quemas_final <- st_drop_geometry(quemas_join)


write.xlsx(quemas_final, args$salida_quemas)

### lisencias 


# 1. licencias viene como MULTISURFACE (CURVEPOLYGON) -> conviértela a
#    MULTIPOLYGON estándar antes de cualquier operación espacial
licencias <- licencias %>% st_cast("MULTIPOLYGON")

# 2. Corrige geometrías inválidas en ambas capas
licencias  <- st_make_valid(licencias)
municipios <- st_make_valid(municipios)

# 3. Lleva ambas capas a un CRS proyectado en metros (MAGNA-SIRGAS
#    Origen-Nacional) para que el cálculo de áreas de traslape sea correcto
licencias_proj  <- st_transform(licencias, 9377)
municipios_proj <- st_transform(municipios, 9377)

# 3b. IMPORTANTE: revalida DESPUÉS de proyectar. La reproyección puede
#     reintroducir invalidez (micro-autointersecciones) aunque la
#     geometría original ya estuviera "válida" antes del transform.
licencias_proj  <- st_make_valid(licencias_proj)
municipios_proj <- st_make_valid(municipios_proj)

sum(!st_is_valid(licencias_proj))
sum(!st_is_valid(municipios_proj))

# 3c. Si AÚN quedan inválidas (o si vuelve a salir el TopologyException al
#     intersectar), fuerza un ajuste de precisión geométrica: esto "ancla"
#     los vértices a una grilla y elimina duplicados casi-coincidentes que
#     confunden a GEOS. 1 = precisión de 1 metro (suficiente para esto).
licencias_proj  <- st_set_precision(licencias_proj, 1)  %>% st_make_valid()
municipios_proj <- st_set_precision(municipios_proj, 1) %>% st_make_valid()

# 4. Identificador temporal para no perder el orden original
licencias_proj <- licencias_proj %>% mutate(.id_licencia = row_number())

# 5. Intersección: una licencia puede cruzar varios municipios
interseccion <- st_intersection(
  licencias_proj,
  municipios_proj %>% select(DEPTO, MPIO_CNMBR, DPTO_CCDGO, MPIO_CCDGO)
) %>%
  mutate(area_traslape = as.numeric(st_area(.)))

# 6. Para cada licencia, se queda con el municipio de mayor traslape
mejor_municipio <- interseccion %>%
  st_drop_geometry() %>%
  group_by(.id_licencia) %>%
  slice_max(area_traslape, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(.id_licencia,
         departamento = DEPTO,
         municipio    = MPIO_CNMBR,
         dpto_ccdgo   = DPTO_CCDGO,
         mpio_ccdgo   = MPIO_CCDGO)

# 7. Une el resultado a la capa original de licencias
licencias <- licencias_proj %>%
  left_join(mejor_municipio, by = ".id_licencia") %>%
  select(-.id_licencia) %>%
  st_transform(st_crs(licencias))   # vuelve al CRS original (MAGNA-SIRGAS)

# 8. Verificación
sum(is.na(licencias$municipio))   # licencias sin municipio asignado
head(st_drop_geometry(licencias))

licencias_tabla <- st_drop_geometry(licencias)
write.xlsx(licencias_tabla, args$salida_licencias)



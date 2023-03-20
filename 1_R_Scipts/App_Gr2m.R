# =========================================================================
# SCRIPT DESARROLLADO PARA EL CURSO DE PROGRAMACION DE R EN HIDROLOGIA
# APLICANDO EL MODELO HIDROLOGICO AGREGADO GR2M 
# EN UNA CUENCA PERUANA
# =========================================================================

# Instalar y cargar librerias necesarias ----------------------------------

install.packages('tidyverse')
install.packages('airGR')
install.packages('raster')
install.packages('ncdf4')
install.packages('rgeos')
install.packages('SPEI')
install.packages('hydroTSM')

library(tidyverse)
library(airGR)
library(raster)
library(ncdf4)
library(rgeos)
library(SPEI)
library(hydroTSM)

# Read shapefile basin

Shp_basin <- shapefile('2_Data/Mantaro_mejorada.shp')

Pisco_pp <- raster::brick('/Volumes/ADATA_2TB/Nueva carpeta/PISCOpd_v2.1.nc')
Pisco_tx <- raster::brick('/Volumes/ADATA_2TB/Nueva carpeta/PISCOdtx_v1.1.nc')
Pisco_tn <- raster::brick('/Volumes/ADATA_2TB/Nueva carpeta/PISCOdtn_v1.1.nc')

# Plot data

plot(Pisco_tn[[1]])
plot(Shp_basin, add = T, col = 'red')

# Vector date

Dates <- seq.Date(from = as.Date('1981-01-01'),
                  to = as.Date('2016-12-31'),
                  by = 'day')

# Extract Precipitation data

pp <- c(t(raster::extract(x = Pisco_pp,
                          y = Shp_basin,
                          fun = mean,
                          small = TRUE)))

pp <- zoo(x = pp, order.by = Dates)

pp <- hydroTSM::daily2monthly(x = pp, FUN = sum)

# Extract temperatures

# Max

tx <- c(t(raster::extract(x = Pisco_tx,
                          y = Shp_basin,
                          fun = mean,
                          small = TRUE)))

tx <- zoo(x = tx, order.by = Dates)

tx <- hydroTSM::daily2monthly(x = tx, FUN = mean)

# Min

tn <- c(t(raster::extract(x = Pisco_tn,
                          y = Shp_basin,
                          fun = mean,
                          small = TRUE)))

tn <- zoo(x = tn, order.by = Dates)

tn <- hydroTSM::daily2monthly(x = tn, FUN = mean)

# Obteniendo el centroide de la cuenca

cent <- rgeos::gCentroid(spgeom = Shp_basin, byid = TRUE)

Pet <- SPEI::hargreaves(Tmin = tn, Tmax = tx, 
                        lat = cent@coords[2], na.rm = T)






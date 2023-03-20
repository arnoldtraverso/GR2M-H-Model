# =========================================================================
# SCRIPT DESARROLLADO PARA EL CURSO DE PROGRAMACION DE R EN HIDROLOGIA
# APLICANDO EL MODELO HIDROLOGICO AGREGADO GR2M 
# EN UNA CUENCA PERUANA
# =========================================================================

library(airGR)
library(raster)
library(zoo)
library(lubridate)

# Ingreso de shapefile

Shp_basin <- shapefile(x = '2_Data/Mantaro_mejorada.shp')

# Ingreso de datos de la cuenca

Data_Mantaro <- read.zoo(file = '2_Data/Data_Mantaro.csv', header = T, sep = ',')
Qobs <- read.zoo(file = '2_Data/QObs_LaMejorada.csv', header = T, sep = ';')

# Llevando los datos de Q m3 a mm

Qobs_mm <- Qobs*days_in_month(Qobs)*24*3600 / (area(Shp_basin) / 1000)

# Ordenando los datos de cuenca

Data_Mantaro <- window(Data_Mantaro, start = '1981-01-01', end = '2005-12-01')

# Nueva base de Mantaro

BasinObs <- data.frame(Date = as.POSIXlt(index(Data_Mantaro), 
                                         format = '%Y-%m-%d'), 
                       Data_Mantaro,
                       Qobs_mm = as.numeric(Qobs_mm))

# Ingreso de data para el modelo GR

InputsModel <- CreateInputsModel(FUN_MOD = RunModel_GR2M, 
                                 DatesR = BasinObs$Date, 
                                 Precip = BasinObs$Pcp,
                                 PotEvap = BasinObs$Pet)

# Periodo de calentamiento 
# Se recomienda dos años de calentamiento

Ind_Wup <- seq(which(format(BasinObs$Date, format = "%Y-%m") == "1981-01"),
               which(format(BasinObs$Date, format = "%Y-%m") == "1982-12"))

Ind_Run <- seq(which(format(BasinObs$Date, format = "%Y-%m") == "1983-01"),
               which(format(BasinObs$Date, format = "%Y-%m") == "1990-12"))

# Simulacion Base

RunOptions <- CreateRunOptions(FUN_MOD = RunModel_GR2M,
                               InputsModel = InputsModel,
                               IndPeriod_WarmUp = Ind_Wup, 
                               IndPeriod_Run = Ind_Run)

Param <- c(265.07, 1.04) # Parametros aleatorios

OutputsModel <- RunModel_GR2M(InputsModel = InputsModel, 
                              RunOptions = RunOptions,
                              Param = Param)

# Graficando el modelo base

plot(OutputsModel, Qobs = BasinObs$Qobs_mm[Ind_Run])

# =========================================================================
# Calibracion del modelo GR2M
# =========================================================================

# Criterio de calibracion

CalibOptions <- CreateCalibOptions(FUN_MOD = RunModel_GR2M, 
                                   FUN_CALIB = Calibration_Michel)

# Ingreso de parametros

InputsCrit <- CreateInputsCrit(FUN_CRIT = ErrorCrit_NSE, 
                               InputsModel = InputsModel,
                               RunOptions = RunOptions,
                               Obs = BasinObs$Qobs_mm[Ind_Run])

# Encontrando los parametros calibrados

OutputsCalib <- Calibration(InputsModel = InputsModel, 
                            RunOptions = RunOptions,
                            InputsCrit = InputsCrit,
                            CalibOptions = CalibOptions,
                            FUN_MOD = RunModel_GR2M,
                            FUN_CALIB = Calibration_Michel)

# Seleccionando los parametros

Param_C <- OutputsCalib$ParamFinalR
Param_C

# Ejecutando el modelo con los parametros encontrados

OutputsModel2 <- RunModel(InputsModel = InputsModel, 
                         RunOptions = RunOptions,
                         Param = Param_C, 
                         FUN = RunModel_GR2M)

# Graficando el modelo calibrado

plot(OutputsModel2, Qobs = BasinObs$Qobs_mm[Ind_Run])


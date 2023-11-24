# SERIES DE TIEMPO ORDENADAS Y PRONOSTICOS EN R

# instalar librerías
library(fpp3)
library(lubridate)
library(urca)
library(loo)

##################  INTRODUCCIÓN SERIES DE TIEMPO ###############
# 1. EXPLORACIÓN DE LA BASE ##########################

# creación de una tsibble 
ts <- tsibble(
  year  = 2012:2016,
  y = c(1,2,3,4,5),
  index = year
)

# base de datos us_employment
bd <- us_employment

# visualización de la bd (TSIBBLE)
head(bd)

# tipos de fechas
# yearmonth
# year
# yearweek
bd %>% 
  mutate(fecha = year(Month))

# sector RETAIL
retail <- bd %>% 
  filter(Title == "Retail Trade", year(Month) >= 1980)
  

# 2. PLOTS TEMPORALES ####################################
# autoplot
autoplot(retail)

# gg_season: period
gg_season(retail)

# gg_subseries
retail %>% 
  gg_subseries(Employed)

# ejemplo tipos de gg_season:
aus_production %>% 
  select(Quarter, Beer) %>% 
  filter(year(Quarter)>= 1990) %>% 
  gg_subseries(Beer)

# 3. DESCOMPOSICION Y ANALISIS DE UNA SERIE DE TIEMPO ###############

# MÉTODO STL
ts_decomposed <- retail %>%
  model(stl = STL(Employed ~ season(window = 13), robust = TRUE))

# visualización de componentes
components(ts_decomposed) %>% 
  autoplot()

# Análisis de Autocorrelación (ACF)
retail %>% 
  ACF(Employed, lag_max = 48) %>% 
  autoplot()

# grafico de autocorrelación parcial
retail %>%
  ACF(Employed, lag_max = 48, type = "partial") %>%
  autoplot()


# 4. FORECASTING ##########################
# AJUSTAR MODELOS
model_fit <- retail %>%
filter(!is.na(Employed)) %>%
model(
  `Seasonal_naïve` = SNAIVE(Employed),
  `Naïve` = NAIVE(Employed),
  Drift = RW(Employed ~ drift()),
  Mean = MEAN(Employed)
)

# pronostico
forecast_model <- model_fit %>%
  forecast(h=12) 

# visualizacion
forecast_v <- forecast_model %>% 
  autoplot(retail, level = NULL) +
  guides(colour = guide_legend(title = "Forecast"))

# intervalos de tiempo
forecast_i <- forecast_model %>% hilo(level = 95)

# residuales 
retail <- retail %>%  
  mutate(test = row_number()) %>% 
  update_tsibble(index = test, regular = TRUE)

# eligiendo solo un modelo
naive_fit <- retail %>%  model(NAIVE(Employed))
augment(naive_fit) %>% 
  ggplot(aes(x = test)) +
  geom_line(aes(y = Employed, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted"))

# grafico de errores
naive_fit %>%  
  gg_tsresiduals()

# ACCURACY

model_fit %>% 
  accuracy()


# 5. INTRODUCCIÓN MODELO ARIMA #############################
# Serie estacionaria
autoplot(retail)

# ACF de la diferencia 
retail %>% 
  ACF(difference(Employed)) %>% 
  autoplot()

# Prueba de Dickey-Fuller para estacionariedad
prueba <- ur.df(retail$Employed, lags = 20, type = "trend")

summary(prueba)

# modelo ARIMA
arima_model <- retail %>%
  model(ARIMA(Employed))

# accuracy del modelo
arima_model %>% 
  accuracy()

fit_arima <- arima_model %>% 
  report()

# Forecast
fit_arima %>%
  forecast(h=5) %>% 
  autoplot(retail)


  
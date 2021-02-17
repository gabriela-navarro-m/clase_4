# Elaborado por: Eduard Martinez
# Colaboradores:
# Fecha de elaboracion: 12/02/2020
# Ultima modificacion: 16/02/2021

# intial configuration
rm(list = ls()) # limpia el entorno de R
pacman::p_load(tidyverse,readxl,haven) # cargar y/o instalar paquetes a usar

# tydyverse
browseURL(url = "https://www.tidyverse.org", browser = getOption("browser"))

#----------------#
# 1. Data Tyding #
#----------------#

#### 1.1 Cargar bases de datos
browseURL(url = "https://www.dane.gov.co/index.php/estadisticas-por-tema/demografia-y-poblacion/proyecciones-de-poblacion", browser = getOption("browser")) # Fuente: DANE
dane = readRDS(file = 'data/input/proyecciones DANE.rds') %>% # Explicar para que sirve pipe
       dplyr::select(.,name_codigo , year , total_poblacion , codigo)

#### 1.2 Generar variables en un dataframe
dane$dummy = 1

dane = mutate(dane , colombia = 1 ) 

#### 1.2.1 Generar variable usando el codigo dane
nchar('Hola')

dane = mutate(dane , depto = ifelse(test = nchar(codigo) == 2 , yes = 1 , no = 0)) # Crear variable usando mutate

dane$codigo # vector 
dane$mpio = ifelse(test =  nchar(dane$codigo) > 2 , yes = 1, no = 0) # Crear variable usando vector

#### 1.2.2 Rellenar con el nombre del municipio
dane_mpio = subset(dane , mpio == 1)

substr(x = 'Hola' , start = 2, stop = 4) # Veamos la funcion substr()
str_locate(string = "Hola - todos" ,pattern = '-') # Veamos la funcion str_locate()

dane_mpio = mutate(dane_mpio , name = substr(x =  name_codigo ,start = 1 , stop =  str_locate(string = name_codigo,pattern = '-')))

dane_mpio = dane_mpio %>% group_by(codigo) %>% fill(name, .direction = "down") # default rellena con el valor anterior

#### 1.2.3 Limpiar el nombre del municipio
gsub(pattern = "ol",replacement = "-",x = "Hola") # Veamos la funcion gsub()

dane_mpio = mutate(dane_mpio , name = gsub(pattern = " -",replacement = "" ,x = name))

#### 1.3 Exportar la base de datos
dane_mpio = dane_mpio[,c('codigo','name','year','total_poblacion')]
saveRDS(object = dane_mpio , file = "data/output/proyecciones DANE.rds")



#--------------------------------#
# 2.Filtrar (filas y/o columnas) # 
#--------------------------------#

#### 2.0. cargar bases de datos
browseURL(url = "https://www.policia.gov.co/grupo-información-criminalidad/estadistica-delictiva", browser = getOption("browser")) # Fuente: DANE
data = read_excel("data/input/homicidios-2020.xlsx")

#### 2.1 Seleccionar columnas de un dataframe usando los nombre de las variables
colnames(data) # Inspeccionar los nombres de las variables

data_1 = dplyr::select(data , municipio , `codigo dane` , mes , cantidad) # usar `` cuando hay espacios en los nombres

data_2 = dplyr::select(data , -departamento) # Anteponer el - cuando quiero eliminar una variable

data_3 = data[,c('municipio' , 'codigo dane' , 'mes' , 'cantidad')] # Usando el vector de los nombres

#### 2.1.2 Seleccionar columnas de un dataframe usando la posicion de las columnas
colnames(data)

colnames(data)[c(2,3,9)]

data_rdata_3 = data[,c(2,3,9)] # Usando el vector de los nombres

#### 2.1.2.1 Veamos la funcion grep
grep(pattern = 'la' , x = c('Hola','Pola','Nada','Todo'))

grep(pattern = 'municipio' , x = colnames(data))

#### Task crear un vector con la posicion de 'codigo dane' y 'cantidad' usando la funcion grep 
nombres = "?"
data_4 = data[,nombres]


#### 2.2 Filtrar filas de un dataframe
rm(list = ls())
df = readRDS("data/output/proyecciones DANE.rds")

# Vamos a sellecionar solo algunas columnas del dataframe data
colnames(df) = c('cod_dane','name_muni','year','poblacion')
df

#### 2.2.1 Usando la posicion de las filas
df_1a = df[1:953,]

nrow(df)
df_1b = df[17000:nrow(df),]

#### 2.2.2 Usando los atributos de la variable
df_2 = subset(x = df, subset = is.na(name_muni) == F)

df_3 = subset(x = df, subset =  poblacion > 100000)

df_4 = subset(x = df, subset = is.na(name_muni) == F  & as.numeric(poblacion) > 100000) # coombinando las opciones

df_5a = df[df$poblacion > 100000,]

df_5b = dplyr::filter(df, poblacion > 100000)

#### 2.2.3 Usando un vector

df_6a = dplyr::filter(df, cod_dane %in% c('05001','08001','13001'))

df_6b = subset(x = df, cod_dane %in% c('05001','08001','13001'))

df_6c = df[df$cod_dane %in% c('05001','08001','13001') , ]

#----------------#
# 3. Fechas en R #
#----------------#

#### Limpiar el entorno
rm(list = ls())

#### 3.0. Funciones para inspeccionar
is.Date <- function(x) inherits(x, "Date")
is.POSIXct <- function(x) inherits(x, "POSIXct")

#### 3.1. Cargar la base de datos
browseURL(url = "https://www.policia.gov.co/grupo-información-criminalidad/estadistica-delictiva", browser = getOption("browser")) # Fuente de los datos
siedco = readRDS(file = "data/input/SIEDCO.rds")
str(siedco)

#### 3.2 Inspeccionemos los datos
is.character(siedco$fecha)
is.POSIXct(siedco$fecha_1)
is.Date(siedco$fecha_2)

#### 3.3 Convertir de character a fecha
siedco  = siedco %>% mutate(fecha_3 = as.POSIXct(fecha, format = "%Y-%m-%d"), # De Character a POSIXct
                            fecha_4 = as.Date(fecha, "%Y-%m-%d")) # De Character a Date
str(siedco)

#### 3.4 Convertir de fecha a character
siedco  = siedco %>% mutate(fecha_5 = as.character(fecha_1, format = "%d-%m-%Y"), # De POSIXct a Character
                            fecha_6 = as.character(fecha_2, format = "%Y-%m-%d")) # De Date a Character
is.character(siedco$fecha_5)
is.character(siedco$fecha_6)

### 3.5 Extraer el year y el mes de una variable de fecha
siedco  = siedco %>% mutate(year_month = format(as.Date(fecha_1), "%Y-%m"), # Extraer el mes y el year
                            year = format(as.Date(fecha_1), "%Y"),
                            month = format(as.Date(fecha_1), "%m")) # Extraer el year

#### 3.6 Operaciones entre fechas
as.Date(x = "2020-09-09","%Y-%m-%d") %>% as.numeric()
as.Date(x = "2020-09-10","%Y-%m-%d") %>% as.numeric()
siedco  = siedco %>% mutate(diferencia = 18514 - as.numeric(fecha_2))





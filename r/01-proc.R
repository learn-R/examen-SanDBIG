# Cargar Paquetes ---------------------------------------------------------

pacman::p_load(tidyverse,
               haven,
               dplyr,
               magrittr,
               sjmisc, #Para explorar datos
               sjPlot, #Para los gráficos
               ggpubr, #Para incorporar correlación en scatterplot (grid)
               grid, #Para incorporar correlación en scatterplot
               ggrepel, #Para evitar solapamientos en gráfico
               forcats,
               car,
               knitr,
               srvyr,
               sjlabelled,
               knitr,
               srvyr)

# Cargar Datos -------------------------------------------------------------
septiembre_2022 <- read.csv("input/data/Septiembre-2022.csv", sep=";", 
                            encoding = "UTF-8", stringsAsFactors = F) # se agrega sep=";" ya que las columnas van separadas con ese caracter



# Procesamiento -----------------------------------------------------------

#omitir n.a
na.omit(septiembre_2022) #omitted 16436 rows
septiembre_2022 <- na.omit(septiembre_2022)

datos_proc <- select(septiembre_2022,
                     id,
                     tamano,
                     categoria,
                     sexo,
                     grupo,
                     ro_t_1)

#renombrar tamano
datos_proc <- datos_proc %>% 
  mutate(tamano =
           case_when(tamano %in% c(1) ~ "Pequeña",
                     tamano %in% c(2) ~ "Mediana",
                     tamano %in% c(3) ~ "Grande",
                     TRUE ~ NA_character_)) 

#renombrar categoria
datos_proc <- datos_proc %>% 
  mutate(categoria =
           case_when(categoria == "B"~"Mineria",
                     categoria == "C"~"Industria_Manufacturera",
                     categoria == "D"~"Suministro_de_Electricidad,Gas,Vapor_y_Aire_Acondicionado",
                     categoria == "E"~"Suministro_de_Agua,Evacuación_de_Aguas_Residuales,Gestión_de_desechos_y_Descontaminación",
                     categoria == "F"~"Construcción",
                     categoria == "G"~"Comercio",
                     categoria == "H"~"Transporte_y_Almacenamiento",
                     categoria == "I"~"Actividades_de_Alojamiento_y_Servicio_de_Comidas",
                     categoria == "J"~"Información_y_Comunicaciones",
                     categoria == "K"~"Actividades_Financieras_y_de_Seguros",
                     categoria == "L"~"Actividades_Inmobiliarias",
                     categoria == "M"~"Actividades_Profesionales,Cientificas_y_Técnicas",
                     categoria == "N"~"Actividades_de_Servicios_Administrativos_y_de_Apoyo",
                     categoria == "O"~"Administración_Pública",
                     categoria == "P"~"Enseñanza",
                     categoria == "Q"~"Actividades_de_Atención_de_la_Salud_Humana_y_de_Asistencia_Social",
                     categoria == "R"~"Actividades_Artísticas,de_Entretenimiento_y_Recreativas",
                     TRUE ~ NA_character_))

#renombrar sexo
datos_proc <- datos_proc %>% 
  mutate(sexo =
           case_when(sexo == "1"~"Mujer",
                     sexo == "2"~"Hombre",
                     TRUE ~ NA_character_))

#renombrar grupo
datos_proc <- datos_proc %>% 
  mutate(grupo =
           case_when(grupo == "1"~"Directores y Gerentes",
                     grupo == "2"~"Profesionales",
                     grupo == "3"~"Tecnicos",
                     grupo == "4"~"Trabajadores de apoyo administrativo",
                     grupo == "5"~"Trabajadores de servicios personales, proteccion y seguridad",
                     grupo == "6"~"Vendedores, promotores y modelos",
                     grupo == "7"~"Operarios manuales y artesanos",
                     grupo == "8"~"Operadores y montadores de instalaciones y maquinas",
                     grupo == "9"~"Trabajadores no especializados",
                     TRUE ~ NA_character_))

#renombrar ro_t_1
datos_proc <- datos_proc %>% 
  rename(remuneracion_ordinaria= ro_t_1 )



#Cambio a numeric y character
datos_proc <- datos_proc %>% 
  mutate_at(vars(id,remuneracion_ordinaria), funs(as_numeric(.))) %>% 
  mutate_at(vars(tamano,categoria,sexo,grupo), funs(as.character(.)))



#objeto exp
objeto_exp  <- datos_proc %>% 
  as_survey_design(ids = id, weights = remuneracion_ordinaria )

#Modelo Lm
modelo1 <- lm(grupo ~ id,
              data = objeto_exp, 
              weights = remuneracion_ordinaria)


#remuneracion y grupo

modelo <- select(datos_proc,
                 grupo,
                 remuneracion_ordinaria)

plot(modelo)


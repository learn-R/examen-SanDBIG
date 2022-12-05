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



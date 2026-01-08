# 0. Cargar paquetes ----

library(dplyr)
library(tidyverse)
library(haven)
library(foreign)
library(summarytools)

#1. Cargar bbdd ----

latbaro_2002 <- read_dta("C:/Users/jpdia/OneDrive/Documents/GitHub/tesis-final/tesis-pregrado/presentations/examen_titulo/data/latbaro_2002_stata.dta")

latbaro_2004 <- read_dta("C:/Users/jpdia/OneDrive/Documents/GitHub/tesis-final/tesis-pregrado/presentations/examen_titulo/data/latbaro_2004_stata.dta")

latbaro_2006 <- read_dta("C:/Users/jpdia/OneDrive/Documents/GitHub/tesis-final/tesis-pregrado/presentations/examen_titulo/data/latbaro_2006_stata.dta")

latbaro_2008 <- read_dta("C:/Users/jpdia/OneDrive/Documents/GitHub/tesis-final/tesis-pregrado/presentations/examen_titulo/data/latbaro_2008_stata.dta")

latbaro_2010 <- read_dta("C:/Users/jpdia/OneDrive/Documents/GitHub/tesis-final/tesis-pregrado/presentations/examen_titulo/data/latbaro_2010_stata.dta")

latbaro_2013 <- read_dta("C:/Users/jpdia/OneDrive/Documents/GitHub/tesis-final/tesis-pregrado/presentations/examen_titulo/data/latbaro_2013_stata.dta")

latbaro_2015 <- read_dta("C:/Users/jpdia/OneDrive/Documents/GitHub/tesis-final/tesis-pregrado/presentations/examen_titulo/data/latbaro_2015_stata.dta")

latbaro_2017 <- read_dta("C:/Users/jpdia/OneDrive/Documents/GitHub/tesis-final/tesis-pregrado/presentations/examen_titulo/data/latbaro_2017_stata.dta")

latbaro_2020 <- read_dta("C:/Users/jpdia/OneDrive/Documents/GitHub/tesis-final/tesis-pregrado/presentations/examen_titulo/data/latbaro_2020_stata.dta")

latbaro_2024 <- read_dta("C:/Users/jpdia/OneDrive/Documents/GitHub/tesis-final/tesis-pregrado/presentations/examen_titulo/data/latbaro_2024_stata.dta")

latbaro_all <- read_dta("C:/Users/jpdia/OneDrive/Documents/GitHub/tesis-final/tesis-pregrado/presentations/examen_titulo/data/latbaro_glob.dta")

#2. Comprobar año y país ----

freq(latbaro_2002$numinves)
freq(latbaro_2002$idenpa)    

freq(latbaro_2004$numinves)
freq(latbaro_2004$idenpa)  

freq(latbaro_2006$numinves)
freq(latbaro_2006$idenpa)  

freq(latbaro_2008$numinves)
freq(latbaro_2008$idenpa)  

freq(latbaro_2010$NUMINVES)
freq(latbaro_2010$IDENPA)  

freq(latbaro_2013$NUMINVES)
freq(latbaro_2013$IDENPA)  

freq(latbaro_2015$NUMINVES)
freq(latbaro_2015$IDENPA)  

freq(latbaro_2017$NUMINVES)
freq(latbaro_2017$IDENPA)  

freq(latbaro_2020$NUMINVES)
freq(latbaro_2020$IDENPA) 

freq(latbaro_2024$NUMINVES)
freq(latbaro_2024$IDENPA)  

freq(latbaro_all$NUMINVES)
freq(latbaro_all$IDENPA)  

#3. Seleccionar variables de interés ----


latbaro_2002 <- latbaro_2002 |> 
  select(numinves, p36std, p34std, p34stc, p34stf)

latbaro_2004 <- latbaro_2004 |> 
  select(numinves, p34stf, p32std, p34stb, p34std)

latbaro_2006 <- latbaro_2006 |> 
  select(numinves, p24st.f, p32st.a, p24st.a, p24st.c)

latbaro_2008 <- latbaro_2008 |> 
  select(numinves, p28st.a, p31s.ta, p28st.b, p28st.c)

latbaro_2010 <- latbaro_2010 |> 
  select(NUMINVES, P20ST.A, P18ST.A, P20ST.B, P20ST.C)

latbaro_2013 <- latbaro_2013 |> 
  select(NUMINVES, P26TGB.C, P26TGB.B, P26TGB.E, P26TGB.G)

latbaro_2015 <- latbaro_2015 |> 
  select(NUMINVES, P16ST.F, P16ST.G, P16ST.H, P19ST.C)

latbaro_2017 <- latbaro_2017 |> 
  select(NUMINVES, P14ST.D, P14ST.E, P14ST.F, P14ST.G)

latbaro_2020 <- latbaro_2020 |> 
  select(NUMINVES, P13ST.D, P13ST.E, P13ST.F, P13ST.G)

latbaro_2024 <- latbaro_2024 |> 
  select(NUMINVES, P14ST.D, P14ST.E, P14ST.F, P14ST.G)

latbaro_all <- latbaro_all |> 
  select(IDENPA, P14ST.D, P14ST.E, P14ST.F, P14ST.G)  

#4. Recodificar variables y convertir en dummies ----

#4.1 2002

freq(latbaro_2002$p36std) #cong
freq(latbaro_2002$p34std) #gob
freq(latbaro_2002$p34stc) #jud
freq(latbaro_2002$p34stf) #partpol

latbaro_2002 = latbaro_2002 |> 
  rename(anio = numinves,
         conf_cong = p36std,
         conf_gob = p34std,
         conf_jud = p34stc,
         conf_part = p34stf) |> 
  mutate(across(
    .cols = c(conf_cong, conf_gob, conf_jud, conf_part),
    .fns  = ~ as.numeric(.) |> 
      car::recode("c(-1, -2, -3, -4, -5) = NA") |> 
      car::recode("c(3,4) = 0; c(1,2) = 1")
  )) 

freq(latbaro_2002$anio)
freq(latbaro_2002$conf_cong)
freq(latbaro_2002$conf_gob)
freq(latbaro_2002$conf_jud)
freq(latbaro_2002$conf_part)

#4.2 2004 ----

freq(latbaro_2004$p34stf) #cong
freq(latbaro_2004$p32std) #gob
freq(latbaro_2004$p34stb) #jud
freq(latbaro_2004$p34std) #partpol


latbaro_2004 = latbaro_2004 |> 
  rename(anio = numinves,
         conf_cong = p34stf,
         conf_gob = p32std,
         conf_jud = p34stb,
         conf_part = p34std) |> 
  mutate(across(
    .cols = c(conf_cong, conf_gob, conf_jud, conf_part),
    .fns  = ~ as.numeric(.) |> 
      car::recode("c(-1, -2, -3, -4, -5) = NA") |> 
      car::recode("c(3,4) = 0; c(1,2) = 1")
  )) 

freq(latbaro_2004$conf_cong)
freq(latbaro_2004$conf_gob)
freq(latbaro_2004$conf_jud)
freq(latbaro_2004$conf_part)

#4.3 2006

freq(latbaro_2006$p24st.f) #cong
freq(latbaro_2006$p32st.a) #gob
freq(latbaro_2006$p24st.a) #jud
freq(latbaro_2006$p24st.c) #partpol


latbaro_2006 = latbaro_2006 |> 
  rename(anio = numinves,
         conf_cong = p24st.f,
         conf_gob = p32st.a,
         conf_jud = p24st.a,
         conf_part = p24st.c) |> 
  mutate(across(
    .cols = c(conf_cong, conf_gob, conf_jud, conf_part),
    .fns  = ~ as.numeric(.) |> 
      car::recode("c(-1, -2, -3, -4, -5) = NA") |> 
      car::recode("c(3,4) = 0; c(1,2) = 1")
  )) 

freq(latbaro_2006$conf_cong)
freq(latbaro_2006$conf_gob)
freq(latbaro_2006$conf_jud)
freq(latbaro_2006$conf_part)

#4.4 2008----

freq(latbaro_2008$p28st.a) #cong
freq(latbaro_2008$p31s.ta) #gob
freq(latbaro_2008$p28st.b) #jud
freq(latbaro_2008$p28st.c) #partpol

latbaro_2008 = latbaro_2008 |> 
  rename(anio = numinves,
         conf_cong = p28st.a,
         conf_gob = p31s.ta,
         conf_jud = p28st.b,
         conf_part = p28st.c) |> 
  mutate(across(
    .cols = c(conf_cong, conf_gob, conf_jud, conf_part),
    .fns  = ~ as.numeric(.) |> 
      car::recode("c(-1, -2, -3, -4, -5) = NA") |> 
      car::recode("c(3,4) = 0; c(1,2) = 1")
  )) 

freq(latbaro_2008$conf_cong)
freq(latbaro_2008$conf_gob)
freq(latbaro_2008$conf_jud)
freq(latbaro_2008$conf_part)

#4.5 2010----

freq(latbaro_2010$P20ST.A) #cong
freq(latbaro_2010$P18ST.A) #gob
freq(latbaro_2010$P20ST.B) #jud
freq(latbaro_2010$P20ST.C) #partpol

latbaro_2010 = latbaro_2010 |> 
  rename(anio = NUMINVES,
         conf_cong = P20ST.A,
         conf_gob = P18ST.A,
         conf_jud = P20ST.B,
         conf_part = P20ST.C) |> 
  mutate(across(
    .cols = c(conf_cong, conf_gob, conf_jud, conf_part),
    .fns  = ~ as.numeric(.) |> 
      car::recode("c(-1, -2, -3, -4, -5) = NA") |> 
      car::recode("c(3,4) = 0; c(1,2) = 1")
  )) 

freq(latbaro_2010$conf_cong)
freq(latbaro_2010$conf_gob)
freq(latbaro_2010$conf_jud)
freq(latbaro_2010$conf_part)

#4.6 2013----

freq(latbaro_2013$P26TGB.C) #cong
freq(latbaro_2013$P26TGB.B) #gob
freq(latbaro_2013$P26TGB.E) #jud
freq(latbaro_2013$ P26TGB.G) #partpol

latbaro_2013 = latbaro_2013 |> 
  rename(anio = NUMINVES,
         conf_cong = P26TGB.C,
         conf_gob = P26TGB.B,
         conf_jud = P26TGB.E,
         conf_part = P26TGB.G) |> 
  mutate(across(
    .cols = c(conf_cong, conf_gob, conf_jud, conf_part),
    .fns  = ~ as.numeric(.) |> 
      car::recode("c(-1, -2, -3, -4, -5) = NA") |> 
      car::recode("c(3,4) = 0; c(1,2) = 1")
  )) 

freq(latbaro_2013$conf_cong)
freq(latbaro_2013$conf_gob)
freq(latbaro_2013$conf_jud)
freq(latbaro_2013$conf_part)

#4.7 2015----

freq(latbaro_2015$P16ST.F) #cong
freq(latbaro_2015$P16ST.G) #gob
freq(latbaro_2015$P16ST.H) #jud
freq(latbaro_2015$P19ST.C) #partpol

latbaro_2015 = latbaro_2015 |> 
  rename(anio = NUMINVES,
         conf_cong = P16ST.F,
         conf_gob = P16ST.G,
         conf_jud = P16ST.H,
         conf_part = P19ST.C) |> 
  mutate(across(
    .cols = c(conf_cong, conf_gob, conf_jud, conf_part),
    .fns  = ~ as.numeric(.) |> 
      car::recode("c(-1, -2, -3, -4, -5) = NA") |> 
      car::recode("c(3,4) = 0; c(1,2) = 1")
  )) 

freq(latbaro_2015$conf_cong)
freq(latbaro_2015$conf_gob)
freq(latbaro_2015$conf_jud)
freq(latbaro_2015$conf_part)

#4.8 2017----

freq(latbaro_2017$P14ST.D) #cong
freq(latbaro_2017$P14ST.E) #gob
freq(latbaro_2017$P14ST.F) #jud
freq(latbaro_2017$P14ST.G) #partpol

latbaro_2017 = latbaro_2017 |> 
  rename(anio = NUMINVES,
         conf_cong = P14ST.D,
         conf_gob = P14ST.E,
         conf_jud = P14ST.F,
         conf_part = P14ST.G) |> 
  mutate(across(
    .cols = c(conf_cong, conf_gob, conf_jud, conf_part),
    .fns  = ~ as.numeric(.) |> 
      car::recode("c(-1, -2, -3, -4, -5) = NA") |> 
      car::recode("c(3,4) = 0; c(1,2) = 1")
  )) 

freq(latbaro_2017$conf_cong)
freq(latbaro_2017$conf_gob)
freq(latbaro_2017$conf_jud)
freq(latbaro_2017$conf_part)

#4.9 2020----

freq(latbaro_2020$P13ST.D) #cong
freq(latbaro_2020$P13ST.E) #gob
freq(latbaro_2020$P13ST.F) #jud
freq(latbaro_2020$P13ST.G) #partpol

latbaro_2020 = latbaro_2020 |> 
  rename(anio = NUMINVES,
         conf_cong = P13ST.D,
         conf_gob = P13ST.E,
         conf_jud = P13ST.F,
         conf_part = P13ST.G) |> 
  mutate(across(
    .cols = c(conf_cong, conf_gob, conf_jud, conf_part),
    .fns  = ~ as.numeric(.) |> 
      car::recode("c(-1, -2, -3, -4, -5) = NA") |> 
      car::recode("c(3,4) = 0; c(1,2) = 1")
  )) 

freq(latbaro_2020$conf_cong)
freq(latbaro_2020$conf_gob)
freq(latbaro_2020$conf_jud)
freq(latbaro_2020$conf_part)

#4.10 2024----

freq(latbaro_2024$P14ST.D) #cong
freq(latbaro_2024$P14ST.E) #gob
freq(latbaro_2024$P14ST.F) #jud
freq(latbaro_2024$P14ST.G) #partpol

latbaro_2024 = latbaro_2024 |> 
  rename(anio = NUMINVES,
         conf_cong = P14ST.D,
         conf_gob = P14ST.E,
         conf_jud = P14ST.F,
         conf_part = P14ST.G) |> 
  mutate(across(
    .cols = c(conf_cong, conf_gob, conf_jud, conf_part),
    .fns  = ~ as.numeric(.) |> 
      car::recode("c(-1, -2, -3, -4, -5) = NA") |> 
      car::recode("c(3,4) = 0; c(1,2) = 1")
  )) 

freq(latbaro_2024$conf_cong)
freq(latbaro_2024$conf_gob)
freq(latbaro_2024$conf_jud)
freq(latbaro_2024$conf_part)


#4.11 all----

freq(latbaro_all$P14ST.D)
freq(latbaro_all$P14ST.E)
freq(latbaro_all$P14ST.F)
freq(latbaro_all$P14ST.G)

latbaro_all = latbaro_all |> 
  rename(conf_cong = P14ST.D,
         conf_gob = P14ST.E,
         conf_jud = P14ST.F,
         conf_part = P14ST.G) |> 
  mutate(across(
    .cols = c(conf_cong, conf_gob, conf_jud, conf_part),
    .fns  = ~ as.numeric(.) |> 
      car::recode("c(-1, -2, -3, -4, -5) = NA") |> 
      car::recode("1 = 4; 2 = 3; 3 = 2; 4 = 1")
  )) 

freq(latbaro_all$conf_cong)
freq(latbaro_all$conf_gob)
freq(latbaro_all$conf_jud)
freq(latbaro_all$conf_part)

#5 Unir bases de datos----

latbaro_long <- bind_rows(latbaro_2002, latbaro_2004, latbaro_2006, latbaro_2008, 
                          latbaro_2010, latbaro_2013, latbaro_2015, latbaro_2017, 
                          latbaro_2020, latbaro_2024)



#6. Calcular media anual----

medias <- latbaro_long |> 
  group_by(anio) |> 
  summarise(
    media_cong = mean(conf_cong, na.rm=T),
    media_gob = mean(conf_gob, na.rm=T),
    media_jud = mean(conf_jud, na.rm=T),
    media_part = mean(conf_part, na.rm=T)
  )

medias[1, "anio"] <- 2013
medias[2, "anio"] <- 2015
medias[3, "anio"] <- 2024


#7. Crear gráfico long----

data_grafico <- medias |> 
  pivot_longer(cols = -anio, names_to = "institucion", values_to = "porcentaje")

freq(data_grafico$anio)

data_grafico <- data_grafico |> 
  mutate(
    anio = as.numeric(anio)
  )


graf_long <- ggplot(data_grafico, aes(x = anio, y = porcentaje, color = institucion, group = institucion)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  # Configuramos el eje Y para que muestre porcentajes de 0 a 100
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 0.7)) +
  # Mejoramos los nombres de la leyenda
  scale_color_discrete(labels = c("Congreso", "Gobierno", "P. Judicial", "Partidos")) +
  # Estética profesional
  theme_minimal() +
  labs(
    x = "Año",
    y = "% de Confianza",
    color = "Institución",
    caption = "Elaboración propia basada en Latinobarómetro (2002-2024)."
  ) +
  theme(legend.position = "right", 
        plot.caption = element_text(hjust = 0, size = 8, color = "black"), # El caption se va a la izquierda
        plot.caption.position = "plot",
        axis.title = element_text(size = 14, color = "black"),       # títulos ejes X e Y
        axis.text = element_text(size = 14, color = "black"),        # etiquetas de ejes
        legend.title = element_text(size = 14, color = "black"),     # título leyenda
        legend.text = element_text(size = 14, color = "black"))

ggsave("C:/Users/jpdia/OneDrive/Documents/GitHub/tesis-final/tesis-pregrado/presentations/examen_titulo/graphs/graf_long.png", 
       graf_long, width = 8, height = 4, dpi = 300)



#8. Crear indice para analisis comparado----

latbaro_all <- latbaro_all |> 
  rowwise() |>  #cada fila como "grupo" independiente
  mutate(confpol = sum(conf_cong, conf_gob, conf_jud, conf_part)) |>  #Suma en cada fila las 4 variables indicadas
  ungroup() |> 
  mutate(confpol = (1 + ((confpol-4)*9 / (16-4)))
  )

descr(latbaro_all$confpol)

#9. Calcular media por país

medias_pais <- latbaro_all |> 
  group_by(IDENPA) |> 
  summarise(media_confpol = mean(confpol, na.rm = T))

freq(medias_pais$media_confpol)
freq(medias_pais$IDENPA)

medias_pais$IDENPA <- factor(medias_pais$IDENPA,
                             levels = c("32","68", "76", "152", "170", "188", 
                                        "214", "218", "222", "320", "340", "484", 
                                        "591", "600", "604", "858", "862"),
                             labels = c("Arg", "Bol", "Bras", "Chi", 
                                        "Col", "C. Rica", "R. Dom",
                                        "Ecu", "El Salv", "Guat", "Hond",
                                        "Mex", "Pan", "Par", "Peru", "Urug", "Venez") 
                             )



#9. Crear gráfico para análisis comparado




graf_pais <- ggplot(medias_pais, aes(x = reorder(IDENPA, media_confpol), y = media_confpol, fill = IDENPA)) +
  geom_col() +
  scale_y_continuous(limits = c(0, 10))+
  # Estética profesional
  theme_minimal() +
  labs(
    x = "",
    y = "Promedio de confianza política",
    caption = "Elaboración propia basada en Latinobarómetro 2024."
  ) +
  theme(legend.position = "none", 
        plot.caption = element_text(hjust = 0, size = 10, color = "black"), # El caption se va a la izquierda
        plot.caption.position = "plot",
        axis.title = element_text(size = 16, color = "black"),       # títulos ejes X e Y
        axis.text = element_text(size = 14, color = "black"),        # etiquetas de ejes
        )
 

ggsave("C:/Users/jpdia/OneDrive/Documents/GitHub/tesis-final/tesis-pregrado/presentations/examen_titulo/graphs/graf_pais.png", 
       graf_pais, width = 12, height = 6, dpi = 300)

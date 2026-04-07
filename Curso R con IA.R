setwd("/Users/juanjo/Desktop/CURSO R PARA PEDIATRAS")
getwd()

# IMPORTAR NUESTRO ARCHIVO DE DATOS (NHANES) ####
# IA: dame el código R para importar en archivo NHANESsimpl.xlsx en una base de datos llamada NHANES
# 1. Instalar el paquete (solo si no lo tienes)
if(!require(readxl)) install.packages("readxl")

# 2. Cargar la librería
library(readxl)

# 3. Importar el archivo y guardarlo como NHANES
data <- read_excel("NHANESsimpl.xlsx")

# 4. Verificar que se cargó correctamente
View(data) # Abre la tabla en una pestaña nueva
head(data) # Muestra las primeras 6 filas en la consola
colnames(data)
ncol(data) # número de columnas
nrow(data) # número de filas
dim(data) # número de filas y columnas

str(data) # para ver la estructura de los datos, tipos de variables

table (data$Gender)

# ARCHIVO BABYNAMES Y BIRTHS ####
# IA: dame el código R para instalar y cargar babynames
install.packages("babynames")
library(babynames)

# Ver las primeras filas
head(babynames)

# Ver el número total de registros (filas) y variables (columnas)
dim(babynames) 
# Debería darte algo cercano a [1,924,665 x 5]

# Ver un resumen estadístico
summary(babynames)

View(births)
View(babynames)
str(babynames)

# NOMBRES MÁS POPULARES ###
library(dplyr)
library(babynames)

babynames %>%
  group_by(name) %>%
  summarise(total = sum(n)) %>%
  arrange(desc(total))

# FILTRAR POR NOMBRE ESPECÍFICO ####
mary_data <- babynames %>%
  filter(name == "Mary")

# Ver los primeros resultados
head(mary_data)

# VISUALIZAR LA TENDENCIA DE UN NOMBRE ####
library(ggplot2)

ggplot(filter(babynames, name == "Mary"), aes(x = year, y = prop, color = sex)) +
  geom_line() +
  labs(title = "Popularidad del nombre 'Mary' a través del tiempo",
       y = "Proporción", x = "Año")

# 5 NOMBRES MÁS POPULARES DE 2017 ####
babynames %>%
  filter(year == 2017, sex == "F") %>%
  slice_max(prop, n = 5)

# CUANTOS NOMBRES ÚNICOS EXISTEN POR AÑO ####
babynames %>%
  group_by(year) %>%
  summarise(n_nombres = n_distinct(name)) %>%
  ggplot(aes(x = year, y = n_nombres)) +
  geom_line() +
  labs(title = "Cantidad de nombres distintos por año")

# UNIR CON ARCHIVO BIRTHS (por la variable year)
library(dplyr)
library(babynames)

# Unimos babynames con births por el año en "data_completa"
data_completa <- babynames %>%
  left_join(births, by = "year")

# EVOLUCIÓN DE LOS NACIMIENTOS ####
library(ggplot2)

ggplot(births, aes(x = year, y = births)) +
  geom_line(color = "steelblue", size = 1) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Evolución de nacimientos totales en EE.UU.",
       x = "Año", y = "Número de nacimientos")

# CALCULO IMC ####
library(dplyr)

data <- data %>%
  mutate(IMC = if_else(Age < 2,
                       Weight / (Length / 100)^2,  # Menores de 2 años usan Length
                       Weight / (Height / 100)^2)) # Mayores de 2 años usan Height

# AÑADIR TABLAS DE OMS Y CREAR VARIABLE STATUS ####
library(dplyr)

# 1. CARGAR TABLAS (Aseguramos una lectura limpia de las columnas L, M, S, P85 y P97)
cargar_y_limpiar <- function(archivo) {
  df <- read.csv2(archivo, header = TRUE)
  if(ncol(df) <= 1) df <- read.csv(archivo, header = TRUE)
  # Seleccionamos solo lo necesario y forzamos a numérico
  df %>% select(1, L, M, S, P85, P97) %>% mutate(across(everything(), as.numeric))
}

ref_m_05  <- cargar_y_limpiar("OMS_Males_0-5_years.csv")
ref_f_05  <- cargar_y_limpiar("OMS_Females_0-5_years.csv")
ref_m_519 <- cargar_y_limpiar("OMS_Males_5-19_years.csv")
ref_f_519 <- cargar_y_limpiar("OMS_Females_5-19_years.csv")

# 2. CREAR TABLA MAESTRA
prep_ref <- function(df, gender, age_col, tipo) {
  df %>%
    mutate(Gender = gender, TipoRef = tipo) %>%
    rename(Age_Match = 1) %>% # Usamos posición 1 para evitar errores de nombre
    select(Gender, Age_Match, TipoRef, L, M, S, P85, P97)
}

ref_maestra <- bind_rows(
  prep_ref(ref_m_05, "male", "Days", "Dias"),
  prep_ref(ref_f_05, "female", "Days", "Dias"),
  prep_ref(ref_m_519, "male", "Month", "Meses"),
  prep_ref(ref_f_519, "female", "Month", "Meses")
)

# 3. PREPARAR "data" Y UNIR REFERENCIAS
data <- data %>%
  mutate(
    Gender = tolower(Gender),
    TipoRef = if_else(AgeMonths <= 60, "Dias", "Meses"),
    Age_Match = if_else(TipoRef == "Dias", 
                        round(AgeMonths * 30.4375), 
                        round(AgeMonths))
  ) %>%
  left_join(ref_maestra, by = c("Gender", "Age_Match", "TipoRef"))

# 4. CALCULAR STATUS
library(dplyr)

data <- data %>%
  mutate(status = case_when(
    # --- LÓGICA PARA ADULTOS (19 años o más) ---
    Age >= 19 & IMC >= 30 ~ "Obesidad",
    Age >= 19 & IMC >= 25 ~ "Sobrepeso",
    Age >= 19            ~ "Normopeso",
    
    # --- LÓGICA PARA MENORES (Menos de 19 años) ---
    # Usamos los percentiles P85 y P97 que vienen de la tabla de la OMS
    Age < 19 & IMC >= P97 ~ "Obesidad",
    Age < 19 & IMC >= P85 ~ "Sobrepeso",
    Age < 19 & !is.na(P85) ~ "Normopeso",
    
    # Si falta algún dato para clasificar
    TRUE ~ "Sin clasificar"
  ))

# Para verificar los resultados del status:
table(data$status)

# CREAR VARIABLE z_IMC ####

library(dplyr)

data <- data %>%
  mutate(z_IMC = if_else(
    condition = Age < 19, 
    # Fórmula LMS: ((IMC/M)^L - 1) / (L*S)
    true = ((IMC / M)^L - 1) / (L * S), 
    # Para adultos o casos sin datos, dejamos NA (vacío)
    false = NA_real_ 
  ))

# Para verificar un resumen de los Z-scores:
summary(data$z_IMC)

# Abrir visor
View(data)

str(data)

# RELACIÓN ENTRE STATUS Y RACE1 ####
library(dplyr)

# Filtramos para quedarnos solo con los registros que tienen un estatus de IMC válido
# (Omitimos adultos o casos sin referencia para que no sesguen el análisis de niños/adolescentes)
data_analisis <- data %>%
  filter(status %in% c("Normopeso", "Sobrepeso", "Obesidad"))

# Crear una tabla de frecuencias relativas (porcentajes por etnia)
tabla_proporciones <- data_analisis %>%
  group_by(Race1, status) %>%
  summarise(n = n()) %>%
  mutate(porcentaje = (n / sum(n)) * 100)

print(tabla_proporciones)

# GRAFICO
library(ggplot2)

ggplot(data_analisis, aes(x = Race1, fill = status)) +
  geom_bar(position = "fill") + # "fill" muestra proporciones de 0 a 1
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Relación entre Etnia y Estatus de IMC",
       x = "Etnia (Race1)",
       y = "Proporción",
       fill = "Estatus") +
  theme_minimal() +
  coord_flip() # Giramos el gráfico para leer mejor los nombres de las etnias

# ESTADÍSTICA para ver relación status y race1
# Crear la tabla de contingencia simple
tabla_cruza <- table(data_analisis$Race1, data_analisis$status)

# Realizar el test de Chi-cuadrado
test_chi2 <- chisq.test(tabla_cruza)

# Ver el resultado
print(test_chi2)

# RELACIÓN ENTRE IMC Y RACE ####
# GRAFICA
library(ggplot2)

# Filtramos para quitar valores NA en Race o IMC
data_plot <- data %>% filter(!is.na(Race1), !is.na(z_IMC))

ggplot(data_plot, aes(x = Race1, y = z_IMC, fill = Race1)) +
  geom_boxplot(outlier.alpha = 0.3) +
  theme_minimal() +
  labs(title = "Distribución del z_IMC por Etnia",
       x = "Etnia",
       y = "Índice de Masa Corporal (z_IMC)") +
  coord_flip() + # Girar para leer mejor las etnias
  theme(legend.position = "none") # Quitamos la leyenda porque ya está en el eje X

# ESTADISTICA para ver relación z_IMC y Etnia
stats_race <- data %>%
  filter(!is.na(Race1), !is.na(z_IMC)) %>%
  group_by(Race1) %>%
  summarise(
    n = n(),
    Media_z_IMC = mean(z_IMC),
    Mediana_z_IMC = median(z_IMC),
    Desv_Est = sd(z_IMC)
  ) %>%
  arrange(desc(Media_z_IMC))

print(stats_race)

# Realizar el ANOVA
modelo_anova <- aov(z_IMC ~ Race1, data = data)

# Ver el resumen del test
summary(modelo_anova)

# Solo si el p-valor del ANOVA fue < 0.05
TukeyHSD(modelo_anova)

# RELACIÓN ENTRE IMC Y POVERTY UTILIZANDO Z-SCORE ####

# Gráfico con Z-score
ggplot(data_poverty, aes(x = Poverty, y = z_IMC)) +
  geom_point(alpha = 0.2, color = "darkgreen") +
  geom_smooth(method = "lm", color = "red") +
  theme_minimal() +
  labs(title = "Relación entre Pobreza y Z-score del IMC (Niños/Adolescentes)",
       x = "Índice de Pobreza",
       y = "Z-score IMC")

# Modelo con Z-score
summary(lm(z_IMC ~ Poverty, data = data_poverty))

# RELACIÓN IMC CON EL COLESTEROL ####
library(ggplot2)

# Filtramos para quitar valores faltantes
data_chol <- data %>% filter(!is.na(TotChol), !is.na(z_IMC))

ggplot(data_chol, aes(x = z_IMC, y = TotChol)) +
  geom_point(alpha = 0.2, color = "darkorchid") + # Puntos con transparencia
  geom_smooth(method = "lm", color = "black") +    # Línea de tendencia lineal
  theme_minimal() +
  labs(title = "Relación entre Z-score del IMC y Colesterol Total",
       x = "Z-score del IMC",
       y = "Colesterol Total (mg/dL)")

test_cor_chol <- cor.test(data_chol$z_IMC, data_chol$TotChol, method = "pearson")
print(test_cor_chol)

modelo_chol <- lm(TotChol ~ z_IMC, data = data_chol)
summary(modelo_chol)

# RELACIÓN DE PANTALLAS CON IMC ####
library(dplyr)
library(ggplot2)

# 1. Limpiar datos (quitar NAs)
data_tv_child <- data %>% 
  filter(!is.na(TVHrsDayChild), !is.na(z_IMC))

# 2. Test de Correlación
cor_tv <- cor.test(data_tv_child$TVHrsDayChild, data_tv_child$z_IMC)
cat("\n--- CORRELACIÓN TV NIÑOS ---\n")
print(cor_tv)

# 3. Modelo de Regresión
modelo_tv <- lm(z_IMC ~ TVHrsDayChild, data = data_tv_child)
cat("\n--- MODELO LINEAL TV NIÑOS ---\n")
print(summary(modelo_tv))

# 4. Gráfico de Dispersión
ggplot(data_tv_child, aes(x = TVHrsDayChild, y = z_IMC)) +
  geom_jitter(alpha = 0.3, color = "steelblue") + # jitter evita que los puntos se amontonen
  geom_smooth(method = "lm", color = "red") +
  theme_minimal() +
  labs(title = "Relación Horas de TV y Z-score IMC (Niños)",
       subtitle = paste("Correlación:", round(cor_tv$estimate, 3), 
                        "| p-valor:", round(cor_tv$p.value, 4)),
       x = "Horas de TV al día",
       y = "Z-score IMC")

# 1. Limpiar datos
data_comp_child <- data %>% 
  filter(!is.na(CompHrsDayChild), !is.na(z_IMC))

# 2. Test de Correlación
cor_comp <- cor.test(data_comp_child$CompHrsDayChild, data_comp_child$z_IMC)
cat("\n--- CORRELACIÓN ORDENADOR NIÑOS ---\n")
print(cor_comp)

# 3. Modelo de Regresión
modelo_comp <- lm(z_IMC ~ CompHrsDayChild, data = data_comp_child)
cat("\n--- MODELO LINEAL ORDENADOR NIÑOS ---\n")
print(summary(modelo_comp))

# 4. Gráfico de Dispersión
ggplot(data_comp_child, aes(x = CompHrsDayChild, y = z_IMC)) +
  geom_jitter(alpha = 0.3, color = "darkgreen") +
  geom_smooth(method = "lm", color = "red") +
  theme_minimal() +
  labs(title = "Relación Horas de Ordenador y Z-score IMC (Niños)",
       subtitle = paste("Correlación:", round(cor_comp$estimate, 3), 
                        "| p-valor:", round(cor_comp$p.value, 4)),
       x = "Horas de ordenador al día",
       y = "Z-score IMC")

# RELACIÓN DE LA ACTIVIDAD FÍSICA CON EL IMC ####
library(dplyr)
library(ggplot2)

# 1. Filtrar NAs
data_active <- data %>% 
  filter(!is.na(PhysActiveDays), !is.na(z_IMC))

# 2. Ver media de z_IMC por cada número de días activos
resumen_actividad <- data_active %>%
  group_by(PhysActiveDays) %>%
  summarise(
    n = n(),
    Media_z_IMC = mean(z_IMC),
    Desv_Est = sd(z_IMC)
  )

print(resumen_actividad)

# 1. Test de Correlación de Pearson
cor_active <- cor.test(data_active$PhysActiveDays, data_active$z_IMC)
cat("\n--- CORRELACIÓN: DÍAS ACTIVOS vs z_IMC ---\n")
print(cor_active)

# 2. Modelo de Regresión Lineal
modelo_active <- lm(z_IMC ~ PhysActiveDays, data = data_active)
cat("\n--- MODELO LINEAL ---\n")
summary(modelo_active)

ggplot(data_active, aes(x = PhysActiveDays, y = z_IMC)) +
  geom_jitter(alpha = 0.2, color = "darkorange", width = 0.2) + # Jitter para dispersar puntos
  geom_smooth(method = "lm", color = "blue", se = TRUE) +      # Línea de regresión
  scale_x_continuous(breaks = 0:7) +                           # Forzar eje X de 0 a 7
  theme_minimal() +
  labs(title = "Relación entre Días de Actividad Física y Z-score IMC",
       subtitle = paste("Correlación:", round(cor_active$estimate, 3), 
                        " | p-valor:", round(cor_active$p.value, 4)),
       x = "Días de actividad física por semana",
       y = "Z-score IMC")

# DIFERENCIAS DE COLESTEROL POR SEXOS ####
library(dplyr)

# Calculamos la media y desviación estándar de TotChol por Gender
resumen_sexo <- data %>%
  filter(!is.na(TotChol), !is.na(Gender)) %>%
  group_by(Gender) %>%
  summarise(
    n = n(),
    Media_Colesterol = mean(TotChol),
    Desv_Est = sd(TotChol),
    Mediana = median(TotChol)
  )

print(resumen_sexo)

library(ggplot2)

ggplot(data %>% filter(!is.na(TotChol)), aes(x = Gender, y = TotChol, fill = Gender)) +
  geom_boxplot(alpha = 0.7, outlier.colour = "red") +
  theme_minimal() +
  labs(title = "Distribución del Colesterol Total por Sexo",
       x = "Sexo",
       y = "Colesterol Total (mg/dL)") +
  scale_fill_manual(values = c("female" = "pink", "male" = "lightblue"))

# Realizar el t-test
test_colesterol <- t.test(TotChol ~ Gender, data = data)

# Ver el resultado
print(test_colesterol)

# DIFERENCIAS EN EL COLESTEROL POR ETNIA ####
# Cargar librerías necesarias
library(dplyr)
library(ggplot2)

# --- 1. ESTADÍSTICA DESCRIPTIVA ---
# Calculamos n, media, desviación estándar y mediana por etnia
resumen_chol_race <- data %>%
  filter(!is.na(TotChol), !is.na(Race1)) %>%
  group_by(Race1) %>%
  summarise(
    n = n(),
    Media_Col = mean(TotChol),
    SD_Col = sd(TotChol),
    Mediana_Col = median(TotChol)
  ) %>%
  arrange(desc(Media_Col)) # Ordenar de mayor a menor promedio

cat("\n--- ESTADÍSTICA DESCRIPTIVA: COLESTEROL POR ETNIA ---\n")
print(resumen_chol_race)

# --- 2. ANÁLISIS GRÁFICO ---
# Boxplot para comparar distribuciones
grafico_chol_race <- ggplot(data %>% filter(!is.na(TotChol), !is.na(Race1)), 
                            aes(x = Race1, y = TotChol, fill = Race1)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.2) +
  coord_flip() + # Giramos el gráfico para leer mejor los nombres de las etnias
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Distribución del Colesterol Total por Etnia",
       subtitle = "NHANES Data",
       x = "Etnia (Race1)",
       y = "Colesterol Total (mg/dL)")

print(grafico_chol_race)

# --- 3. TEST ESTADÍSTICO (ANOVA) ---
# Usamos ANOVA porque comparamos más de 2 grupos
modelo_anova_chol <- aov(TotChol ~ Race1, data = data)

cat("\n--- RESULTADOS DEL TEST ANOVA ---\n")
anova_summary <- summary(modelo_anova_chol)
print(anova_summary)

# --- 4. TEST POST-HOC (TUKEY) ---
# Si el p-valor del ANOVA es < 0.05, vemos cuáles etnias son diferentes entre sí
p_valor_anova <- anova_summary[[1]][["Pr(>F)"]][1]

if(p_valor_anova < 0.05) {
  cat("\n--- EL ANOVA ES SIGNIFICATIVO (p < 0.05) ---\n")
  cat("Ejecutando Test de Tukey para ver diferencias entre parejas:\n")
  print(TukeyHSD(modelo_anova_chol))
} else {
  cat("\n--- EL ANOVA NO ES SIGNIFICATIVO ---\n")
  cat("No hay diferencias estadísticamente significativas entre las etnias.\n")
}

# DIFERENCIAS EN EL COLESTEROL SEGÚN ACTIVIDAD FÍSICA (YES/NO) ####
# Cargar librerías necesarias
library(dplyr)
library(ggplot2)

# --- 1. ESTADÍSTICA DESCRIPTIVA ---
# Filtramos NAs y calculamos promedios por grupo
resumen_chol_active <- data %>%
  filter(!is.na(TotChol), !is.na(PhysActive)) %>%
  group_by(PhysActive) %>%
  summarise(
    n = n(),
    Media_Col = mean(TotChol),
    SD_Col = sd(TotChol),
    Mediana_Col = median(TotChol)
  )

cat("\n--- ESTADÍSTICA DESCRIPTIVA: COLESTEROL SEGÚN ACTIVIDAD FÍSICA ---\n")
print(resumen_chol_active)

# --- 2. ANÁLISIS GRÁFICO ---
# Boxplot comparativo
grafico_chol_active <- ggplot(data %>% filter(!is.na(TotChol), !is.na(PhysActive)), 
                              aes(x = PhysActive, y = TotChol, fill = PhysActive)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) +
  theme_minimal() +
  scale_fill_manual(values = c("Yes" = "springgreen3", "No" = "tomato")) +
  labs(title = "Diferencias en Colesterol Total según Actividad Física",
       subtitle = "Comparación entre grupos activos y sedentarios",
       x = "¿Realiza actividad física?",
       y = "Colesterol Total (mg/dL)") +
  theme(legend.position = "none")

print(grafico_chol_active)

# --- 3. TEST ESTADÍSTICO (t-test) ---
# Comparamos las medias de los dos grupos
test_t_chol <- t.test(TotChol ~ PhysActive, data = data)

cat("\n--- RESULTADOS DE LA PRUEBA t DE STUDENT ---\n")
print(test_t_chol)

# COMPROBAR NORMALIDAD ####
# Ver si el colesterol parece una campana de Gauss
library(ggplot2)
ggplot(data, aes(x = TotChol, fill = PhysActive)) +
  geom_histogram(bins = 30, alpha = 0.5, position = "identity") +
  facet_wrap(~PhysActive) +
  theme_minimal()

# Solo funciona si la muestra es < 5000. Si es mayor, nos guiamos por el histograma.
shapiro.test(data$TotChol[1:100]) # Probamos con una parte

# EXPORTAR GRÁFICO ####
library(ggplot2)
library(dplyr)

# 1. Preparar el gráfico
grafico_final <- data %>% 
  filter(!is.na(TotChol), !is.na(PhysActive)) %>% 
  ggplot(aes(x = PhysActive, y = TotChol, fill = PhysActive)) +
  # Gráfico de violín
  geom_violin(alpha = 0.6, trim = FALSE) + 
  # Añadimos un boxplot fino dentro para ver la mediana y cuartiles
  geom_boxplot(width = 0.1, color = "black", alpha = 0.7, outlier.shape = NA) +
  theme_minimal() +
  # Colores Rosa y Azul
  scale_fill_manual(values = c("No" = "#F06292", "Yes" = "#4FC3F7")) + 
  labs(
    title = "Diferencias en el Colesterol Total según la\nPráctica de Actividad Física Semanal",
    subtitle = "Comparación basada en datos de la encuesta NHANES",
    x = "¿Realiza actividad física?",
    y = "Colesterol Total (mg/dL)"
  ) +
  theme(
    legend.position = "none",
    # Ajustes para que el título se vea completo y centrado
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5, lineheight = 1.2),
    plot.subtitle = element_text(hjust = 0.5, size = 10, color = "grey30"),
    axis.title = element_text(face = "bold"),
    # Añadimos margen alrededor para que no se corte al exportar
    plot.margin = margin(10, 10, 10, 10)
  )

# Mostrar el gráfico en RStudio
print(grafico_final)

# 2. EXPORTAR EN ALTA CALIDAD
# Se guardará en tu carpeta de trabajo actual
ggsave(
  filename = "Colesterol_Actividad_Fisica.png", 
  plot = grafico_final,
  width = 8,       # Ancho en pulgadas
  height = 6,      # Alto en pulgadas
  dpi = 300,       # Resolución (300 es calidad de impresión)
  bg = "white"     # Fondo blanco para evitar transparencias
)

# GRÁFICO CON ESTADÍSTICA INCLUIDA wilcoxon ####
# 1. CARGAR LIBRERÍAS
library(dplyr)
library(ggplot2)
library(ggpubr)

# 2. CREAR LA VARIABLE Y REALIZAR EL ANÁLISIS
# Usamos un pipe único para asegurar que todo se guarde en 'data'
data <- data %>%
  mutate(Grupo_Mexicano = if_else(grepl("Mexican", Race1, ignore.case = TRUE), 
                                  "Mexicano", 
                                  "Resto de Etnias"))

# --- A. ESTADÍSTICA DESCRIPTIVA ---
resumen_mex <- data %>%
  filter(!is.na(z_IMC), !is.na(Grupo_Mexicano)) %>%
  group_by(Grupo_Mexicano) %>%
  summarise(
    n = n(),
    Media_Z = mean(z_IMC, na.rm = TRUE),
    Mediana_Z = median(z_IMC, na.rm = TRUE),
    SD_Z = sd(z_IMC, na.rm = TRUE)
  )

cat("\n--- RESUMEN DESCRIPTIVO ---\n")
print(resumen_mex)

# --- B. CREACIÓN DEL GRÁFICO DE VIOLÍN ---
grafico_mexicano <- data %>%
  filter(!is.na(z_IMC), !is.na(Grupo_Mexicano)) %>%
  ggplot(aes(x = Grupo_Mexicano, y = z_IMC, fill = Grupo_Mexicano)) +
  # Violín con transparencia
  geom_violin(alpha = 0.6, trim = FALSE) + 
  # Boxplot interno estrecho
  geom_boxplot(width = 0.1, color = "black", alpha = 0.7, outlier.shape = NA) +
  
  # Añadir p-valor automático (Test de Wilcoxon)
  stat_compare_means(method = "wilcox.test", 
                     label = "p.format", 
                     label.x = 1.4, 
                     size = 5) +
  
  # Estética y Colores
  theme_minimal() +
  scale_fill_manual(values = c("Mexicano" = "#F06292", "Resto de Etnias" = "#4FC3F7")) + 
  labs(
    title = "Diferencias en el Z-score del IMC:\nMexicanos vs. Resto de Etnias",
    subtitle = "Comparación estadística mediante test de Wilcoxon (NHANES)",
    x = "Grupo Poblacional",
    y = "Z-score del IMC"
  ) +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5, lineheight = 1.2),
    plot.subtitle = element_text(hjust = 0.5, color = "grey30"),
    axis.title = element_text(face = "bold"),
    plot.margin = margin(15, 15, 15, 15)
  )

# Mostrar el gráfico en pantalla
print(grafico_mexicano)

# --- C. TEST ESTADÍSTICO DETALLADO EN CONSOLA ---
cat("\n--- RESULTADO DEL TEST ESTADÍSTICO ---\n")
test_final <- wilcox.test(z_IMC ~ Grupo_Mexicano, data = data)
print(test_final)

# --- D. EXPORTAR EN ALTA CALIDAD ---
ggsave("Comparacion_IMC_Mexicanos_Final.png", 
       plot = grafico_mexicano, 
       width = 8, 
       height = 6, 
       dpi = 300, 
       bg = "white")

# GRÁFICO CON TEST ESTADÍSTICO t.student ####
# 1. CARGAR LIBRERÍAS
library(dplyr)
library(ggplot2)
library(ggpubr)

# 2. CREAR LA VARIABLE "Grupo_Mexicano" EN EL DATASET
# Usamos grepl para detectar "Mexican" sin importar si hay guiones o espacios
data <- data %>%
  mutate(Grupo_Mexicano = if_else(grepl("Mexican", Race1, ignore.case = TRUE), 
                                  "Mexicano", 
                                  "Resto de Etnias"))

# --- A. ESTADÍSTICA DESCRIPTIVA ---
# Calculamos medias y desviaciones (necesarias para el concepto de t-Student)
resumen_t <- data %>%
  filter(!is.na(z_IMC), !is.na(Grupo_Mexicano)) %>%
  group_by(Grupo_Mexicano) %>%
  summarise(
    n = n(),
    Media_Z = mean(z_IMC, na.rm = TRUE),
    SD_Z = sd(z_IMC, na.rm = TRUE),
    Error_Estandar = SD_Z / sqrt(n)
  )

cat("\n--- ESTADÍSTICA DESCRIPTIVA (MEDIAS) ---\n")
print(resumen_t)

# --- B. CREACIÓN DEL GRÁFICO DE VIOLÍN ---
# Con test t de Student incorporado
grafico_t_student <- data %>%
  filter(!is.na(z_IMC), !is.na(Grupo_Mexicano)) %>%
  ggplot(aes(x = Grupo_Mexicano, y = z_IMC, fill = Grupo_Mexicano)) +
  # Cuerpo del violín
  geom_violin(alpha = 0.6, trim = FALSE) + 
  # Boxplot interno para referencia de mediana y cuartiles
  geom_boxplot(width = 0.1, color = "black", alpha = 0.7, outlier.shape = NA) +
  
  # AÑADIR SIGNIFICACIÓN MEDIANTE T-TEST
  stat_compare_means(method = "t.test", 
                     label = "p.format", 
                     label.x = 1.4, 
                     size = 5) +
  
  # Estética y Colores (Rosa y Azul)
  theme_minimal() +
  scale_fill_manual(values = c("Mexicano" = "#F06292", "Resto de Etnias" = "#4FC3F7")) + 
  labs(
    title = "Comparación del Z-score del IMC:\nMexicanos vs. Resto de Etnias",
    subtitle = "Diferencia de medias evaluada mediante t de Student (NHANES)",
    x = "Grupo Poblacional",
    y = "Z-score del IMC"
  ) +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5, lineheight = 1.2),
    plot.subtitle = element_text(hjust = 0.5, color = "grey30"),
    axis.title = element_text(face = "bold"),
    plot.margin = margin(15, 15, 15, 15)
  )

# Mostrar el gráfico
print(grafico_t_student)

# --- C. RESULTADO DEL TEST t DE STUDENT EN CONSOLA ---
cat("\n--- RESULTADO DETALLADO DEL TEST t DE STUDENT ---\n")
# Por defecto R hace el test de Welch (más robusto), que es la versión moderna de t-Student
test_t_resultado <- t.test(z_IMC ~ Grupo_Mexicano, data = data)
print(test_t_resultado)

# --- D. EXPORTAR EN ALTA CALIDAD ---
ggsave("Comparacion_IMC_t_Student.png", 
       plot = grafico_t_student, 
       width = 8, 
       height = 6, 
       dpi = 300, 
       bg = "white")




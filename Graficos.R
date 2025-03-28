library(readr)
df <- read_tsv("/Users/francinazuliani/Desktop/TF/Database/df.tsv")
View(df)
#------------------GRÁFICO DE BARRAS GENEREAL-------

#1

library(ggplot2)

colores <- c("blue", "red", "green", "orange", "purple", "pink")

ggplot(df, aes(x = `Neoplasm Histologic Type Name`, fill = `Mutation Type`)) +
  geom_bar(position = "dodge") +
  labs(title = "Relación entre tipo de mutaciones somáticas y tipo de cáncer de mama",
       x = "Neoplasm Histologic Type Name",
       y = "Frecuencia") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotar etiquetas del eje x
  scale_fill_manual(values = colores)  # Asignar colores personalizados

#
library(ggplot2)
library(dplyr)

# Crear una nueva columna con las abreviaturas para los tipos específicos
df <- df %>%
  mutate(`Neoplasm Histologic Type Abbr` = case_when(
    `Neoplasm Histologic Type Name` == "Infiltrating Ductal Carcinoma" ~ "CDI",
    `Neoplasm Histologic Type Name` == "Infiltrating Lobular Carcinoma" ~ "CLI",
    TRUE ~ `Neoplasm Histologic Type Name`
  ))

colores <- c("blue", "red", "green", "orange", "purple", "pink")

ggplot(df, aes(x = `Neoplasm Histologic Type Abbr`, fill = `Mutation Type`)) +
  geom_bar(position = "dodge") +
  labs(title = "Relación entre tipo de mutaciones somáticas y tipo de cáncer de mama",
       x = "Neoplasm Histologic Type",
       y = "Frecuencia") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10)) +  # Etiquetas del eje x horizontales
  scale_fill_manual(values = colores)  # Asignar colores personalizados


#2
# Histograma de la edad de diagnóstico

library(ggplot2)
ggplot(df, aes(x = `Diagnosis Age`)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribución de Edad de Pacientes con Cáncer de Mama", x = "Edad de Diagnóstico", y = "Frecuencia") +
  theme_minimal()


# Cantidad de sitios mutantes diferentes

library(dplyr)

# Calcular la cantidad de sitios mutantes diferentes
cantidad_sitios_mutantes <- df %>%
  distinct(HGVSc) %>%  # Extraer los valores únicos del campo HGVSc
  nrow() 

print(cantidad_sitios_mutantes)

#-
library(dplyr)
library(ggplot2)

# Calcular la cantidad de mutaciones en cada sitio mutante
mutaciones_por_sitio <- df %>%
  group_by(HGVSc) %>%
  summarize(Cantidad_Mutaciones = n())

# Crear el gráfico de dispersión
ggplot(mutaciones_por_sitio, aes(x = HGVSc, y = Cantidad_Mutaciones)) +
  geom_point(color = "blue", size = 2) +
  labs(title = "Mutaciones somáticas de TP53 en diferentes sitios mutantes",
       x = "Sitio Mutante",
       y = "Cantidad de Mutaciones") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#-
# Cantidad de sitios mutantes diferentes

library(dplyr)

cantidad_mutac <- df %>%
  distinct(`Mutation Type`) %>%  
  nrow()

print(cantidad_mutac)

#graf torta % mutac

library(dplyr)

# Calcular los conteos de cada tipo de mutación
conteos_mutaciones <- df %>%
  count(`Mutation Type`)

# Calcular los porcentajes de cada tipo de mutación
porcentajes_mutaciones <- conteos_mutaciones %>%
  mutate(Porcentaje = n / sum(n) * 100)

# Gráfico de torta
library(ggplot2)

colores <- c("blue", "red", "green", "orange", "purple", "pink")

ggplot(porcentajes_mutaciones, aes(x = "", y = Porcentaje, fill = `Mutation Type`, label = paste0(round(Porcentaje, 1), "%"))) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  # Capa para el borde negro
  geom_text(aes(label = paste0(round(Porcentaje, 1), "%")),
            position = position_stack(vjust = 0.5), 
            color = "black",      # Borde negro
            fontface = "bold", 
            size = 5,           # Tamaño mayor para el borde
            label.size = 0.8,
            #nudge_x = 0.1,        # Desplazamiento para simular el borde
            #nudge_y = 0.1,        # Desplazamiento para simular el borde
            check_overlap = TRUE) +
  # Capa para el texto blanco (contenido principal)
  geom_text(aes(label = paste0(round(Porcentaje, 1), "%")),
            position = position_stack(vjust = 0.5), 
            color = "white",     # Texto blanco
            fontface = "bold", 
            size = 4.5,           # Tamaño del texto
            check_overlap = TRUE) +
  labs(title = "Porcentaje de Mutaciones",
       fill = "Tipo de Mutación",
       y = "Porcentaje") +
  theme_void() +
  theme(legend.position = "right") +
  scale_fill_manual(values = colores)

#----HER2-----
library(dplyr)
library(ggplot2)

# Calcular los porcentajes de cada categoría en IHC-HER2
porcentajes_IHC_HER2 <- df %>%
  group_by(`IHC-HER2`) %>%
  summarize(Conteo = n()) %>%
  mutate(Porcentaje = Conteo / sum(Conteo) * 100)

# Gráfico de barras con porcentajes
ggplot(porcentajes_IHC_HER2, aes(x = `IHC-HER2`, y = Porcentaje, fill = `IHC-HER2`)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = paste0(round(Porcentaje, 1), "%")), vjust = -0.5) +
  labs(title = "Porcentajes de HER2",
       x = "HER2 Status",
       y = "Porcentaje") +
  scale_fill_manual(values = c("Positive" = "skyblue", "Negative" = "salmon")) +
  theme_minimal()

#----ER-----
library(dplyr)
library(ggplot2)

# Calcular los porcentajes de cada categoría en IHC-ER
porcentajes_IHC_ER <- df %>%
  group_by(`ER Status By IHC`) %>%
  summarize(Conteo = n()) %>%
  mutate(Porcentaje_ER = Conteo / sum(Conteo) * 100)

# Crear el gráfico de barras con porcentajes
ggplot(porcentajes_IHC_ER, aes(x = `ER Status By IHC`, y = Porcentaje_ER, fill = `ER Status By IHC`)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = paste0(round(Porcentaje_ER, 1), "%")), vjust = -0.5) +
  labs(title = "Porcentajes de ER",
       x = "ER Status",
       y = "Porcentaje") +
  scale_fill_manual(values = c("Positive" = "skyblue", "Negative" = "salmon")) +
  theme_minimal()

#----PR-----
library(dplyr)
library(ggplot2)

# Porcentajes de cada categoría en IHC-ER
porcentajes_IHC_PR <- df %>%
  group_by(`PR status by ihc`) %>%
  summarize(Conteo = n()) %>%
  mutate(Porcentaje_PR = Conteo / sum(Conteo) * 100)

# Gráfico de barras con porcentajes
ggplot(porcentajes_IHC_PR, aes(x = `PR status by ihc`, y = Porcentaje_PR, fill = `PR status by ihc`)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = paste0(round(Porcentaje_PR, 1), "%")), vjust = -0.5) +
  labs(title = "Porcentajes de PR",
       x = "PR Status",
       y = "Porcentaje") +
  scale_fill_manual(values = c("Positive" = "skyblue", "Negative" = "salmon")) +
  theme_minimal()


#er y her2 Y PR
library(dplyr)
library(ggplot2)

# Filtrar los datos para cada grupo
grupo_LA <- df %>%
    filter(`IHC-HER2` == "Positive", `ER Status By IHC` == "Positive", `PR status by ihc` == "Negative")

grupo_LB <- df %>%
  filter(
    (`ER Status By IHC` == "Positive" & `IHC-HER2` == "Positive") |
      (`ER Status By IHC` == "Positive" & `IHC-HER2` == "Negative" & `PR status by ihc` == "Negative")
  )

grupo_HE <- df %>%
  filter(`IHC-HER2` == "Positive", `ER Status By IHC` == "Negative", `PR status by ihc` == "Negative")

grupo_TN <- df %>%
  filter(`IHC-HER2` == "Negative", `ER Status By IHC` == "Negative", `PR status by ihc` == "Negative")

# Porcentajes para cada grupo
porcentajes_LA <- nrow(grupo_LA) / nrow(df) * 100
porcentajes_LB <- nrow(grupo_LB) / nrow(df) * 100
porcentajes_HE <- nrow(grupo_HE) / nrow(df) * 100
porcentajes_TN <- nrow(grupo_TN) / nrow(df) * 100

# Crear un dataframe con los porcentajes
porcentajes_df <- data.frame(
  Grupo = c("LA", "LB", "HE", "TN"),
  Porcentaje = c(porcentajes_LA, porcentajes_LB, porcentajes_HE, porcentajes_TN)
)

# Gráfico de barras
ggplot(porcentajes_df, aes(x = Grupo, y = Porcentaje, fill = Grupo)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(Porcentaje, 1), "%")), vjust = -0.5) +
  labs(title = "Distribución Perfiles Moleculares",
       x = "Grupo",
       y = "Porcentaje") +
  scale_fill_manual(values = c("LA" = "skyblue", "LB" = "salmon", "HE" = "yellow", "TN" = "green")) +
  theme_minimal()

##########--GRÁFICO % DE MUTACIONES POR PERFIL MOLECULAR--    
#---HE---
library(dplyr)

# Calcular los conteos de cada tipo de mutación
conteos_mutaciones_HE <- grupo_HE %>%
  count(`Mutation Type`)

# Calcular los porcentajes de cada tipo de mutación
porcentajes_mutaciones_HE <- conteos_mutaciones_HE %>%
  mutate(Porcentaje = n / sum(n) * 100)

# Gráfico de torta
#Figura 28
library(ggplot2)

colores <- c("red", "green",  "orange","purple", "pink")

ggplot(porcentajes_mutaciones_HE, aes(x = "", y = Porcentaje, fill = `Mutation Type`, label = paste0(round(Porcentaje, 1), "%"))) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  # Capa para el borde negro
  geom_text(aes(label = paste0(round(Porcentaje, 1), "%")),
            position = position_stack(vjust = 0.5), 
            color = "black",      # Borde negro
            fontface = "bold", 
            size = 7,           # Tamaño mayor para el borde
            label.size = 1,
            #nudge_x = 0.1,        # Desplazamiento para simular el borde
            #nudge_y = 0.1,        # Desplazamiento para simular el borde
            check_overlap = TRUE) +
  # Capa para el texto blanco (contenido principal)
  geom_text(aes(label = paste0(round(Porcentaje, 1), "%")),
            position = position_stack(vjust = 0.5), 
            color = "white",     # Texto blanco
            fontface = "bold", 
            size = 6.5,           # Tamaño del texto
            check_overlap = TRUE) +
  labs(title = "Frecuencia de Mutaciones Del Perfil Molecular HE",
       fill = "Tipo de Mutación",
       y = "Porcentaje") +
  theme_void() +
  theme(legend.position = "right") +
  scale_fill_manual(values = colores)

#----LA-----
library(dplyr)
# Calcular los conteos de cada tipo de mutación
conteos_mutaciones_LA <- grupo_LA %>%
  count(`Mutation Type`)
# Calcular los porcentajes de cada tipo de mutación
porcentajes_mutaciones_LA <- conteos_mutaciones_LA %>%
  mutate(Porcentaje = n / sum(n) * 100)
# Gráfico de torta
#Figura 29
library(ggplot2)

colores <- c("orange","purple","blue", "red", "green", "pink")
ggplot(porcentajes_mutaciones_LA, aes(x = "", y = Porcentaje, fill = `Mutation Type`, label = paste0(round(Porcentaje, 1), "%"))) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(round(Porcentaje, 1), "%")),
            position = position_stack(vjust = 0.5), 
            color = "black",      # Borde negro
            fontface = "bold", 
            size = 7,           # Tamaño mayor para el borde
            label.size = 1,
            #nudge_x = 0.1,        # Desplazamiento para simular el borde
            #nudge_y = 0.1,        # Desplazamiento para simular el borde
            check_overlap = TRUE) +
  # Capa para el texto blanco (contenido principal)
  geom_text(aes(label = paste0(round(Porcentaje, 1), "%")),
            position = position_stack(vjust = 0.5), 
            color = "white",     # Texto blanco
            fontface = "bold", 
            size = 6.5,           # Tamaño del texto
            check_overlap = TRUE) +
  labs(title = "Frecuencia de Mutaciones Del Perfil Molecular LA",
       fill = "Tipo de Mutación",
       y = "Porcentaje") +
  theme_void() +
  theme(legend.position = "right") +
  scale_fill_manual(values = colores)

#----LB-----
library(dplyr)
# Calcular los conteos de cada tipo de mutación
conteos_mutaciones_LB <- grupo_LB %>%
  count(`Mutation Type`)
# Calcular los porcentajes de cada tipo de mutación
porcentajes_mutaciones_LB <- conteos_mutaciones_LB %>%
  mutate(Porcentaje = n / sum(n) * 100)
# Gráfico de torta
library(ggplot2)

colores <- c("blue", "red", "orange","purple",  "pink","green")

ggplot(porcentajes_mutaciones_LB, aes(x = "", y = Porcentaje, fill = `Mutation Type`, label = paste0(round(Porcentaje, 1), "%"))) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(round(Porcentaje, 1), "%")),
            position = position_stack(vjust = 0.5), 
            color = "black",      # Borde negro
            fontface = "bold", 
            size = 7,           # Tamaño mayor para el borde
            label.size = 1,
            #nudge_x = 0.1,        # Desplazamiento para simular el borde
            #nudge_y = 0.1,        # Desplazamiento para simular el borde
            check_overlap = TRUE) +
  # Capa para el texto blanco (contenido principal)
  geom_text(aes(label = paste0(round(Porcentaje, 1), "%")),
            position = position_stack(vjust = 0.5), 
            color = "white",     # Texto blanco
            fontface = "bold", 
            size = 6.5,           # Tamaño del texto
            check_overlap = TRUE) +
  labs(title = "Frecuencia de Mutaciones Del Perfil Molecular LB",
       fill = "Tipo de Mutación",
       y = "Porcentaje") +
  theme_void() +
  theme(legend.position = "right") +
  scale_fill_manual(values = colores)
 
#----TN-----
library(dplyr)
# Calcular los conteos de cada tipo de mutación
conteos_mutaciones_TN <- grupo_TN %>%
  count(`Mutation Type`)
# Calcular los porcentajes de cada tipo de mutación
porcentajes_mutaciones_TN <- conteos_mutaciones_TN %>%
  mutate(Porcentaje = n / sum(n) * 100)
# Gráfico de torta
library(ggplot2)

colores <- c("blue", "red", "green", "orange", "purple", "pink")

ggplot(porcentajes_mutaciones_TN, aes(x = "", y = Porcentaje, fill = `Mutation Type`, label = paste0(round(Porcentaje, 1), "%"))) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(round(Porcentaje, 1), "%")),
            position = position_stack(vjust = 0.5), 
            color = "black",      # Borde negro
            fontface = "bold", 
            size = 7,           # Tamaño mayor para el borde
            label.size = 1,
            #nudge_x = 0.1,        # Desplazamiento para simular el borde
            #nudge_y = 0.1,        # Desplazamiento para simular el borde
            check_overlap = TRUE) +
  # Capa para el texto blanco (contenido principal)
  geom_text(aes(label = paste0(round(Porcentaje, 1), "%")),
            position = position_stack(vjust = 0.5), 
            color = "white",     # Texto blanco
            fontface = "bold", 
            size = 6.5,           # Tamaño del texto
            check_overlap = TRUE) +
  labs(title = "Frecuencia de Mutaciones Del Perfil Molecular TN",
       fill = "Tipo de Mutación",
       y = "Porcentaje") +
  theme_void() +
  theme(legend.position = "right") +
  scale_fill_manual(values = colores)


#-------AMINOACIDOS---------#  <-----------

library(dplyr)

# Filtrar Mutation Type = "Missense_Mutation"
df_filtered <- df %>%
  filter(`Mutation Type` == "Missense_Mutation") %>%  
  group_by(`Protein Change`) %>%  
  summarise(count = n()) %>%  
  arrange(desc(count))

# Calcular el porcentaje 
total_mutations <- sum(df_filtered$count)
df_filtered <- df_filtered %>%
  mutate(percentage = (count / total_mutations) * 100) %>%  
  mutate(percentage = paste0(round(percentage, 2), "%"))  

#top_10 <- df_filtered %>% 
#  head(10)

#print(top_10)

View(df_filtered)

#------------ GRÁFICOS POR TIPO DE CÁNCER DE MAMA ---------
library(readr)

# Crear un nuevo conjunto de datos con solo las columnas necesarias para hacer el dot plot
dp <- subset(df, select = c(`Neoplasm Histologic Type Name`, `Mutation Type`))
#View(dp)

# Filtrar el conjunto de datos 

#TIPO 1: Infiltrating Ductal Carcinoma
dp1 <- subset(df, `Neoplasm Histologic Type Name` == 'Infiltrating Ductal Carcinoma')
View(dp1)


#TIPO 3: Infiltrating Lobular Carcinoma
dp3 <- subset(df, `Neoplasm Histologic Type Name` == 'Infiltrating Lobular Carcinoma')
View(dp3)

#============================================================================================
#TIPO 1: Infiltrating Ductal Carcinoma

#------------- GRÁFICO DE TORTA
library(ggplot2)
# Calcular las frecuencias de cada tipo de mutación
frecuencias <- table(dp1$`Mutation Type`)
# Porcentajes
porcentajes <- prop.table(frecuencias) * 100
# Crear dataframe con las frecuencias y los porcentajes
df_frecuencias1 <- data.frame(Mutation_Type = names(frecuencias),
                              Frecuencia = as.vector(frecuencias),
                              porcentajes = porcentajes)

# Definir los colores deseados
colores <- c("blue", "red", "green", "orange", "purple", "pink")  # Ejemplo de colores
# Crear el gráfico de pastel con etiquetas de porcentaje y colores personalizados
ggplot(df_frecuencias1, aes(x = "", y = Frecuencia, fill = Mutation_Type)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(round(porcentajes, 1), "%")),
            position = position_stack(vjust = 0.5), 
            color = "black",      # Borde negro
            fontface = "bold", 
            size = 5,           # Tamaño mayor para el borde
            label.size = 0.8,
            #nudge_x = 0.1,        # Desplazamiento para simular el borde
            #nudge_y = 0.1,        # Desplazamiento para simular el borde
            check_overlap = TRUE) +
  # Capa para el texto blanco (contenido principal)
  geom_text(aes(label = paste0(round(porcentajes, 1), "%")),
            position = position_stack(vjust = 0.5), 
            color = "white",     # Texto blanco
            fontface = "bold", 
            size = 4.5,           # Tamaño del texto
            check_overlap = TRUE) +
  
  labs(title = "Infiltrating Ductal Carcinoma",
       fill = "Mutation Type") +
  theme_void() +
  scale_fill_manual(values = colores)

#---------------------Edad--------------------------
#Figura 15
library(dplyr)
library(ggplot2)

# Crear una nueva columna con los rangos de edades
dp1 <- dp1 %>%
  mutate(Rango_Edad = cut(`Diagnosis Age`, breaks = seq(20, 100, by = 10), labels = c("20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89", "90-99")))
View(dp1)

# Crear el bar plot
ggplot(dp1, aes(x = Rango_Edad, fill = Rango_Edad)) +
  geom_bar() +
  labs(title = "Edades de Diagnóstico por Rango del Tipo de Cáncer Infiltrating Ductal Carcinoma",
       x = "Rango de Edad",
       y = "Frecuencia") +
  theme_minimal()

#----- porcentaje edad -----

# Contar las frecuencias de cada rango de edad
age_distribution_1 <- dp1 %>%
  count(Rango_Edad) %>%
  arrange(desc(n))

# Calcular el porcentaje de cada rango de edad
age_distribution_1 <- age_distribution_1 %>%
  mutate(Percentage = (n / sum(n)) * 100)

# Mostrar el rango de edad que aparece más frecuentemente
most_frequent_age_range_1 <- age_distribution_1[1, ]
#top_three_age_ranges <- age_distribution[1:5, ]


print(paste("El rango de edad que aparece más frecuentemente es:", most_frequent_age_range_1$Rango_Edad, 
            "con", most_frequent_age_range_1$n, "casos, representando el", 
            round(most_frequent_age_range_1$Percentage, 2), "% del total."))


#--------------er y her2 y pr
library(dplyr)
library(ggplot2)

# Filtrar los datos para cada grupo
grupo_LA_1 <- dp1 %>%
  filter(`IHC-HER2` == "Positive", `ER Status By IHC` == "Positive", `PR status by ihc` == "Negative")
grupo_LB_1 <- dp1 %>%
  filter(
    (`ER Status By IHC` == "Positive" & `IHC-HER2` == "Positive") |
      (`ER Status By IHC` == "Positive" & `IHC-HER2` == "Negative" & `PR status by ihc` == "Negative")
  )
grupo_HE_1 <- dp1 %>%
  filter(`IHC-HER2` == "Positive", `ER Status By IHC` == "Negative", `PR status by ihc` == "Negative")
grupo_TN_1 <- dp1 %>%
  filter(`IHC-HER2` == "Negative", `ER Status By IHC` == "Negative", `PR status by ihc` == "Negative")

# Calcular los porcentajes para cada grupo
porcentajes_LA_1 <- nrow(grupo_LA_1) / nrow(dp1) * 100
porcentajes_LB_1 <- nrow(grupo_LB_1) / nrow(dp1) * 100
porcentajes_HE_1 <- nrow(grupo_HE_1) / nrow(dp1) * 100
porcentajes_TN_1 <- nrow(grupo_TN_1) / nrow(dp1) * 100

# Crear dataset con porcentajes
porcentajes_dp1 <- data.frame(
  Grupo = c("LA", "LB", "HE", "TN"),
  Porcentaje_1 = c(porcentajes_LA_1, porcentajes_LB_1, porcentajes_HE_1, porcentajes_TN_1)
)

# Gráfico de barras
ggplot(porcentajes_dp1, aes(x = Grupo, y = Porcentaje_1, fill = Grupo)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(Porcentaje_1, 1), "%")), vjust = -0.5) +
  labs(title = "Distribución Perfiles Moleculares en CDI",
       x = "Grupo",
       y = "Porcentaje") +
  scale_fill_manual(values = c("LA" = "skyblue", "LB" = "salmon", "HE" = "yellow", "TN" = "green")) +
  theme_minimal()



#===========================================================================================

#TIPO 3: Infiltrating Lobular Carcinoma

#------------- GRÁFICO DE TORTA
library(ggplot2)
#Frecuencias de cada tipo de mutación
frecuencias <- table(dp3$`Mutation Type`)
# Porcentajes
porcentajes <- prop.table(frecuencias) * 100
# Crear un dataframe con las frecuencias y los porcentajes
df_frecuencias2 <- data.frame(Mutation_Type = names(frecuencias),
                              Frecuencia = as.vector(frecuencias),
                              porcentajes = porcentajes)

colores <- c("blue", "red", "green", "orange", "purple", "pink")  # Ejemplo de colores

# Crear el gráfico de torta
ggplot(df_frecuencias2, aes(x = "", y = Frecuencia, fill = Mutation_Type)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(round(porcentajes, 1), "%")),
            position = position_stack(vjust = 0.5), 
            color = "black",      # Borde negro
            fontface = "bold", 
            size = 5,           # Tamaño mayor para el borde
            label.size = 0.8,
            #nudge_x = 0.1,        # Desplazamiento para simular el borde
            #nudge_y = 0.1,        # Desplazamiento para simular el borde
            check_overlap = TRUE) +
  # Capa para el texto blanco (contenido principal)
  geom_text(aes(label = paste0(round(porcentajes, 1), "%")),
            position = position_stack(vjust = 0.5), 
            color = "white",     # Texto blanco
            fontface = "bold", 
            size = 4.5,           # Tamaño del texto
            check_overlap = TRUE) +
  labs(title = "Infiltrating Lobular Carcinoma",
       fill = "Mutation Type") +
  theme_void() +
  scale_fill_manual(values = colores)

#---------------------Edad--------------------------

library(dplyr)
library(ggplot2)

# Crear una nueva columna con los rangos de edades
dp3 <- dp3 %>%
  mutate(Rango_Edad = cut(`Diagnosis Age`, breaks = seq(20, 100, by = 10), labels = c("20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89", "90-99")))

# Crear el bar plot
ggplot(dp3, aes(x = Rango_Edad, fill = Rango_Edad)) +
  geom_bar() +
  labs(title = "Edades de Diagnóstico por Rango del Tipo de Cáncer Infiltrating Lobular Carcinoma",
       x = "Rango de Edad",
       y = "Frecuencia") +
  theme_minimal()

library(dplyr)

# Mostrar la columna ID del subset df
df %>%
  select(ID) %>%
  print(n = 200)

#----- porcentaje edad -----

# Contar las frecuencias de cada rango de edad
age_distribution_3 <- dp3 %>%
  count(Age_Range) %>%
  arrange(desc(n))

# Calcular el porcentaje de cada rango de edad
age_distribution_3 <- age_distribution_3 %>%
  mutate(Percentage = (n / sum(n)) * 100)

# Rango de edad que aparece más frecuentemente
most_frequent_age_range_3 <- age_distribution_3[1, ]
#top_three_age_ranges <- age_distribution[1:5, ]


print(paste("El rango de edad que aparece más frecuentemente es:", most_frequent_age_range_3$Age_Range, 
            "con", most_frequent_age_range_3$n, "casos, representando el", 
            round(most_frequent_age_range_3$Percentage, 2), "% del total."))

#-------Género-------

total_observaciones <- nrow(df)

num_female <- sum(df$Sex == "Female")

# Sex = "Female"
porcentaje_female <- (num_female / total_observaciones) * 100

print(paste("Porcentaje de 'Female':", round(porcentaje_female, 2), "%"))

#---------Edad--------

library(dplyr)

# Crear los rangos de edad
df <- df %>%
  mutate(Age_Range = cut(`Diagnosis Age`, 
                         breaks = seq(0, 100, by = 10), 
                         right = FALSE, 
                         labels = paste(seq(0, 90, by = 10), seq(10, 100, by = 10) - 1, sep = "-")))

# Contar las frecuencias de cada rango de edad
age_distribution <- df %>%
  count(Age_Range) %>%
  arrange(desc(n))

# Calcular el porcentaje de cada rango de edad
age_distribution <- age_distribution %>%
  mutate(Percentage = (n / sum(n)) * 100)

# Rango de edad que aparece más frecuentemente
most_frequent_age_range <- age_distribution[1, ]
top_three_age_ranges <- age_distribution[1:5, ]


print(paste("El rango de edad que aparece más frecuentemente es:", most_frequent_age_range$Age_Range, 
            "con", most_frequent_age_range$n, "casos, representando el", 
            round(most_frequent_age_range$Percentage, 2), "% del total."))

for (i in 1:nrow(top_three_age_ranges)) {
  print(paste("El rango de edad que aparece en la posición", i, "es:", top_three_age_ranges$Age_Range[i], 
              "con", top_three_age_ranges$n[i], "casos, representando el", 
              round(top_three_age_ranges$Percentage[i], 2), "% del total."))
}
#--------------er y her2 y pr
library(dplyr)
library(ggplot2)

# Filtrar los datos para cada grupo
grupo_LA_3 <- dp3 %>%
  filter(`IHC-HER2` == "Positive", `ER Status By IHC` == "Positive", `PR status by ihc` == "Negative")
grupo_LB_3 <- dp3 %>%
  filter(
    (`ER Status By IHC` == "Positive" & `IHC-HER2` == "Positive") |
      (`ER Status By IHC` == "Positive" & `IHC-HER2` == "Negative" & `PR status by ihc` == "Negative")
  )
grupo_HE_3 <- dp3 %>%
  filter(`IHC-HER2` == "Positive", `ER Status By IHC` == "Negative", `PR status by ihc` == "Negative")
grupo_TN_3 <- dp3 %>%
  filter(`IHC-HER2` == "Negative", `ER Status By IHC` == "Negative", `PR status by ihc` == "Negative")

# Calcular los porcentajes para cada grupo
porcentajes_LA_3 <- nrow(grupo_LA_3) / nrow(dp3) * 100
porcentajes_LB_3 <- nrow(grupo_LB_3) / nrow(dp3) * 100
porcentajes_HE_3 <- nrow(grupo_HE_3) / nrow(dp3) * 100
porcentajes_TN_3 <- nrow(grupo_TN_3) / nrow(dp3) * 100

# Crear un dataframe con los porcentajes
porcentajes_dp3 <- data.frame(
  Grupo = c("LA", "LB", "HE", "TN"),
  Porcentaje_3 = c(porcentajes_LA_3, porcentajes_LB_3, porcentajes_HE_3, porcentajes_TN_3)
)

# Crear el gráfico de barras
ggplot(porcentajes_dp3, aes(x = Grupo, y = Porcentaje_3, fill = Grupo)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(Porcentaje_3, 1), "%")), vjust = -0.5) +
  labs(title = "Distribución Perfiles Moleculares en CLI",
       x = "Grupo",
       y = "Porcentaje") +
  scale_fill_manual(values = c("LA" = "skyblue", "LB" = "salmon", "HE" = "yellow", "TN" = "green")) +
  theme_minimal()

#----------------------
#------ Gráfico Barras Acumulado ----

library(ggplot2)

data <- data.frame(
  Tipo_Cancer = rep(c("IDC", "ILC"), each = 4),
  Estado = rep(c("LA", "LB", "HE", "TN"), times = 2),
  Porcentaje = c(3, 18, 7, 40, 1, 21, 5, 15)
)

# Creo el gráfico de barras apiladas
ggplot(data, aes(x = Estado, y = Porcentaje, fill = Tipo_Cancer)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0(Porcentaje, "%")), 
            position = position_stack(vjust = 0.5), 
            size = 3) +
  labs(title = "Distribución acumulada de de perfiles moleculares por tipo de cáncer",
       x = "Estado",
       y = "Porcentaje") +
  scale_fill_manual(values = c("IDC" = "skyblue", "ILC" = "salmon")) +
  theme_minimal()





#Limpio la memoria
rm(list = ls())
gc()

#Cargamos las librerias necesarias
library(ggplot2)
library(dplyr)
library(data.table)
library(arules)
library(arulesSequences)
library(GGally)
library(scales)
library(viridisLite)
library(arulesViz)
library(htmlwidgets)
library(paletteer)

df <- fread("shop clothing.csv")

######################################################################
################# Exploración del Data Frame #########################
######################################################################

#Vemos cuantas filas y columnas tiene el archivo
dim(df)

#visualizamos las primeras 5 filas del dataset
head(df,5)
#visualizamos las ultimas 5 filas del dataset
tail(df,5)

#Estadísticas básicas del Data Frame
summary(df)

#Vemos cuántos NA hay por columna
colSums(is.na(df))

#Si hay filas duplicadas las sumamos y vemos cuantas tenemos
sum(duplicated(df))

#Utilizando el archivo .txt con la explicacion de las variables
#Cambiamos los nombres de las variables para entenderlas mejor
colnames(df)
colnames(df) <- c("año", "mes", "dia", "clicks_sesion", "pais", "sesión_ID",
                  "categoria", "modelo_ropa", "color", "ubicacion_foto",
                  "fotografia", "precio", "prec_may_prom_cat", "num_pagina_sitio")
colnames(df)

#eliminamos la columna año
df <- df %>%
  select(-año)


#Reemplazamos los valores numericos por su categoria
# Definir el mapeo correctamente
paises <- c(
  "1" = "Australia",
  "2" = "Austria",
  "3" = "Bélgica",
  "4" = "Islas Vírgenes Británicas",
  "5" = "Islas Caimán",
  "6" = "Isla de Navidad",
  "7" = "Croacia",
  "8" = "Chipre",
  "9" = "República Checa",
  "10" = "Dinamarca",
  "11" = "Estonia",
  "12" = "no identificado",
  "13" = "Islas Feroe",
  "14" = "Finlandia",
  "15" = "Francia",
  "16" = "Alemania",
  "17" = "Grecia",
  "18" = "Hungría",
  "19" = "Islandia",
  "20" = "India",
  "21" = "Irlanda",
  "22" = "Italia",
  "23" = "Letonia",
  "24" = "Lituania",
  "25" = "Luxemburgo",
  "26" = "México",
  "27" = "Países Bajos",
  "28" = "Noruega",
  "29" = "Polonia",
  "30" = "Portugal",
  "31" = "Rumania",
  "32" = "Rusia",
  "33" = "San Marino",
  "34" = "Eslovaquia",
  "35" = "Eslovenia",
  "36" = "España",
  "37" = "Suecia",
  "38" = "Suiza",
  "39" = "Ucrania",
  "40" = "Emiratos Árabes Unidos",
  "41" = "Reino Unido",
  "42" = "USA",
  "43" = "biz",
  "44" = "com",
  "45" = "int",
  "46" = "net",
  "47" = "org"
)
# Convertir la columna a character para que funcione el mapeo
df$pais <- as.character(df$pais)
# Mapear los valores
df$pais <- paises[df$pais]
# Verificar resultados
head(df$pais)

#hacemos lo mismo para la variable categoria
categorias <- c(
  "1" = "pantalones",
  "2" = "faldas",
  "3" = "blusas",
  "4" = "ofertas"
)
#Se realiza el mapeo
df$categoria <- categorias[as.character(df$categoria)]
# Verificar resultados
head(df$categoria)

##Tambien hacemos esto para los colores y las ubicaciones de las fotos
colores <- c(
  "beige", "negro", "azul", "marrón", "burdeos", "gris",
         "verde", "azul marino", "multicolor", "oliva", "rosado",
         "rojo", "violeta", "blanco"
)
#Ubicaciones
ubicaciones <- c(
  "arriba izquierda", "arriba en el medio", "arriba derecha",
  "abajo izquierda", "abajo en el medio", "abajo derecha"
)
# Mapear las columnas
df <- df %>%
  mutate(
    color = factor(color, levels = 1:14, labels = colores),
    ubicacion_foto = factor(ubicacion_foto, levels = 1:6, labels = ubicaciones)
  )
#Pasamos la columna que ingica si el precio es mayor al promedio de la categoria
#a factor con valores 0 y 1
df$prec_may_prom_cat <- factor(df$prec_may_prom_cat,levels = c(1, 2),labels = c(0, 1))

# --- Mapeo de la variable 'fotografia' ---
fotografia_tipos <- c(
  "1" = "frontal",
  "2" = "de perfil"
)
df$fotografia <- fotografia_tipos[as.character(df$fotografia)]
# --- Fin del mapeo de 'fotografia' ---

#Vemos como queda
head(df)

df <- df %>%
  mutate(
    pais = factor(pais),
    categoria = factor(categoria),
    fotografia = factor(fotografia)
  )

# Verificamos la estructura del dataframe
str(df)

################################################################################
########### Realizamos algunas graficas para conocer mejor los datos ###########
################################################################################

# Crear vectores de colores para categorias
#categorias <- unique(df$categoria)
#colores_categorias <- setNames(viridis(length(categorias), option = "C"), categorias)


# Grafico de barras de Clicks por sesión
df_discretizado <- df %>%
  count(sesión_ID) %>%
  mutate(clicks_discretos = cut(n, 
                                breaks = c(0, 5, 10, 20, Inf), 
                                labels = c("0-5", "6-10", "11-20", "Más de 20"),
                                include.lowest = TRUE)) %>%
  count(clicks_discretos)


ggplot(df_discretizado, aes(x = clicks_discretos, y = n, fill = clicks_discretos)) +
  geom_bar(stat = "identity", color = "white") +
  scale_fill_manual(values = c("#EDF8BC", "#ACE0B4", "#86D0B9", "#41B7C4")) +
  labs(
    title = "Clicks por sesión (agrupados en rangos)", 
    x = "Intervalo de Clicks", 
    y = "Número de sesiones"
  ) +
  theme_minimal() +
  theme(
    plot.title       = element_text(color = "black", size = 24, face = "bold", hjust = 0.1),
    axis.title.x     = element_text(color = "black", size = 14, face = "bold"),
    axis.title.y     = element_text(color = "black", size = 14, face = "bold"),
    axis.text.x      = element_text(size = 15, face = "bold", margin = margin(t = 10)),
    axis.text.y      = element_text(size = 15, face = "bold"),
    legend.text      = element_text(size = 10, face = "bold"),
    legend.title     = element_text(size = 12, face = "bold"),
    panel.background = element_blank(),
    plot.background  = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )


# Grafico de barras de países con más clicks en el sitio web

df %>%
  filter(!pais %in% c("Polonia", "com", "net")) %>% 
  group_by(pais) %>%
  summarise(total_clicks = sum(clicks_sesion, na.rm = TRUE)) %>%
  slice_max(total_clicks, n = 5) %>%
  arrange(total_clicks) %>%
  ggplot(aes(x = pais, y = total_clicks)) +
  geom_col(fill = "#001F3F") +
  scale_y_continuous(labels = scales::label_number(accuracy = 0.01)) +
  labs(
    title = "Países con más clicks en el sitio web en el año 2008",
    y = "Número de clicks",
    x = "",
    caption = "Fuente: elaboración propia"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_blank(),   # Quita líneas principales de grid
    panel.grid.minor = element_blank(),   # Quita líneas menores de grid
    axis.line = element_line(color = "black", size = 0.5),
    plot.title = element_text(color = "#00008B", size = 16, face = "bold"),
    plot.caption = element_text(size = 10)
  )


#Productos distintos vistos por sesión

# Agrupamos, discretizamos y luego contamos
df_discretizado <- df %>%
  group_by(sesión_ID) %>%
  summarise(productos_vistos = n_distinct(modelo_ropa)) %>%
  mutate(productos_discretos = cut(productos_vistos, 
                                   breaks = c(0, 5, 10, 20, Inf), 
                                   labels = c("1-5", "6-10", "11-20", "Más de 20"),
                                   include.lowest = TRUE)) %>%
  group_by(productos_discretos) %>%
  summarise(n = n())  # <- esta línea preserva productos_discretos y crea la columna 'n'

# Ahora el gráfico debería funcionar correctamente
ggplot(df_discretizado, aes(x = productos_discretos, y = n, fill = productos_discretos)) +
  geom_bar(stat = "identity", color = "white") +
  labs(
    title = "Distribución de productos vistos por sesión", 
    x     = "Intervalo de productos vistos", 
    y     = "Número de sesiones",
    fill  = "Productos vistos"
  ) +
  theme_minimal() +
  theme(
    plot.title   = element_text(face = "bold", hjust = 0.5),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    axis.text.x  = element_text(angle = 45, hjust = 1, face = "bold"),
    axis.text.y  = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    legend.text  = element_text(face = "bold"),
    panel.background = element_blank(),
    plot.background  = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )


#Grafico de barras de categorías de producto por sesión.

# Agrupar y contar sesiones únicas por categoría
df_categoria <- df %>%
  group_by(categoria) %>%
  summarise(sesiones = n_distinct(sesión_ID)) %>%
  arrange(desc(sesiones))

# Vector de colores personalizados
colores_personalizados <- c("#CC243C", "#CF8552", "#3E9AB4", "#8E6189")



# Agrupar y contar sesiones únicas por categoría
df_categoria <- df %>%
  group_by(categoria) %>%
  summarise(sesiones = n_distinct(sesión_ID)) %>%
  arrange(desc(sesiones))

ggplot(df_categoria, aes(x = categoria, y = sesiones, fill = categoria)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = colores_personalizados) +
  labs(
    title = "Número de sesiones por categoría de producto",
    x = "Categoría",
    y = "Número de sesiones"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 20, color = "#003366"),
    axis.title.x = element_text(face = "bold", size = 18, color = "black"),
    axis.title.y = element_text(face = "bold", size = 18, color = "black"),
    axis.text.x = element_text(face = "bold", angle = 45, hjust = 1, color = "black"),
    axis.text.y = element_text(face = "bold", color = "black"),
    panel.background = element_blank(),
    plot.background  = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

####### D) Numero total de transacciones y de items ###################

# 1. Encuentramos el número de transacciones
trans <- df %>% distinct(sesión_ID) %>% nrow()

# 2. Encuentramos la cantidad de items distintos
num_modelos_distintos <- df %>% distinct(modelo_ropa) %>% nrow()
print(num_modelos_distintos) 

# 3. Mostramos el total de transacciones e items
trans
num_modelos_distintos


####### E) Itemsets frecuentes para un soporte mínimo de 2% ######

# 1. Creamos un dataframe solo con sesión e ítem, eliminando duplicados si una sesión vio el 
#mismo modelo varias veces

df_trans <- df %>%
  select(sesión_ID, modelo_ropa) %>%
  distinct()

# 2. Agrupar ítems (modelo_ropa) por sesión_ID en una lista
lista_transacciones <- split(df_trans$modelo_ropa, df_trans$sesión_ID)

# 3.Convertir la lista a un objeto de clase "transactions" para arules
transacciones <- as(lista_transacciones, "transactions")

# 4.Ver estructura del objeto transactions resultante
summary(transacciones)

transacciones

# 5. Definir soporte mínimo 2 %
soporte_minimo <- 0.02  

# 6. Aplicar el algoritmo ECLAT
itemsets_eclat <- eclat(
  transacciones,
  parameter = list(supp = soporte_minimo, minlen = 2)
)

#7. Mostrar resultados 
cat("\nTOP-10 itemsets (Eclat)\n")
inspect(head(sort(itemsets_eclat,   by = "support", decreasing = TRUE), 10))


#VISUALIZAMOS EN UN GRAFICO LOS ITEMSETS FRECUENTES
# 1. Filtrar itemsets de longitud 2
itemsets_eclat_2 <- subset(itemsets_eclat, size(items) == 2)

# 2. Obtener top 10 por soporte
top10 <- sort(itemsets_eclat_2, by = "support", decreasing = TRUE)[1:10]
top10_df <- as(top10, "data.frame")

# 3. Crear etiquetas con los ítems
top10_df$items <- labels(top10)

# 4. Graficar con degradé (naranja más fuerte = mayor soporte)
ggplot(top10_df, aes(x = reorder(items, support), y = support, fill = support)) +
  geom_col(color = "black", width = 0.7, show.legend = FALSE) +
  coord_flip() +
  scale_fill_gradientn(colours = rev(paletteer_c("grDevices::YlOrRd", n = 10))) +
  labs(
    title = "Top 10 itemsets frecuentes (2 ítems)", 
    x = "Itemsets", 
    y = "Soporte"
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal() +
  theme(
    plot.title       = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.title.x     = element_text(face = "bold"),
    axis.title.y     = element_text(face = "bold"),
    axis.text        = element_text(face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )


# --- Reglas de Asociación para Polonia, categoría "blusas" ---

# 1. Filtrar el dataset
df_polonia_blusas <- df %>%
  filter(pais == "Polonia", categoria == "blusas") %>%
  select(sesión_ID, modelo_ropa)

# 2. Crear transacciones por sesión
transacciones <- as(split(df_polonia_blusas$modelo_ropa, df_polonia_blusas$sesión_ID), "transactions")

# 3. Generar las reglas de asociación
reglas <- apriori(transacciones,
                  parameter = list(supp = 0.02, conf = 0.20, target = "rules"))

# 4. Ordenar por soporte descendente y seleccionar las 10 principales
reglas_ordenadas <- sort(reglas, by = "support", decreasing = TRUE)
top_10_reglas <- reglas_ordenadas[1:10]

# 5. Mostrar las 10 reglas principales
inspect(top_10_reglas)

#plot(top_10_reglas, method = "graph", engine = "htmlwidget")  # Gráfico interactivo

# Guardar grouped plot como PNG
#png("reglas_grouped.png", width = 800, height = 600)
#plot(top_10_reglas, method = "grouped")
#dev.off()

# Guardar paracoord plot como PNG
#png("reglas_paracoord.png", width = 800, height = 600)
#plot(top_10_reglas, method = "paracoord")
#dev.off()


#g <- plot(top_10_reglas, method = "graph", engine = "htmlwidget")
#saveWidget(g, "reglas_graph_interactivo.html")

#6. Graficar las reglas 
png("reglas_grouped_ampliado_polonia.png", width = 1400, height = 800, res = 120)
plot(top_10_reglas, method = "grouped", control = list(k = 10), cex = 1.6)
dev.off()

# ---- REGLAS DE ASOCIACIÓN (República Checa, blusas) ----

# 1. Filtro
df_chequia_blusas <- df %>%
  filter(pais == "República Checa", categoria == "blusas") %>%
  select(sesión_ID, modelo_ropa)

# 2. Transacciones
transacciones_chequia <- as(split(df_chequia_blusas$modelo_ropa, df_chequia_blusas$sesión_ID), "transactions")
transacciones_chequia

# 3. Reglas
reglas_chequia <- apriori(transacciones_chequia,
                          parameter = list(supp = 0.04, conf = 0.25, target = "rules"))

# 4. Selección top 10
reglas_chequia_ordenadas <- sort(reglas_chequia, by = "support", decreasing = TRUE)
top_10_reglas_chequia <- reglas_chequia_ordenadas[1:min(10, length(reglas_chequia_ordenadas))]

# 5. Inspección
inspect(top_10_reglas_chequia)


#g_chequia <- plot(top_10_reglas_chequia, method = "graph", engine = "htmlwidget")
#saveWidget(g_chequia, "reglas_chequia_graph_interactivo.html")

# 6. Gráficar las reglas 
plot(top_10_reglas_chequia, method = "grouped", control = list(k = 10))
png("reglas_grouped_ampliado.png", width = 1400, height = 800, res = 120)
plot(top_10_reglas_chequia, method = "grouped", control = list(k = 10), cex = 1.6)
dev.off()

#-------- Secuencias más frecuentes con un soporte mayor a 2%-------

# Agrupar por sesión y orden de click
df_seq_grouped <- df %>%
  group_by(sesión_ID, clicks_sesion) %>%
  summarise(items = paste(unique(modelo_ropa), collapse = ","), .groups = "drop")

# Guardar en archivo
write.table(df_seq_grouped, "secuencias.txt", sep = " ", 
            row.names = FALSE, col.names = FALSE, quote = FALSE)

# Leer como secuencias
secuencias <- read_baskets("secuencias.txt", info = c("sequenceID", "eventID"))

# Verificar estructura
summary(secuencias)

# Ejecutar cSPADE
secuencias_frecuentes <- cspade(secuencias, 
                                parameter = list(support = 0.02, maxlen = 10), 
                                control = list(verbose = TRUE))

# Filtrar secuencias con más de un ítem
secuencias_filtradas <- subset(secuencias_frecuentes, size(secuencias_frecuentes) > 1)

# Mostrar las 10 más frecuentes
inspect(head(sort(secuencias_filtradas, by = "support", decreasing = TRUE), 10))

df_secuencias <- as(secuencias_filtradas, "data.frame")

# Agregar una columna con las secuencias como texto plano
df_secuencias$sequence <- labels(secuencias_filtradas)

# Gráfico de barras con top 10
top10 <- df_secuencias %>%
  arrange(desc(support)) %>%
  slice(1:5)

ggplot(top10, aes(x = reorder(sequence, support), y = support)) +
  geom_col(fill = "#001F3F") +
  coord_flip() +
  labs(
    title = "Secuencias frecuentes con soporte mayor al 2%",
    x="",#x= "Secuencia",
    y="",#y = "Soporte",
    caption = "Fuente: elaboración propia"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_blank(),       # Quita líneas principales
    panel.grid.minor = element_blank(),       # Quita líneas menores
    axis.line = element_line(color = "black", size = 0.5),
    plot.title = element_text(color = "#00008B", size = 16, face = "bold"),
    plot.caption = element_text(size = 10, hjust = 0, color = "gray40")
  )





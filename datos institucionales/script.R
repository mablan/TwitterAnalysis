# Este script reproduce y cambia de ser necesario el documento de "Reproduciendo:
# Text mining con R: ejemplo práctico Twitter de Amat Rodrigo"

library(rtweet)                       # Twitter API 
library(tidyverse)                    # Data manipulation
library(stringr)                      # String manipulation
library(knitr)                        # Report generation
library(tidytext)                     # text mining, Silge package
library(stopwords)                    # to clean up text
library(lubridate)                    # to ease date manipulation
library(wordcloud)                    # to make word clouds
library(RColorBrewer)                 # nice colors
library(gridExtra)                    # For graph related to
library(scales)                       # ...correlations
library(readxl)


# Leer los tweets almacenados 
tweets_corpoelecinfo1<- read_csv(file = "datos_tweets_@CORPOELECinfo 17 de mayo.csv",
                                col_names = TRUE)
tweets_corpoelecinfo1 = tweets_corpoelecinfo1 %>% select(status_id,created_at,user_id,  screen_name,text,source,                
                                  is_quote, is_retweet,favorite_count,retweet_count)

tweets_corpoelecinfo2<- read_csv(file = "datos_tweets_@CORPOELECinfo 25 de agosto.csv",
                                 col_names = TRUE)

tweets_corpoelecmerida <- read_csv(file = "datos_tweets_@corpoelecmerida 29 de mayo.csv",
                                   col_names = TRUE)
tweets_corpoeleczulia <- read_csv(file= "datos_tweets_@CorpoelecZulia_27 de agosto.csv",
                                  col_names = TRUE)

tweets_lmottad <- read_csv(file = "datos_tweets_@LMOTTAD 27 de agosto.csv",
                              col_names = TRUE)


# Para trabajar con lo que ya se tiene es a partir de aqui:

# Se unen todos los tweets en un único dataframe
tweetsA <- bind_rows(tweets_corpoelecinfo1, tweets_corpoelecinfo2, tweets_corpoelecmerida, 
                     tweets_corpoeleczulia,tweets_lmottad)
tweetsA %>% group_by(screen_name) %>% summarise(numero_tweets = n())
# Selección de variables: autor, fecha, texto, id:

tweetsA <- tweetsA %>% select(screen_name, created_at, status_id, text) %>%
  mutate(created_at = ymd_hms(created_at))

#Se muestra el numero de tweets muestreados a lo largo del tiempo
ggplot(tweetsA, aes(x = created_at, fill = screen_name)) +
  geom_histogram(position = "identity", bins = 20, show.legend = FALSE) +
  facet_wrap(~screen_name, ncol = 1)

#Se selecciona solo los del 2018
tweetsA1 <- tweetsA %>% filter(year(created_at)==2018)

# y se repite el gráfico anterior
ggplot(tweetsA1, aes(x = created_at, fill = screen_name)) +
  geom_histogram(position = "identity", bins = 20, show.legend = FALSE) +
  facet_wrap(~screen_name, ncol = 1)

# Tabla con número de tweets
tweetsA1 %>% group_by(screen_name) %>% summarise(numero_tweets = n())

# Se renombran las variables con nombres más prácticos
tweetsA1 <- tweetsA1 %>% rename(autor = screen_name, fecha = created_at,
                            texto = text, tweet_id = status_id)
head(tweetsA1)

# Esto se hace para limpiar el texto de patrones no informativos
# Ver explicacion en la p. 8 de Amat

#Vamos a hacerlo segun la función de Amat primero y luego segun la función del
#libro de Silge

limpiar_tokenizar <- function(texto){
  # El orden de la limpieza no es arbitrario
  # Se convierte todo el texto a minúsculas
  nuevo_texto <- tolower(texto)
  # Eliminación de páginas web (palabras que empiezan por "http." seguidas
  # de cualquier cosa que no sea un espacio)
  nuevo_texto <- str_replace_all(nuevo_texto,"http\\S*", "")
  # Eliminación de signos de puntuación
  nuevo_texto <- str_replace_all(nuevo_texto,"[[:punct:]]", " ")
  # Eliminación de números
  nuevo_texto <- str_replace_all(nuevo_texto,"[[:digit:]]", " ")
  # Eliminación de espacios en blanco múltiples
  nuevo_texto <- str_replace_all(nuevo_texto,"[\\s]+", " ")
  # Tokenización por palabras individuales
  nuevo_texto <- str_split(nuevo_texto, " ")[[1]]
  # Eliminación de tokens con una longitud < 2
  nuevo_texto <- keep(.x = nuevo_texto, .p = function(x){str_length(x) > 1})
  return(nuevo_texto)
}

# Este pequeno ejemplo sirve para ver lo que hace la función
test = "Esto es 1 ejemplo de l'limpieza de6 TEXTO
@JoaquinAmatRodrigo #textmining"
limpiar_tokenizar(texto = test)

# Ahora, se aplica la función de limpieza y tokenización a cada tweet
# con lo cual cada tweet se descompone en sus palabras y esto es lo que 
# se almacena a continuacion
tweetsA1 <- tweetsA1 %>% mutate(texto_tokenizado = map(.x = texto,
                                                   .f = limpiar_tokenizar))
tweetsA1 %>% select(texto_tokenizado) %>% head()
tweetsA1 %>% slice(1) %>% select(texto_tokenizado) %>% pull()

tweets_tidyA1 <- tweetsA1 %>% select(-texto) %>% unnest()
tweets_tidyA1 <- tweets_tidyA1 %>% rename(token = texto_tokenizado)
head(tweets_tidyA1)

# Este grafico explora la distribucion temporal de los tweets para los
# diferentes usuarios
ggplot(tweets_tidyA1, aes(x = as.Date(fecha), fill = autor)) +
  geom_histogram(position = "identity", bins = 20, show.legend = FALSE) +
  scale_x_date(date_labels = "%m-%Y", date_breaks = "2 week") +
  labs(x = "fecha de publicación", y = "número de tweets") +
  facet_wrap(~ autor, ncol = 1) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))

# Como LMOTTAD usa el twitter menos que CORPOELECinfo y corpoelecmerida,
# por la forma en que se recuperaron los tweets hay que recuperar mas tweets de
# las otras dos cuentas si se quiere cubrir mas o menos el mismo lapso temporal
# Quizas tratar de cubrir 2018 para todos esta bien.

# Otra forma de visualizarlo:
tweets_mes_anyo <- tweets_tidyA1 %>% mutate(mes_anyo = format(fecha, "%Y-%m"))
tweets_mes_anyo %>% group_by(autor, mes_anyo) %>% summarise(n = n()) %>%
  ggplot(aes(x = mes_anyo, y = n, color = autor)) +
  geom_line(aes(group = autor)) +
  labs(title = "Número de tweets publicados", x = "fecha de publicación",
       y = "número de tweets") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, size = 6),
        legend.position = "bottom")

# Ahora se estudia la frecuencia de palabras utilizada por cada usuario
tweets_tidyA1 %>% group_by(autor) %>% summarise(n = n())
tweets_tidyA1 %>%
  ggplot(aes(x = autor)) + geom_bar() + coord_flip() + theme_bw()

# Estas son frecuencia de palabras distintas por cada usuario:
tweets_tidyA1 %>% select(autor, token) %>% distinct() %>% group_by(autor) %>%
  summarise(palabras_distintas = n())
tweets_tidyA1 %>% select(autor, token) %>% distinct() %>%
  ggplot(aes(x = autor)) + geom_bar() + coord_flip() + theme_bw()

# Notar los cambios de frecuencia entre palabras totales y únicas por usuario

# Longitud media de tweets por usuario
tweets_tidyA1 %>% group_by(autor, tweet_id) %>% summarise(longitud = n()) %>%
group_by(autor) %>% summarise(media_longitud = mean(longitud), sd_longitud = sd(longitud))
tweets_tidyA1 %>% group_by(autor, tweet_id) %>% summarise(longitud = n()) %>%
  group_by(autor) %>%
  summarise(media_longitud = mean(longitud),
            sd_longitud = sd(longitud)) %>%
  ggplot(aes(x = autor, y = media_longitud)) +
  geom_col() +
  geom_errorbar(aes(ymin = media_longitud - sd_longitud,
                    ymax = media_longitud + sd_longitud)) +
  coord_flip() + theme_bw()

# Palabras mas utilizadas por usuario:
tweets_tidyA1 %>% group_by(autor, token) %>% count(token) %>% group_by(autor) %>%
  top_n(10, n) %>% arrange(autor, desc(n)) %>% print(n=30)

# Como se ve, esto incluye un monton de palabras no interesantes: articulos,
# preposiciones, etc. que es necesario quitar usando stopwords

# Esto es una lista de palabras a excluir del analisis
lista_stopwords <-stopwords("es")

# De nuevo, las palabras mas usadas pero excluyendo las no interesantes
tweets_tidyA1 <- tweets_tidyA1 %>% filter(!(token %in% lista_stopwords))
tweets_tidyA1 %>% group_by(autor, token) %>% count(token) %>% group_by(autor) %>%
  top_n(10, n) %>% arrange(autor, desc(n)) %>%
  ggplot(aes(x = reorder(token,n), y = n, fill = autor)) +
  geom_col() +
  theme_bw() +
  labs(y = "", x = "") +
  theme(legend.position = "none") +
  coord_flip() +
  facet_wrap(~autor,scales = "free", ncol = 1, drop = TRUE)

# si se excluye rt que no se porque se cuela aqui:
lista_stopwords <- c(lista_stopwords, "rt","regranned","from")

# y repetimos de nuevo el grafico anterior
tweets_tidyA1 <- tweets_tidyA1 %>% filter(!(token %in% lista_stopwords))
tweets_tidyA1 %>% group_by(autor, token) %>% count(token) %>% group_by(autor) %>%
  top_n(10, n) %>% arrange(autor, desc(n)) %>%
  ggplot(aes(x = reorder(token,n), y = n, fill = autor)) +
  geom_col() +
  theme_bw() +
  labs(y = "", x = "") +
  theme(legend.position = "none") +
  coord_flip() +
  facet_wrap(~autor,scales = "free", ncol = 1, drop = TRUE)

# Esto es uno de los resultados interesantes a analizar
# Mensajes plantilla
# Mensajes fijados

# Nubes de palabras, otra forma de visualizar lo anterior
wordcloud_custom <- function(grupo, df){
  print(grupo)
  wordcloud(words = df$token, freq = df$frecuencia,
            max.words = 400, random.order = FALSE, rot.per = 0.35,
            colors = brewer.pal(8, "Dark2"))
}
df_grouped <- tweets_tidyA1 %>% group_by(autor, token) %>% count(token) %>%
  group_by(autor) %>% mutate(frecuencia = n / n()) %>%
  arrange(autor, desc(frecuencia)) %>% nest()
walk2(.x = df_grouped$autor, .y = df_grouped$data, .f = wordcloud_custom)

#Correlacion entre usuarios

tweets_spread <- tweets_tidyA1 %>% group_by(autor, token) %>% count(token) %>%
  spread(key = autor, value = n, fill = NA, drop = TRUE)

cor.test(~ CORPOELECinfo + corpoelecmerida, method = "pearson", data = tweets_spread)
cor.test(~ CorpoelecZulia_ + CORPOELECinfo, method = "pearson", data = tweets_spread)

cor.test(~ CORPOELECinfo + LMOTTAD, method = "pearson", data = tweets_spread)
cor.test(~ corpoelecmerida + LMOTTAD, method = "pearson", data = tweets_spread)
cor.test(~ CorpoelecZulia_ + LMOTTAD, method = "pearson", data = tweets_spread)

cor.test(~ CorpoelecZulia_ + corpoelecmerida, method = "pearson", data = tweets_spread)

# Rev Interpreta Corpoelecmerida y CORPOELECinfo estan mas correlacionadas entre si que ellas con 
# LMOTTAD

# Grafico de comparación de frequencia de palabras usadas por cada usuario en
# referencia a Corpoelecinfo

p1 <- ggplot(tweets_spread, aes(corpoelecmerida, CORPOELECinfo)) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = token), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red") +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank())

p2 <- ggplot(tweets_spread, aes(LMOTTAD,CORPOELECinfo)) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = token), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red") +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank())

p3 <- ggplot(tweets_spread, aes(CorpoelecZulia_,CORPOELECinfo)) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = token), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red") +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank())


grid.arrange(p1,p3,p2, nrow = 1)


#Número de palabras comunes entre los usuarios
palabras_comunes <- dplyr::intersect(tweets_tidyA1 %>% filter(autor=="CORPOELECinfo") %>%
                                       select(token), tweets_tidyA1 %>% filter(autor=="corpoelecmerida") %>%
                                       select(token)) %>% nrow()
paste("Número de palabras comunes entre CORPOELECinfo y corpoelecmerida", palabras_comunes)

palabras_comunes <- dplyr::intersect(tweets_tidyA1 %>% filter(autor=="CORPOELECinfo") %>%
                                       select(token), tweets_tidyA1 %>% filter(autor=="CorpoelecZulia_") %>%
                                       select(token)) %>% nrow()
paste("Número de palabras comunes entre CORPOELECinfo y CorpoelecZulia_", palabras_comunes)



palabras_comunes <- dplyr::intersect(tweets_tidyA1 %>% filter(autor=="CORPOELECinfo") %>%
                                       select(token), tweets_tidyA1 %>% filter(autor=="LMOTTAD") %>%
                                       select(token)) %>% nrow()
paste("Número de palabras comunes entre CORPOELECinfo y LMOTTAD", palabras_comunes)

palabras_comunes <- dplyr::intersect(tweets_tidyA1 %>% filter(autor=="corpoelecmerida") %>%
                                       select(token), tweets_tidyA1 %>% filter(autor=="LMOTTAD") %>%
                                       select(token)) %>% nrow()
paste("Número de palabras comunes entre corpoelecmerida y LMOTTAD", palabras_comunes)

# Comparación de uso de las palabras:
# Pivotaje y despivotaje,
tweets_spread <- tweets_tidyA1 %>% group_by(autor, token) %>% count(token) %>%
  spread(key = autor, value = n, fill = 0, drop = TRUE)
tweets_unpivot <- tweets_spread %>% gather(key = "autor", value = "n", -token)

# Selección de los autores corpoelecmerida y CORPOELECinfo
tweets_unpivot1 <- tweets_unpivot %>% filter(autor %in% c("corpoelecmerida",
                                                         "CORPOELECinfo"))
# Se añade el total de palabras de cada autor
tweets_unpivot1 <- tweets_unpivot1 %>% left_join(tweets_tidyA1 %>%
                                                 group_by(autor) %>%
                                                 summarise(N = n()),
                                               by = "autor")
# Cálculo de odds y log of odds de cada palabra
tweets_logOdds1 <- tweets_unpivot1 %>% mutate(odds = (n + 1) / (N + 1))
tweets_logOdds1 <- tweets_logOdds1 %>% select(autor, token, odds) %>%
  spread(key = autor, value = odds)
tweets_logOdds1 <- tweets_logOdds1 %>% mutate(log_odds = log(corpoelecmerida/CORPOELECinfo),
                                            abs_log_odds = abs(log_odds))
# Si el logaritmo de odds es mayor que cero, significa que es una palabra con
# mayor probabilidad de ser de corpoelecmerida. Esto es así porque el ratio sea ha
# calculado como corpoelecmerida/CORPOELECinfo.
tweets_logOdds1 <- tweets_logOdds1 %>%
  mutate(autor_frecuente = if_else(log_odds > 0,
                                   "@corpoelecmerida",
                                   "@CORPOELECinfo"))
tweets_logOdds1 %>% arrange(desc(abs_log_odds)) %>% head()

#Graficamente:
tweets_logOdds1 %>% group_by(autor_frecuente) %>% top_n(15, abs_log_odds) %>%
  ggplot(aes(x = reorder(token, log_odds), y = log_odds, fill = autor_frecuente)) +
  geom_col() +
  labs(x = "palabra", y = "log odds ratio (@corpoelecmerida / @CORPOELECinfo)") +
  coord_flip() +
  theme_bw()

# Misma cosa pero ahora entre corpoelecZulia_ y CORPOELECinfo
# Selección de los autores corpoelecZulia_ y CORPOELECinfo
tweets_unpivot2 <- tweets_unpivot %>% filter(autor %in% c("CorpoelecZulia_",
                                                          "CORPOELECinfo"))
# Se añade el total de palabras de cada autor
tweets_unpivot2 <- tweets_unpivot2 %>% left_join(tweets_tidyA1 %>%
                                                   group_by(autor) %>%
                                                   summarise(N = n()),
                                                 by = "autor")
# Cálculo de odds y log of odds de cada palabra
tweets_logOdds2 <- tweets_unpivot2 %>% mutate(odds = (n + 1) / (N + 1))
tweets_logOdds2 <- tweets_logOdds2 %>% select(autor, token, odds) %>%
  spread(key = autor, value = odds)
tweets_logOdds2 <- tweets_logOdds2 %>% mutate(log_odds = log(CorpoelecZulia_/CORPOELECinfo),
                                              abs_log_odds = abs(log_odds))
# Si el logaritmo de odds es mayor que cero, significa que es una palabra con
# mayor probabilidad de ser de corpoelecmerida. Esto es así porque el ratio sea ha
# calculado como corpoelecmerida/CORPOELECinfo.
tweets_logOdds2 <- tweets_logOdds2 %>%
  mutate(autor_frecuente = if_else(log_odds > 0,
                                   "@CorpoelecZulia_",
                                   "@CORPOELECinfo"))
tweets_logOdds2 %>% arrange(desc(abs_log_odds)) %>% head()

#Graficamente:
tweets_logOdds2 %>% group_by(autor_frecuente) %>% top_n(15, abs_log_odds) %>%
  ggplot(aes(x = reorder(token, log_odds), y = log_odds, fill = autor_frecuente)) +
  geom_col() +
  labs(x = "palabra", y = "log odds ratio (@CorpoelecZulia_ / @CORPOELECinfo)") +
  coord_flip() +
  theme_bw()

# De nuevo, misma cosa pero ahora entre LMOTTAD y CORPOELECinfo
# Selección de los autores LMOTTAD y CORPOELECinfo
tweets_unpivot3 <- tweets_unpivot %>% filter(autor %in% c("LMOTTAD",
                                                          "CORPOELECinfo"))
# Se añade el total de palabras de cada autor
tweets_unpivot3 <- tweets_unpivot3 %>% left_join(tweets_tidyA1 %>%
                                                   group_by(autor) %>%
                                                   summarise(N = n()),
                                                 by = "autor")
# Cálculo de odds y log of odds de cada palabra
tweets_logOdds3 <- tweets_unpivot3 %>% mutate(odds = (n + 1) / (N + 1))
tweets_logOdds3 <- tweets_logOdds3 %>% select(autor, token, odds) %>%
  spread(key = autor, value = odds)
tweets_logOdds3 <- tweets_logOdds3 %>% mutate(log_odds = log(LMOTTAD/CORPOELECinfo),
                                              abs_log_odds = abs(log_odds))
# Si el logaritmo de odds es mayor que cero, significa que es una palabra con
# mayor probabilidad de ser de corpoelecmerida. Esto es así porque el ratio sea ha
# calculado como corpoelecmerida/CORPOELECinfo.
tweets_logOdds3 <- tweets_logOdds3 %>%
  mutate(autor_frecuente = if_else(log_odds > 0,
                                   "@LMOTTAD",
                                   "@CORPOELECinfo"))
tweets_logOdds3 %>% arrange(desc(abs_log_odds)) %>% head()

#Graficamente:
tweets_logOdds3 %>% group_by(autor_frecuente) %>% top_n(15, abs_log_odds) %>%
  ggplot(aes(x = reorder(token, log_odds), y = log_odds, fill = autor_frecuente)) +
  geom_col() +
  labs(x = "palabra", y = "log odds ratio (@LMOTTAD / @CORPOELECinfo)") +
  coord_flip() +
  theme_bw()



# Relación entre palabras:
limpiar <- function(texto){
  # El orden de la limpieza no es arbitrario
  # Se convierte todo el texto a minúsculas
  nuevo_texto <- tolower(texto)
  # Eliminación de páginas web (palabras que empiezan por "http." seguidas
  # de cualquier cosa que no sea un espacio)
  nuevo_texto <- str_replace_all(nuevo_texto,"http\\S*", "")
  # Eliminación de signos de puntuación
  nuevo_texto <- str_replace_all(nuevo_texto,"[[:punct:]]", " ")
  # Eliminación de números
  nuevo_texto <- str_replace_all(nuevo_texto,"[[:digit:]]", " ")
  # Eliminación de espacios en blanco múltiples
  nuevo_texto <- str_replace_all(nuevo_texto,"[\\s]+", " ")
  return(nuevo_texto)
}
bigramas <- tweetsA1 %>% mutate(texto = limpiar(texto)) %>%
  select(texto) %>%
  unnest_tokens(input = texto, output = "bigrama",
                token = "ngrams",n = 2, drop = TRUE)
# Contaje de ocurrencias de cada bigrama
bigramas %>% count(bigrama, sort = TRUE)
# Separación de los bigramas
bigrams_separados <- bigramas %>% separate(bigrama, c("palabra1", "palabra2"),
                                           sep = " ")
head(bigrams_separados)
# Filtrado de los bigramas que contienen alguna stopword
bigrams_separados <- bigrams_separados %>%
  filter(!palabra1 %in% lista_stopwords) %>%
  filter(!palabra2 %in% lista_stopwords)
# Unión de las palabras para formar de nuevo los bigramas
bigramas <- bigrams_separados %>%
  unite(bigrama, palabra1, palabra2, sep = " ")
# Nuevo contaje para identificar los bigramas más frecuentes
bigramas %>% count(bigrama, sort = TRUE) %>% print(n = 20)

#Graficamente:

library(igraph)
library(ggraph)
graph <- bigramas %>%
  separate(bigrama, c("palabra1", "palabra2"), sep = " ") %>%
  count(palabra1, palabra2, sort = TRUE) %>%
  filter(n > 18) %>% graph_from_data_frame(directed = FALSE)
set.seed(123)
plot(graph, vertex.label.font = 2,
     vertex.label.color = "black",
     vertex.label.cex = 0.7, edge.color = "gray85")

ggraph(graph = graph) +
  geom_edge_link(colour = "gray70") +
  geom_node_text(aes(label = name), size = 4) +
  theme_bw()

# Análisis de sentimientos
# Leer la base de datos
sentiments <- read_excel("..\NRC-Emotion-Lexicon-v0.92-spanish.xlsx", 
                    col_types = c("text", "text", "numeric", 
                       "numeric", "blank", "blank", "blank", 
                       "blank", "blank", "blank", "blank", 
                       "blank"))

# Cambiar los nombres de las columnas 
names(sentiments) <- c("ingles","palabra","Positivo","Negativo")

# Calcular el sentimiento de cada palabra
sentimientos <-sentiments %>% mutate (valor = Positivo + -1*Negativo)

# Añadir el sentimiento a cada palabra en la lista de tweets
tweets_sent <- inner_join(x = tweets_tidy, y = sentimientos,
                          by = c("token" = "palabra"))

# Calcula el sentimiento promedio de cada tweet
tweets_sent %>% group_by(autor, tweet_id) %>%
  summarise(sentimiento_promedio = sum(valor)) %>%
  head()

# Calcula el porcentaje de positivos, negativos y neutros de tweets por autor
tweets_sent %>% group_by(autor, tweet_id) %>%
  summarise(sentimiento_promedio = sum(valor)) %>%
  group_by(autor) %>%
  summarise(positivos = 100 * sum(sentimiento_promedio > 0) / n(),
            neutros = 100 * sum(sentimiento_promedio == 0) / n(),
            negativos = 100 * sum(sentimiento_promedio < 0) / n())

# Hace un grafico de barras para ilustrar lo anterior
tweets_sent %>% group_by(autor, tweet_id) %>%
  summarise(sentimiento_promedio = sum(valor)) %>%
  group_by(autor) %>%
  summarise(positivos = 100*sum(sentimiento_promedio > 0) / n(),
            neutros = 100*sum(sentimiento_promedio == 0) / n(),
            negativos = 100*sum(sentimiento_promedio < 0) / n()) %>%
  ungroup() %>%
  gather(key = "sentimiento", value = "valor", -autor) %>%
  ggplot(aes(x = autor, y = valor, fill = sentimiento)) +
  geom_col(position = "dodge", color = "black") + coord_flip() +
  theme_bw()

# Muestra la evolucion temporal de los tweets
tweets_sent %>% mutate(anyo = year(fecha),
                      mes = month(fecha),
                      anyo_mes = ymd(paste(anyo, mes, sep="-"),truncated=2)) %>%
                      group_by(autor, anyo_mes) %>%
                      summarise(sentimiento = mean(valor)) %>%
                      ungroup() %>%
                      ggplot(aes(x = anyo_mes, y = sentimiento, color = autor)) +
                        geom_point() +
                        #geom_smooth() +
                        labs(x = "fecha de publicación") +
                        facet_wrap(~ autor, ncol = 1) +
                        theme_bw() +
                        theme(legend.position = "none")


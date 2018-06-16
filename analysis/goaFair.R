#####################################################
# Global Organization Assessment
# Proyecto FAIR
# Lic. Renato Vargas, M.Sc.
#####################################################
rm(list=ls())
setwd("/Users/renato/data/fair/analysis/")
# La base de datos
webdb <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vReABTDj2bwUC3KUwrfSXz669d-DosGJONKW6g_ZtBz6VL_b2lh31Mi3fC7TM3CaezvxrFuRcywSk-A/pub?gid=2064932869&single=true&output=csv"


# Cargamos paquetes importantes
library(tm)
library(wordcloud)
library(SnowballC)
library(openxlsx)
library(cluster)
library(fpc)
library(ggplot2)

# Leemos la base de datos del GOA
#fairdb <- as.data.frame(read.xlsx("fair.xlsx"))
fairdb <- as.data.frame(read.csv(webdb, stringsAsFactors = FALSE))

# Estructura para cambiar el texto de algunos elementos que cumplan con una condicion
# fairdb$PREGUNTA[autodiag$PREGUNTA=="Barrera de educación formación laboral emprendimiento"] <- "Barreras de formación laboral y emprendimiento"

# Extraemos los segmentos para posteriores analisis por grupo
segmentos <- unique(fairdb$Segmento.al.que.pertenece)

# Esto lo usaremos en el analisis por segmentos, pero no aun.
#for (i in 1:length(segmentos)){
# Separamos por segmento
# fairdb <- fairdb[ which(fairdb$Segmento.al.que.pertenece==segmentos[3]), ]

#####################################################
# Global Organization Assessment
# Proyecto FAIR
# Lic. Renato Vargas, M.Sc.
#####################################################
rm(list=ls())
setwd("/Users/renato/data/fair/analysis/")
# La base de datos
webdb <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vReABTDj2bwUC3KUwrfSXz669d-DosGJONKW6g_ZtBz6VL_b2lh31Mi3fC7TM3CaezvxrFuRcywSk-A/pub?gid=2064932869&single=true&output=csv"

# 1. ANALISIS PRELIMINAR (Basado en todas las respuestas)
# =======================================================

# 1.1. Entorno economico
# ----------------------

# Y nos quedamos solamente con las respuestas
txt <- fairdb$Entorno.económico

# Limpiamos el texto de puntuacion y palabras comunes como preposiciones y articulos
txt <- gsub("[[:punct:]]", "", txt)
txt <- gsub("[[:digit:]]", "", txt)
txt <- gsub("http\\w+", "", txt)

# Creamos un corpus (cuerpo de texto) para analizar
corpus <- Corpus(VectorSource(txt))
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removeWords, c(stopwords("spanish")))
corpus <- tm_map(corpus, removeWords, "solo")

# Corregimos errores de digitación y consolidamos palabras
# similares que se refieren a lo mismo en un solo término.
# Se restringe la búsqueda y reemplazo a lo que se encuentre 
# dentro de \\b para no reemplazar palabras derivadas de
# manera equivocada o inesperada. 

# for (j in seq(corpus)){
# 	 corpus[[j]] <- gsub("\\bpequeños productores\\b", "pequeños_productores", corpus[[j]])
#  }

# corpus <- tm_map(corpus, removeWords, "jóvenes")

# Eliminamos el espacio en blanco
corpus <- tm_map(corpus, stripWhitespace)
# corpus <- tm_map(corpus, PlainTextDocument)

# Creamos una matriz de terminos
dtm <- DocumentTermMatrix(corpus)
tdm <- TermDocumentMatrix(corpus)
#writeLines(as.character(corpus[[31]]))
m <- as.matrix(tdm)
wf <- sort(rowSums(m), decreasing=TRUE)
dm <- data.frame(word = names(wf), freq=wf)

# Graficamos una nube de palabras para inspección visual
set.seed(124)
pdf(file= "1_Entorno_Economico_Nube.pdf", bg="white")
wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"))
dev.off()

# Creamos una tabla de frecuencias
freq <- colSums(as.matrix(dtm))   
length(freq)  
ord <- order(freq)
wf <- data.frame(word=names(freq), freq=freq)
head(wf)

# Graficamos palabras que aparecen al menos 6 veces
# en formato histograma
p <- ggplot(subset(wf, freq>6), aes(word,freq)) + geom_bar(stat="identity") + theme_bw() + theme(axis.text.x=element_text(angle = 45, hjust = 1))  + labs(x = "Palabras") + ylab("Frecuencia")
pdf(file="1_Entorno_Economico_Histograma.pdf", bg="white")
print(p)
dev.off()

# Compactamos la matriz de análisis
# y graficamos los conglomerados
dtms <- removeSparseTerms(dtm, 0.93)
d <- dist(t(dtms), method="euclidian")
kfit <- kmeans(d, 3)
pdf(file="1_Entorno_Economico_cluster.pdf", bg="white")
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)
dev.off()

# Hacemos conglomerados por el metodo de Ward
# Graficamos un dendrograma para identificar 
# grupos estadísticamente independientes
fit <- hclust(d=d, method="ward.D")
pdf(file="1_Entorno_Economico_dendrograma.pdf", bg="white")
plot(fit, hang=-1, main=paste("Palabras ", sep=""), ylab="Niveles de Grupos", xlab="Grupos (Método de Ward)")
groups <- cutree(fit, k=7)
rect.hclust(fit, k=7, border="red")
dev.off()

# Extraemos las palabras relevantes que aparecen
# al menos 11 veces
corwords <- as.data.frame(subset(wf, freq>10))
corwords <- as.vector(corwords$word)

# Encontramos palabras con asociación más fuerte para
# cada una de las palabras que más se repiten
# poniendo un umbral mínimo de 75%.
# Esto significa que estas palabras aparecen juntas
# al menos 25% del tiempo.
asoc <- findAssocs(dtm, c(corwords), corlimit=0.60)

# Escribimos las tablas de correlaciones de cada
# una de las palabras relevantes a pestanas de 
# un Excel (corrPalabrasRelev.xlsx).

wb <- createWorkbook()
addWorksheet(wb, "dimensiones")
writeData(wb, "dimensiones", as.data.frame(dim(dtm), row.names=c("filas", "columnas")), rowNames=TRUE)
for (k in 1:length(asoc)) {
	a <- as.data.frame(asoc[k])
	addWorksheet(wb, names(a))
	writeData(wb, names(a), a, rowNames = TRUE)
}
saveWorkbook(wb, "1_Entorno_Economico_corr.xlsx", overwrite=TRUE)


# 1.2. Entorno politico
# ----------------------

# Y nos quedamos solamente con las respuestas
txt <- fairdb$Entorno.político

# Limpiamos el texto de puntuacion y palabras comunes como preposiciones y articulos
txt <- gsub("[[:punct:]]", "", txt)
txt <- gsub("[[:digit:]]", "", txt)
txt <- gsub("http\\w+", "", txt)

# Creamos un corpus (cuerpo de texto) para analizar
corpus <- Corpus(VectorSource(txt))
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removeWords, c(stopwords("spanish")))

# Removemos palabras 
corpus <- tm_map(corpus, removeWords, "proyecto")
corpus <- tm_map(corpus, removeWords, "proyectos")
corpus <- tm_map(corpus, removeWords, "tema")
corpus <- tm_map(corpus, removeWords, "temas")
corpus <- tm_map(corpus, removeWords, "política")
corpus <- tm_map(corpus, removeWords, "políticas")

# Corregimos errores de digitación y consolidamos palabras
# similares que se refieren a lo mismo en un solo término.
# Se restringe la búsqueda y reemplazo a lo que se encuentre 
# dentro de \\b para no reemplazar palabras derivadas de
# manera equivocada o inesperada. 

# for (j in seq(corpus)){
# 	 corpus[[j]] <- gsub("\\pequeños productores\\b", "pequeños_productores", corpus[[j]])
#  }

# corpus <- tm_map(corpus, removeWords, "jóvenes")

# Eliminamos el espacio en blanco
corpus <- tm_map(corpus, stripWhitespace)
# corpus <- tm_map(corpus, PlainTextDocument)

# Creamos una matriz de terminos
dtm <- DocumentTermMatrix(corpus)
tdm <- TermDocumentMatrix(corpus)
#writeLines(as.character(corpus[[31]]))
m <- as.matrix(tdm)
wf <- sort(rowSums(m), decreasing=TRUE)
dm <- data.frame(word = names(wf), freq=wf)

# Graficamos una nube de palabras para inspección visual
set.seed(124)
pdf(file= "2_Entorno_politico_nube.pdf", bg="white")
wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"))
dev.off()

# Creamos una tabla de frecuencias
freq <- colSums(as.matrix(dtm))   
length(freq)  
ord <- order(freq)
wf <- data.frame(word=names(freq), freq=freq)
head(wf)

# Graficamos palabras que aparecen al menos 6 veces
# en formato histograma
p <- ggplot(subset(wf, freq>6), aes(word,freq)) + geom_bar(stat="identity") + theme_bw() + theme(axis.text.x=element_text(angle = 45, hjust = 1))  + labs(x = "Palabras") + ylab("Frecuencia")
pdf(file="2_Entorno_politico_histograma.pdf", bg="white")
print(p)
dev.off()

# Compactamos la matriz de análisis
# y graficamos los conglomerados
dtms <- removeSparseTerms(dtm, 0.93)
d <- dist(t(dtms), method="euclidian")
kfit <- kmeans(d, 3)
pdf(file="2_Entorno_politico_cluster.pdf", bg="white")
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)
dev.off()

# Hacemos conglomerados por el metodo de Ward
# Graficamos un dendrograma para identificar 
# grupos estadísticamente independientes
fit <- hclust(d=d, method="ward.D")
pdf(file="2_Entorno_politico_dendrograma.pdf", bg="white")
plot(fit, hang=-1, main=paste("Palabras ", sep=""), ylab="Niveles de Grupos", xlab="Grupos (Método de Ward)")
groups <- cutree(fit, k=7)
rect.hclust(fit, k=7, border="red")
dev.off()

# Extraemos las palabras relevantes que aparecen
# al menos 11 veces
corwords <- as.data.frame(subset(wf, freq>10))
corwords <- as.vector(corwords$word)

# Encontramos palabras con asociación más fuerte para
# cada una de las palabras que más se repiten
# poniendo un umbral mínimo de 75%.
# Esto significa que estas palabras aparecen juntas
# al menos 25% del tiempo.
asoc <- findAssocs(dtm, c(corwords), corlimit=0.60)

# Escribimos las tablas de correlaciones de cada
# una de las palabras relevantes a pestanas de 
# un Excel (corrPalabrasRelev.xlsx).

wb <- createWorkbook()
addWorksheet(wb, "dimensiones")
writeData(wb, "dimensiones", as.data.frame(dim(dtm), row.names=c("filas", "columnas")), rowNames=TRUE)
for (k in 1:length(asoc)) {
  a <- as.data.frame(asoc[k])
  addWorksheet(wb, names(a))
  writeData(wb, names(a), a, rowNames = TRUE)
}
saveWorkbook(wb, "2_Entorno_politico_corr.xlsx", overwrite=TRUE)

# 1.3. Entorno social
# ----------------------

# Y nos quedamos solamente con las respuestas
txt <- fairdb$Entorno.social

# Limpiamos el texto de puntuacion y palabras comunes como preposiciones y articulos
txt <- gsub("[[:punct:]]", "", txt)
txt <- gsub("[[:digit:]]", "", txt)
txt <- gsub("http\\w+", "", txt)

# Creamos un corpus (cuerpo de texto) para analizar
corpus <- Corpus(VectorSource(txt))
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removeWords, c(stopwords("spanish")))
# corpus <- tm_map(corpus, removeWords, "eis")

# Corregimos errores de digitación y consolidamos palabras
# similares que se refieren a lo mismo en un solo término.
# Se restringe la búsqueda y reemplazo a lo que se encuentre 
# dentro de \\b para no reemplazar palabras derivadas de
# manera equivocada o inesperada. 

# for (j in seq(corpus)){
# 	 corpus[[j]] <- gsub("\\pequeños productores\\b", "pequeños_productores", corpus[[j]])
#  }

# corpus <- tm_map(corpus, removeWords, "jóvenes")

# Eliminamos el espacio en blanco
corpus <- tm_map(corpus, stripWhitespace)
# corpus <- tm_map(corpus, PlainTextDocument)

# Creamos una matriz de terminos
dtm <- DocumentTermMatrix(corpus)
tdm <- TermDocumentMatrix(corpus)
#writeLines(as.character(corpus[[31]]))
m <- as.matrix(tdm)
wf <- sort(rowSums(m), decreasing=TRUE)
dm <- data.frame(word = names(wf), freq=wf)

# Graficamos una nube de palabras para inspección visual
set.seed(124)
pdf(file= "3_Entorno_social_nube.pdf", bg="white")
wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"))
dev.off()

# Creamos una tabla de frecuencias
freq <- colSums(as.matrix(dtm))   
length(freq)  
ord <- order(freq)
wf <- data.frame(word=names(freq), freq=freq)
head(wf)

# Graficamos palabras que aparecen al menos 6 veces
# en formato histograma
p <- ggplot(subset(wf, freq>6), aes(word,freq)) + geom_bar(stat="identity") + theme_bw() + theme(axis.text.x=element_text(angle = 45, hjust = 1))  + labs(x = "Palabras") + ylab("Frecuencia")
pdf(file="3_Entorno_social_histograma.pdf", bg="white")
print(p)
dev.off()

# Compactamos la matriz de análisis
# y graficamos los conglomerados
dtms <- removeSparseTerms(dtm, 0.90)
d <- dist(t(dtms), method="euclidian")
kfit <- kmeans(d, 3)
pdf(file="3_Entorno_social_cluster.pdf", bg="white")
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)
dev.off()

# Hacemos conglomerados por el metodo de Ward
# Graficamos un dendrograma para identificar 
# grupos estadísticamente independientes
fit <- hclust(d=d, method="ward.D")
pdf(file="3_Entorno_social_dendrograma.pdf", bg="white")
plot(fit, hang=-1, main=paste("Palabras ", sep=""), ylab="Niveles de Grupos", xlab="Grupos (Método de Ward)")
groups <- cutree(fit, k=7)
rect.hclust(fit, k=7, border="red")
dev.off()

# Extraemos las palabras relevantes que aparecen
# al menos 11 veces
corwords <- as.data.frame(subset(wf, freq>10))
corwords <- as.vector(corwords$word)

# Encontramos palabras con asociación más fuerte para
# cada una de las palabras que más se repiten
# poniendo un umbral mínimo de 75%.
# Esto significa que estas palabras aparecen juntas
# al menos 25% del tiempo.
asoc <- findAssocs(dtm, c(corwords), corlimit=0.60)

# Escribimos las tablas de correlaciones de cada
# una de las palabras relevantes a pestanas de 
# un Excel (corrPalabrasRelev.xlsx).

wb <- createWorkbook()
addWorksheet(wb, "dimensiones")
writeData(wb, "dimensiones", as.data.frame(dim(dtm), row.names=c("filas", "columnas")), rowNames=TRUE)
for (k in 1:length(asoc)) {
  a <- as.data.frame(asoc[k])
  addWorksheet(wb, names(a))
  writeData(wb, names(a), a, rowNames = TRUE)
}
saveWorkbook(wb, "3_Entorno_social_corr.xlsx", overwrite=TRUE)

# 1.4. Entorno tecnologico
# ------------------------

# Y nos quedamos solamente con las respuestas
txt <- fairdb$Entorno.tecnológico

# Limpiamos el texto de puntuacion y palabras comunes como preposiciones y articulos
txt <- gsub("[[:punct:]]", "", txt)
txt <- gsub("[[:digit:]]", "", txt)
txt <- gsub("http\\w+", "", txt)

# Creamos un corpus (cuerpo de texto) para analizar
corpus <- Corpus(VectorSource(txt))
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removeWords, c(stopwords("spanish")))
# corpus <- tm_map(corpus, removeWords, "eis")

# Corregimos errores de digitación y consolidamos palabras
# similares que se refieren a lo mismo en un solo término.
# Se restringe la búsqueda y reemplazo a lo que se encuentre 
# dentro de \\b para no reemplazar palabras derivadas de
# manera equivocada o inesperada. 

# for (j in seq(corpus)){
# 	 corpus[[j]] <- gsub("\\pequeños productores\\b", "pequeños_productores", corpus[[j]])
#  }

# corpus <- tm_map(corpus, removeWords, "jóvenes")

# Eliminamos el espacio en blanco
corpus <- tm_map(corpus, stripWhitespace)
# corpus <- tm_map(corpus, PlainTextDocument)

# Creamos una matriz de terminos
dtm <- DocumentTermMatrix(corpus)
tdm <- TermDocumentMatrix(corpus)
#writeLines(as.character(corpus[[31]]))
m <- as.matrix(tdm)
wf <- sort(rowSums(m), decreasing=TRUE)
dm <- data.frame(word = names(wf), freq=wf)

# Graficamos una nube de palabras para inspección visual
set.seed(124)
pdf(file= "4_Entorno_tecnologico_nube.pdf", bg="white")
wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"))
dev.off()

# Creamos una tabla de frecuencias
freq <- colSums(as.matrix(dtm))   
length(freq)  
ord <- order(freq)
wf <- data.frame(word=names(freq), freq=freq)
head(wf)

# Graficamos palabras que aparecen al menos 6 veces
# en formato histograma
p <- ggplot(subset(wf, freq>6), aes(word,freq)) + geom_bar(stat="identity") + theme_bw() + theme(axis.text.x=element_text(angle = 45, hjust = 1))  + labs(x = "Palabras") + ylab("Frecuencia")
pdf(file="4_Entorno_tecnologico_histograma.pdf", bg="white")
print(p)
dev.off()

# Compactamos la matriz de análisis
# y graficamos los conglomerados
dtms <- removeSparseTerms(dtm, 0.90)
d <- dist(t(dtms), method="euclidian")
kfit <- kmeans(d, 3)
pdf(file="4_Entorno_tecnologico_cluster.pdf", bg="white")
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)
dev.off()

# Hacemos conglomerados por el metodo de Ward
# Graficamos un dendrograma para identificar 
# grupos estadísticamente independientes
fit <- hclust(d=d, method="ward.D")
pdf(file="4_Entorno_tecnologico_dendrograma.pdf", bg="white")
plot(fit, hang=-1, main=paste("Palabras ", sep=""), ylab="Niveles de Grupos", xlab="Grupos (Método de Ward)")
groups <- cutree(fit, k=7)
rect.hclust(fit, k=7, border="red")
dev.off()

# Extraemos las palabras relevantes que aparecen
# al menos 11 veces
corwords <- as.data.frame(subset(wf, freq>10))
corwords <- as.vector(corwords$word)

# Encontramos palabras con asociación más fuerte para
# cada una de las palabras que más se repiten
# poniendo un umbral mínimo de 75%.
# Esto significa que estas palabras aparecen juntas
# al menos 25% del tiempo.
asoc <- findAssocs(dtm, c(corwords), corlimit=0.60)

# Escribimos las tablas de correlaciones de cada
# una de las palabras relevantes a pestanas de 
# un Excel (corrPalabrasRelev.xlsx).

wb <- createWorkbook()
addWorksheet(wb, "dimensiones")
writeData(wb, "dimensiones", as.data.frame(dim(dtm), row.names=c("filas", "columnas")), rowNames=TRUE)
for (k in 1:length(asoc)) {
  a <- as.data.frame(asoc[k])
  addWorksheet(wb, names(a))
  writeData(wb, names(a), a, rowNames = TRUE)
}
saveWorkbook(wb, "4_Entorno_tecnologico_corr.xlsx", overwrite=TRUE)


# 1.5. Entorno ambiental
# ------------------------

# Y nos quedamos solamente con las respuestas
txt <- fairdb$Entorno.ambiental

# Limpiamos el texto de puntuacion y palabras comunes como preposiciones y articulos
txt <- gsub("[[:punct:]]", "", txt)
txt <- gsub("[[:digit:]]", "", txt)
txt <- gsub("http\\w+", "", txt)

# Creamos un corpus (cuerpo de texto) para analizar
corpus <- Corpus(VectorSource(txt))
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removeWords, c(stopwords("spanish")))
# corpus <- tm_map(corpus, removeWords, "eis")

# Corregimos errores de digitación y consolidamos palabras
# similares que se refieren a lo mismo en un solo término.
# Se restringe la búsqueda y reemplazo a lo que se encuentre 
# dentro de \\b para no reemplazar palabras derivadas de
# manera equivocada o inesperada. 

# for (j in seq(corpus)){
# 	 corpus[[j]] <- gsub("\\pequeños productores\\b", "pequeños_productores", corpus[[j]])
#  }

# corpus <- tm_map(corpus, removeWords, "jóvenes")

# Eliminamos el espacio en blanco
corpus <- tm_map(corpus, stripWhitespace)
# corpus <- tm_map(corpus, PlainTextDocument)

# Creamos una matriz de terminos
dtm <- DocumentTermMatrix(corpus)
tdm <- TermDocumentMatrix(corpus)
#writeLines(as.character(corpus[[31]]))
m <- as.matrix(tdm)
wf <- sort(rowSums(m), decreasing=TRUE)
dm <- data.frame(word = names(wf), freq=wf)

# Graficamos una nube de palabras para inspección visual
set.seed(124)
pdf(file= "5_Entorno_ambiental_nube.pdf", bg="white")
wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"))
dev.off()

# Creamos una tabla de frecuencias
freq <- colSums(as.matrix(dtm))   
length(freq)  
ord <- order(freq)
wf <- data.frame(word=names(freq), freq=freq)
head(wf)

# Graficamos palabras que aparecen al menos 6 veces
# en formato histograma
p <- ggplot(subset(wf, freq>6), aes(word,freq)) + geom_bar(stat="identity") + theme_bw() + theme(axis.text.x=element_text(angle = 45, hjust = 1))  + labs(x = "Palabras") + ylab("Frecuencia")
pdf(file="5_Entorno_ambiental_histograma.pdf", bg="white")
print(p)
dev.off()

# Compactamos la matriz de análisis
# y graficamos los conglomerados
dtms <- removeSparseTerms(dtm, 0.90)
d <- dist(t(dtms), method="euclidian")
kfit <- kmeans(d, 3)
pdf(file="5_Entorno_ambiental_cluster.pdf", bg="white")
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)
dev.off()

# Hacemos conglomerados por el metodo de Ward
# Graficamos un dendrograma para identificar 
# grupos estadísticamente independientes
fit <- hclust(d=d, method="ward.D")
pdf(file="5_Entorno_ambiental_dendrograma.pdf", bg="white")
plot(fit, hang=-1, main=paste("Palabras ", sep=""), ylab="Niveles de Grupos", xlab="Grupos (Método de Ward)")
groups <- cutree(fit, k=7)
rect.hclust(fit, k=7, border="red")
dev.off()

# Extraemos las palabras relevantes que aparecen
# al menos 11 veces
corwords <- as.data.frame(subset(wf, freq>10))
corwords <- as.vector(corwords$word)

# Encontramos palabras con asociación más fuerte para
# cada una de las palabras que más se repiten
# poniendo un umbral mínimo de 75%.
# Esto significa que estas palabras aparecen juntas
# al menos 25% del tiempo.
asoc <- findAssocs(dtm, c(corwords), corlimit=0.60)

# Escribimos las tablas de correlaciones de cada
# una de las palabras relevantes a pestanas de 
# un Excel (corrPalabrasRelev.xlsx).

wb <- createWorkbook()
addWorksheet(wb, "dimensiones")
writeData(wb, "dimensiones", as.data.frame(dim(dtm), row.names=c("filas", "columnas")), rowNames=TRUE)
for (k in 1:length(asoc)) {
  a <- as.data.frame(asoc[k])
  addWorksheet(wb, names(a))
  writeData(wb, names(a), a, rowNames = TRUE)
}
saveWorkbook(wb, "4_Entorno_ambiental_corr.xlsx", overwrite=TRUE)

# 6. Estrategias de adaptacion
# ----------------------------

# Y nos quedamos solamente con las respuestas
txt <- fairdb$Estrategias

# Limpiamos el texto de puntuacion y palabras comunes como preposiciones y articulos
txt <- gsub("[[:punct:]]", "", txt)
txt <- gsub("[[:digit:]]", "", txt)
txt <- gsub("http\\w+", "", txt)

# Creamos un corpus (cuerpo de texto) para analizar
corpus <- Corpus(VectorSource(txt))
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removeWords, c(stopwords("spanish")))
corpus <- tm_map(corpus, removeWords, "temas")

# Corregimos errores de digitación y consolidamos palabras
# similares que se refieren a lo mismo en un solo término.
# Se restringe la búsqueda y reemplazo a lo que se encuentre 
# dentro de \\b para no reemplazar palabras derivadas de
# manera equivocada o inesperada. 

# for (j in seq(corpus)){
# 	 corpus[[j]] <- gsub("\\pequeños productores\\b", "pequeños_productores", corpus[[j]])
#  }

# corpus <- tm_map(corpus, removeWords, "jóvenes")

# Eliminamos el espacio en blanco
corpus <- tm_map(corpus, stripWhitespace)
# corpus <- tm_map(corpus, PlainTextDocument)

# Creamos una matriz de terminos
dtm <- DocumentTermMatrix(corpus)
tdm <- TermDocumentMatrix(corpus)
#writeLines(as.character(corpus[[31]]))
m <- as.matrix(tdm)
wf <- sort(rowSums(m), decreasing=TRUE)
dm <- data.frame(word = names(wf), freq=wf)

# Graficamos una nube de palabras para inspección visual
set.seed(124)
pdf(file= "6_Estrategias_nube.pdf", bg="white")
wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"))
dev.off()

# Creamos una tabla de frecuencias
freq <- colSums(as.matrix(dtm))   
length(freq)  
ord <- order(freq)
wf <- data.frame(word=names(freq), freq=freq)
head(wf)

# Graficamos palabras que aparecen al menos 6 veces
# en formato histograma
p <- ggplot(subset(wf, freq>6), aes(word,freq)) + geom_bar(stat="identity") + theme_bw() + theme(axis.text.x=element_text(angle = 45, hjust = 1))  + labs(x = "Palabras") + ylab("Frecuencia")
pdf(file="6_Estrategias_histograma.pdf", bg="white")
print(p)
dev.off()

# Compactamos la matriz de análisis
# y graficamos los conglomerados
dtms <- removeSparseTerms(dtm, 0.92)
d <- dist(t(dtms), method="euclidian")
kfit <- kmeans(d, 3)
pdf(file="6_Estrategias_cluster.pdf", bg="white")
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)
dev.off()

# Hacemos conglomerados por el metodo de Ward
# Graficamos un dendrograma para identificar 
# grupos estadísticamente independientes
fit <- hclust(d=d, method="ward.D")
pdf(file="6_Estrategias_dendrograma.pdf", bg="white")
plot(fit, hang=-1, main=paste("Palabras ", sep=""), ylab="Niveles de Grupos", xlab="Grupos (Método de Ward)")
groups <- cutree(fit, k=7)
rect.hclust(fit, k=7, border="red")
dev.off()

# Extraemos las palabras relevantes que aparecen
# al menos 11 veces
corwords <- as.data.frame(subset(wf, freq>10))
corwords <- as.vector(corwords$word)

# Encontramos palabras con asociación más fuerte para
# cada una de las palabras que más se repiten
# poniendo un umbral mínimo de 75%.
# Esto significa que estas palabras aparecen juntas
# al menos 25% del tiempo.
asoc <- findAssocs(dtm, c(corwords), corlimit=0.60)

# Escribimos las tablas de correlaciones de cada
# una de las palabras relevantes a pestanas de 
# un Excel (corrPalabrasRelev.xlsx).

wb <- createWorkbook()
addWorksheet(wb, "dimensiones")
writeData(wb, "dimensiones", as.data.frame(dim(dtm), row.names=c("filas", "columnas")), rowNames=TRUE)
for (k in 1:length(asoc)) {
  a <- as.data.frame(asoc[k])
  addWorksheet(wb, names(a))
  writeData(wb, names(a), a, rowNames = TRUE)
}
saveWorkbook(wb, "6_Estrategias_corr.xlsx", overwrite=TRUE)

# 7. Actividades en comun
# -----------------------

# Y nos quedamos solamente con las respuestas
txt <- fairdb$Actividades.en.común

# Limpiamos el texto de puntuacion y palabras comunes como preposiciones y articulos
txt <- gsub("[[:punct:]]", "", txt)
#txt <- gsub("[[:digit:]]", "", txt)
txt <- gsub("http\\w+", "", txt)

# Creamos un corpus (cuerpo de texto) para analizar
corpus <- Corpus(VectorSource(txt))
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removeWords, c(stopwords("spanish")))
# corpus <- tm_map(corpus, removeWords, "eis")

# Corregimos errores de digitación y consolidamos palabras
# similares que se refieren a lo mismo en un solo término.
# Se restringe la búsqueda y reemplazo a lo que se encuentre 
# dentro de \\b para no reemplazar palabras derivadas de
# manera equivocada o inesperada. 

# for (j in seq(corpus)){
# 	 corpus[[j]] <- gsub("\\pequeños productores\\b", "pequeños_productores", corpus[[j]])
#  }

# corpus <- tm_map(corpus, removeWords, "jóvenes")

# Eliminamos el espacio en blanco
corpus <- tm_map(corpus, stripWhitespace)
# corpus <- tm_map(corpus, PlainTextDocument)

# Creamos una matriz de terminos
dtm <- DocumentTermMatrix(corpus)
tdm <- TermDocumentMatrix(corpus)
#writeLines(as.character(corpus[[31]]))
m <- as.matrix(tdm)
wf <- sort(rowSums(m), decreasing=TRUE)
dm <- data.frame(word = names(wf), freq=wf)

# Graficamos una nube de palabras para inspección visual
set.seed(124)
pdf(file= "7_En_Comun_nube.pdf", bg="white")
wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"))
dev.off()

# Creamos una tabla de frecuencias
freq <- colSums(as.matrix(dtm))   
length(freq)  
ord <- order(freq)
wf <- data.frame(word=names(freq), freq=freq)
head(wf)

# Graficamos palabras que aparecen al menos 6 veces
# en formato histograma
p <- ggplot(subset(wf, freq>6), aes(word,freq)) + geom_bar(stat="identity") + theme_bw() + theme(axis.text.x=element_text(angle = 45, hjust = 1))  + labs(x = "Palabras") + ylab("Frecuencia")
pdf(file="7_En_Comun_histograma.pdf", bg="white")
print(p)
dev.off()

# Compactamos la matriz de análisis
# y graficamos los conglomerados
dtms <- removeSparseTerms(dtm, 0.90)
d <- dist(t(dtms), method="euclidian")
kfit <- kmeans(d, 3)
pdf(file="7_En_Comun_cluster.pdf", bg="white")
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)
dev.off()

# Hacemos conglomerados por el metodo de Ward
# Graficamos un dendrograma para identificar 
# grupos estadísticamente independientes
fit <- hclust(d=d, method="ward.D")
pdf(file="7_En_Comun_dendrograma.pdf", bg="white")
plot(fit, hang=-1, main=paste("Palabras ", sep=""), ylab="Niveles de Grupos", xlab="Grupos (Método de Ward)")
groups <- cutree(fit, k=7)
rect.hclust(fit, k=7, border="red")
dev.off()

# Extraemos las palabras relevantes que aparecen
# al menos 11 veces
corwords <- as.data.frame(subset(wf, freq>10))
corwords <- as.vector(corwords$word)

# Encontramos palabras con asociación más fuerte para
# cada una de las palabras que más se repiten
# poniendo un umbral mínimo de 75%.
# Esto significa que estas palabras aparecen juntas
# al menos 25% del tiempo.
asoc <- findAssocs(dtm, c(corwords), corlimit=0.60)

# Escribimos las tablas de correlaciones de cada
# una de las palabras relevantes a pestanas de 
# un Excel (corrPalabrasRelev.xlsx).

wb <- createWorkbook()
addWorksheet(wb, "dimensiones")
writeData(wb, "dimensiones", as.data.frame(dim(dtm), row.names=c("filas", "columnas")), rowNames=TRUE)
for (k in 1:length(asoc)) {
  a <- as.data.frame(asoc[k])
  addWorksheet(wb, names(a))
  writeData(wb, names(a), a, rowNames = TRUE)
}
saveWorkbook(wb, "7_En_Comun_corr.xlsx", overwrite=TRUE)

# 8. Obstaculos al participar en proyectos como este
# --------------------------------------------------

# Y nos quedamos solamente con las respuestas
txt <- fairdb$Si.su.respuesta.a.la.pregunta.anterior.es..Sí...cuáles.son.los.principales.obstáculos.que.enfrentó.

# Limpiamos el texto de puntuacion y palabras comunes como preposiciones y articulos
txt <- gsub("[[:punct:]]", "", txt)
txt <- gsub("[[:digit:]]", "", txt)
txt <- gsub("http\\w+", "", txt)

# Creamos un corpus (cuerpo de texto) para analizar
corpus <- Corpus(VectorSource(txt))
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removeWords, c(stopwords("spanish")))

# Removemos palabras que hacen ruido
corpus <- tm_map(corpus, removeWords, "proyecto")
corpus <- tm_map(corpus, removeWords, "proyectos")
corpus <- tm_map(corpus, removeWords, "ser")
corpus <- tm_map(corpus, removeWords, "solo")
corpus <- tm_map(corpus, removeWords, "temas")
corpus <- tm_map(corpus, removeWords, "falta")

# Corregimos errores de digitación y consolidamos palabras
# similares que se refieren a lo mismo en un solo término.
# Se restringe la búsqueda y reemplazo a lo que se encuentre 
# dentro de \\b para no reemplazar palabras derivadas de
# manera equivocada o inesperada. 

# for (j in seq(corpus)){
# 	 corpus[[j]] <- gsub("\\pequeños productores\\b", "pequeños_productores", corpus[[j]])
#  }

# corpus <- tm_map(corpus, removeWords, "jóvenes")

# Eliminamos el espacio en blanco
corpus <- tm_map(corpus, stripWhitespace)
# corpus <- tm_map(corpus, PlainTextDocument)

# Creamos una matriz de terminos
dtm <- DocumentTermMatrix(corpus)
tdm <- TermDocumentMatrix(corpus)
#writeLines(as.character(corpus[[31]]))
m <- as.matrix(tdm)
wf <- sort(rowSums(m), decreasing=TRUE)
dm <- data.frame(word = names(wf), freq=wf)

# Graficamos una nube de palabras para inspección visual
set.seed(124)
pdf(file= "8_Obstaculos_nube.pdf", bg="white")
wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"))
dev.off()

# Creamos una tabla de frecuencias
freq <- colSums(as.matrix(dtm))   
length(freq)  
ord <- order(freq)
wf <- data.frame(word=names(freq), freq=freq)
head(wf)

# Graficamos palabras que aparecen al menos 6 veces
# en formato histograma
p <- ggplot(subset(wf, freq>6), aes(word,freq)) + geom_bar(stat="identity") + theme_bw() + theme(axis.text.x=element_text(angle = 45, hjust = 1))  + labs(x = "Palabras") + ylab("Frecuencia")
pdf(file="8_Obstaculos_histograma.pdf", bg="white")
print(p)
dev.off()

# Compactamos la matriz de análisis
# y graficamos los conglomerados
dtms <- removeSparseTerms(dtm, 0.92)
d <- dist(t(dtms), method="euclidian")
kfit <- kmeans(d, 3)
pdf(file="8_Obstaculos_cluster.pdf", bg="white")
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)
dev.off()

# Hacemos conglomerados por el metodo de Ward
# Graficamos un dendrograma para identificar 
# grupos estadísticamente independientes
fit <- hclust(d=d, method="ward.D")
pdf(file="8_Obstaculos_dendrograma.pdf", bg="white")
plot(fit, hang=-1, main=paste("Palabras ", sep=""), ylab="Niveles de Grupos", xlab="Grupos (Método de Ward)")
groups <- cutree(fit, k=7)
rect.hclust(fit, k=7, border="red")
dev.off()

# Extraemos las palabras relevantes que aparecen
# al menos 11 veces
corwords <- as.data.frame(subset(wf, freq>10))
corwords <- as.vector(corwords$word)

# Encontramos palabras con asociación más fuerte para
# cada una de las palabras que más se repiten
# poniendo un umbral mínimo de 75%.
# Esto significa que estas palabras aparecen juntas
# al menos 25% del tiempo.
asoc <- findAssocs(dtm, c(corwords), corlimit=0.60)

# Escribimos las tablas de correlaciones de cada
# una de las palabras relevantes a pestanas de 
# un Excel (corrPalabrasRelev.xlsx).

wb <- createWorkbook()
addWorksheet(wb, "dimensiones")
writeData(wb, "dimensiones", as.data.frame(dim(dtm), row.names=c("filas", "columnas")), rowNames=TRUE)
for (k in 1:length(asoc)) {
  a <- as.data.frame(asoc[k])
  addWorksheet(wb, names(a))
  writeData(wb, names(a), a, rowNames = TRUE)
}
saveWorkbook(wb, "8_Obstaculos_corr.xlsx", overwrite=TRUE)


# 9. Oportunidades de mejora
# --------------------------

# Y nos quedamos solamente con las respuestas
txt <- fairdb$X.Cómo.se.podrían.mejorar.estos.proyectos..Oportunidades.de.mejora

# Limpiamos el texto de puntuacion y palabras comunes como preposiciones y articulos
txt <- gsub("[[:punct:]]", "", txt)
txt <- gsub("[[:digit:]]", "", txt)
txt <- gsub("http\\w+", "", txt)

# Creamos un corpus (cuerpo de texto) para analizar
corpus <- Corpus(VectorSource(txt))
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removeWords, c(stopwords("spanish")))

# Removemos palabras 
corpus <- tm_map(corpus, removeWords, "proyecto")
corpus <- tm_map(corpus, removeWords, "proyectos")
corpus <- tm_map(corpus, removeWords, "tener")
corpus <- tm_map(corpus, removeWords, "nivel")
corpus <- tm_map(corpus, removeWords, "hacer")
corpus <- tm_map(corpus, removeWords, "ser")

# Corregimos errores de digitación y consolidamos palabras
# similares que se refieren a lo mismo en un solo término.
# Se restringe la búsqueda y reemplazo a lo que se encuentre 
# dentro de \\b para no reemplazar palabras derivadas de
# manera equivocada o inesperada. 

# for (j in seq(corpus)){
# 	 corpus[[j]] <- gsub("\\pequeños productores\\b", "pequeños_productores", corpus[[j]])
#  }

# corpus <- tm_map(corpus, removeWords, "jóvenes")

# Eliminamos el espacio en blanco
corpus <- tm_map(corpus, stripWhitespace)
# corpus <- tm_map(corpus, PlainTextDocument)

# Creamos una matriz de terminos
dtm <- DocumentTermMatrix(corpus)
tdm <- TermDocumentMatrix(corpus)
#writeLines(as.character(corpus[[31]]))
m <- as.matrix(tdm)
wf <- sort(rowSums(m), decreasing=TRUE)
dm <- data.frame(word = names(wf), freq=wf)

# Graficamos una nube de palabras para inspección visual
set.seed(124)
pdf(file= "9_Oportunidades_nube.pdf", bg="white")
wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"))
dev.off()

# Creamos una tabla de frecuencias
freq <- colSums(as.matrix(dtm))   
length(freq)  
ord <- order(freq)
wf <- data.frame(word=names(freq), freq=freq)
head(wf)

# Graficamos palabras que aparecen al menos 6 veces
# en formato histograma
p <- ggplot(subset(wf, freq>6), aes(word,freq)) + geom_bar(stat="identity") + theme_bw() + theme(axis.text.x=element_text(angle = 45, hjust = 1))  + labs(x = "Palabras") + ylab("Frecuencia")
pdf(file="9_Oportunidades_histograma.pdf", bg="white")
print(p)
dev.off()

# Compactamos la matriz de análisis
# y graficamos los conglomerados
dtms <- removeSparseTerms(dtm, 0.93)
d <- dist(t(dtms), method="euclidian")
kfit <- kmeans(d, 3)
pdf(file="9_Oportunidades_cluster.pdf", bg="white")
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)
dev.off()

# Hacemos conglomerados por el metodo de Ward
# Graficamos un dendrograma para identificar 
# grupos estadísticamente independientes
fit <- hclust(d=d, method="ward.D")
pdf(file="9_Oportunidades_dendrograma.pdf", bg="white")
plot(fit, hang=-1, main=paste("Palabras ", sep=""), ylab="Niveles de Grupos", xlab="Grupos (Método de Ward)")
groups <- cutree(fit, k=7)
rect.hclust(fit, k=7, border="red")
dev.off()

# Extraemos las palabras relevantes que aparecen
# al menos 11 veces
corwords <- as.data.frame(subset(wf, freq>10))
corwords <- as.vector(corwords$word)

# Encontramos palabras con asociación más fuerte para
# cada una de las palabras que más se repiten
# poniendo un umbral mínimo de 75%.
# Esto significa que estas palabras aparecen juntas
# al menos 25% del tiempo.
asoc <- findAssocs(dtm, c(corwords), corlimit=0.60)

# Escribimos las tablas de correlaciones de cada
# una de las palabras relevantes a pestanas de 
# un Excel (corrPalabrasRelev.xlsx).

wb <- createWorkbook()
addWorksheet(wb, "dimensiones")
writeData(wb, "dimensiones", as.data.frame(dim(dtm), row.names=c("filas", "columnas")), rowNames=TRUE)
for (k in 1:length(asoc)) {
  a <- as.data.frame(asoc[k])
  addWorksheet(wb, names(a))
  writeData(wb, names(a), a, rowNames = TRUE)
}
saveWorkbook(wb, "9_Oportunidades_corr.xlsx", overwrite=TRUE)


# 13. Factores
# ------------

# Unimos los tres factores

fairdb$factores <- paste(fairdb$Factor.1, fairdb$Factor.2, fairdb$Factor.3, sep=" ")

# Y nos quedamos solamente con las respuestas
txt <- fairdb$factores

# Limpiamos el texto de puntuacion y palabras comunes como preposiciones y articulos
txt <- gsub("[[:punct:]]", "", txt)
txt <- gsub("[[:digit:]]", "", txt)
txt <- gsub("http\\w+", "", txt)

# Creamos un corpus (cuerpo de texto) para analizar
corpus <- Corpus(VectorSource(txt))
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removeWords, c(stopwords("spanish")))

# Removemos palabras 
corpus <- tm_map(corpus, removeWords, "proyecto")
corpus <- tm_map(corpus, removeWords, "proyectos")
corpus <- tm_map(corpus, removeWords, "tener")
corpus <- tm_map(corpus, removeWords, "hacer")

# Corregimos errores de digitación y consolidamos palabras
# similares que se refieren a lo mismo en un solo término.
# Se restringe la búsqueda y reemplazo a lo que se encuentre 
# dentro de \\b para no reemplazar palabras derivadas de
# manera equivocada o inesperada. 

# for (j in seq(corpus)){
# 	 corpus[[j]] <- gsub("\\pequeños productores\\b", "pequeños_productores", corpus[[j]])
#  }

# corpus <- tm_map(corpus, removeWords, "jóvenes")

# Eliminamos el espacio en blanco
corpus <- tm_map(corpus, stripWhitespace)
# corpus <- tm_map(corpus, PlainTextDocument)

# Creamos una matriz de terminos
dtm <- DocumentTermMatrix(corpus)
tdm <- TermDocumentMatrix(corpus)
#writeLines(as.character(corpus[[31]]))
m <- as.matrix(tdm)
wf <- sort(rowSums(m), decreasing=TRUE)
dm <- data.frame(word = names(wf), freq=wf)

# Graficamos una nube de palabras para inspección visual
set.seed(124)
pdf(file= "13_Factores_nube.pdf", bg="white")
wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"))
dev.off()

# Creamos una tabla de frecuencias
freq <- colSums(as.matrix(dtm))   
length(freq)  
ord <- order(freq)
wf <- data.frame(word=names(freq), freq=freq)
head(wf)

# Graficamos palabras que aparecen al menos 6 veces
# en formato histograma
p <- ggplot(subset(wf, freq>7), aes(word,freq)) + geom_bar(stat="identity") + theme_bw() + theme(axis.text.x=element_text(angle = 45, hjust = 1))  + labs(x = "Palabras") + ylab("Frecuencia")
pdf(file="13_Factores_histograma.pdf", bg="white")
print(p)
dev.off()

# Compactamos la matriz de análisis
# y graficamos los conglomerados
dtms <- removeSparseTerms(dtm, 0.90)
d <- dist(t(dtms), method="euclidian")
kfit <- kmeans(d, 3)
pdf(file="13_Factores_cluster.pdf", bg="white")
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)
dev.off()

# Hacemos conglomerados por el metodo de Ward
# Graficamos un dendrograma para identificar 
# grupos estadísticamente independientes
fit <- hclust(d=d, method="ward.D")
pdf(file="13_Factores_dendrograma.pdf", bg="white")
plot(fit, hang=-1, main=paste("Palabras ", sep=""), ylab="Niveles de Grupos", xlab="Grupos (Método de Ward)")
groups <- cutree(fit, k=7)
rect.hclust(fit, k=7, border="red")
dev.off()

# Extraemos las palabras relevantes que aparecen
# al menos 11 veces
corwords <- as.data.frame(subset(wf, freq>10))
corwords <- as.vector(corwords$word)

# Encontramos palabras con asociación más fuerte para
# cada una de las palabras que más se repiten
# poniendo un umbral mínimo de 75%.
# Esto significa que estas palabras aparecen juntas
# al menos 25% del tiempo.
asoc <- findAssocs(dtm, c(corwords), corlimit=0.60)

# Escribimos las tablas de correlaciones de cada
# una de las palabras relevantes a pestanas de 
# un Excel (corrPalabrasRelev.xlsx).

wb <- createWorkbook()
addWorksheet(wb, "dimensiones")
writeData(wb, "dimensiones", as.data.frame(dim(dtm), row.names=c("filas", "columnas")), rowNames=TRUE)
for (k in 1:length(asoc)) {
  a <- as.data.frame(asoc[k])
  addWorksheet(wb, names(a))
  writeData(wb, names(a), a, rowNames = TRUE)
}
saveWorkbook(wb, "13_Factores_corr.xlsx", overwrite=TRUE)

# 14. Observaciones
# ------------


# Y nos quedamos solamente con las respuestas
txt <- fairdb$Observaciones

# Limpiamos el texto de puntuacion y palabras comunes como preposiciones y articulos
txt <- gsub("[[:punct:]]", "", txt)
txt <- gsub("[[:digit:]]", "", txt)
txt <- gsub("http\\w+", "", txt)

# Creamos un corpus (cuerpo de texto) para analizar
corpus <- Corpus(VectorSource(txt))
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removeWords, c(stopwords("spanish")))

# Removemos palabras 
corpus <- tm_map(corpus, removeWords, "proyecto")
corpus <- tm_map(corpus, removeWords, "proyectos")

# Corregimos errores de digitación y consolidamos palabras
# similares que se refieren a lo mismo en un solo término.
# Se restringe la búsqueda y reemplazo a lo que se encuentre 
# dentro de \\b para no reemplazar palabras derivadas de
# manera equivocada o inesperada. 

# for (j in seq(corpus)){
# 	 corpus[[j]] <- gsub("\\pequeños productores\\b", "pequeños_productores", corpus[[j]])
#  }

# corpus <- tm_map(corpus, removeWords, "jóvenes")

# Eliminamos el espacio en blanco
corpus <- tm_map(corpus, stripWhitespace)
# corpus <- tm_map(corpus, PlainTextDocument)

# Creamos una matriz de terminos
dtm <- DocumentTermMatrix(corpus)
tdm <- TermDocumentMatrix(corpus)
#writeLines(as.character(corpus[[31]]))
m <- as.matrix(tdm)
wf <- sort(rowSums(m), decreasing=TRUE)
dm <- data.frame(word = names(wf), freq=wf)

# Graficamos una nube de palabras para inspección visual
set.seed(124)
pdf(file= "14_Observaciones_nube.pdf", bg="white")
wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"))
dev.off()

# Creamos una tabla de frecuencias
freq <- colSums(as.matrix(dtm))   
length(freq)  
ord <- order(freq)
wf <- data.frame(word=names(freq), freq=freq)
head(wf)

# Graficamos palabras que aparecen al menos 6 veces
# en formato histograma
p <- ggplot(subset(wf, freq>12), aes(word,freq)) + geom_bar(stat="identity") + theme_bw() + theme(axis.text.x=element_text(angle = 45, hjust = 1))  + labs(x = "Palabras") + ylab("Frecuencia")
pdf(file="14_Observaciones_histograma.pdf", bg="white")
print(p)
dev.off()

# Compactamos la matriz de análisis
# y graficamos los conglomerados
dtms <- removeSparseTerms(dtm, 0.90)
d <- dist(t(dtms), method="euclidian")
kfit <- kmeans(d, 3)
pdf(file="14_Observaciones_cluster.pdf", bg="white")
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)
dev.off()

# Hacemos conglomerados por el metodo de Ward
# Graficamos un dendrograma para identificar 
# grupos estadísticamente independientes
fit <- hclust(d=d, method="ward.D")
pdf(file="14_Observaciones_dendrograma.pdf", bg="white")
plot(fit, hang=-1, main=paste("Palabras ", sep=""), ylab="Niveles de Grupos", xlab="Grupos (Método de Ward)")
groups <- cutree(fit, k=7)
rect.hclust(fit, k=7, border="red")
dev.off()

# Extraemos las palabras relevantes que aparecen
# al menos 11 veces
corwords <- as.data.frame(subset(wf, freq>10))
corwords <- as.vector(corwords$word)

# Encontramos palabras con asociación más fuerte para
# cada una de las palabras que más se repiten
# poniendo un umbral mínimo de 75%.
# Esto significa que estas palabras aparecen juntas
# al menos 25% del tiempo.
asoc <- findAssocs(dtm, c(corwords), corlimit=0.60)

# Escribimos las tablas de correlaciones de cada
# una de las palabras relevantes a pestanas de 
# un Excel (corrPalabrasRelev.xlsx).

wb <- createWorkbook()
addWorksheet(wb, "dimensiones")
writeData(wb, "dimensiones", as.data.frame(dim(dtm), row.names=c("filas", "columnas")), rowNames=TRUE)
for (k in 1:length(asoc)) {
  a <- as.data.frame(asoc[k])
  addWorksheet(wb, names(a))
  writeData(wb, names(a), a, rowNames = TRUE)
}
saveWorkbook(wb, "14_Observaciones_corr.xlsx", overwrite=TRUE)



#####################################################
# Global Organization Assessment
# Proyecto FAIR
# Consultor: Renato Vargas
#####################################################

# Cargamos paquetes importantes
library(tm)
library(wordcloud)
library(openxlsx)
rm(list=ls())


# Leemos la base de datos de preguntas y respuestas
fairdb <- as.data.frame(read.csv("fair.csv"))

# Cambiamos el texto
# autodiag$PREGUNTA[autodiag$PREGUNTA=="Barrera de educación formación laboral emprendimiento"] <- "Barreras de formación laboral y emprendimiento"

# Extraemos los segmentos
# segmentos <- unique(fairdb$Segmento.al.que.pertenece)

# for (i in 1:length(segmentos)){
	
# 
# adeduc <- as.data.frame(autodiag[ which(autodiag$Segmento.al.que.pertenece==segmentos[i]), ])

# Y nos quedamos solamente con las respuestas
# txt <- adeduc$Entorno.político
txt <- fairdb$Entorno.económico

# Limpiamos el texto de puntuacion y palabras comunes como preposiciones y articulos
corpus <- gsub("[[:punct:]]", "", txt)
corpus <- gsub("[[:digit:]]", "", corpus)
corpus <- gsub("http\\w+", "", corpus)

# Creamos un corpus (cuerpo de texto) para analizar
corpus <- Corpus(VectorSource(corpus))
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removeWords, c(stopwords("spanish")))
# corpus <- tm_map(corpus, removeWords, "eis")


# Corregimos errores de digitación y consolidamos palabras
# similares que se refieren a lo mismo en un solo término.
# Se restringe la búsqueda y reemplazo a lo que se encuentre 
# dentro de \\b para no reemplazar palabras derivadas de
# manera equivocada o inesperada. 

for (j in seq(corpus)){
	corpus[[j]] <- gsub("\\fata\\b", "falta", corpus[[j]])
}
# corpus <- tm_map(corpus, removeWords, "jóvenes")
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, PlainTextDocument)


# Creamos una matriz de terminos
dtm <-DocumentTermMatrix(corpus)
tdm <- TermDocumentMatrix(corpus)
m <- as.matrix(tdm)
wf <- sort(rowSums(m), decreasing=TRUE)
dm <- data.frame(word = names(wf), freq=wf)

# # Graficamos una nube de palabras para inspección visual
set.seed(124)
pdf(file= paste("Tema_", "_nube.pdf", sep=""), bg="white")
wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"))
dev.off()

# Cargamos dos paquetes que nos serviran de aqui en adelante
library(cluster)
library(fpc)

freq <- colSums(as.matrix(dtm))   
length(freq)  
ord <- order(freq)
wf <- data.frame(word=names(freq), freq=freq)
head(wf)

# Graficamos palabras que aparecen al menos 6 veces
# en formato histograma
library(ggplot2)
p <- ggplot(subset(wf, freq>1), aes(word,freq)) + geom_bar(stat="identity") + theme_bw() + theme(axis.text.x=element_text(angle = 45, hjust = 1)) + ggtitle(preguntas[i]) + labs(x = "Palabras") + ylab("Frecuencia") 
p <- ggplot(subset(wf, freq>1), aes(word,freq)) + geom_bar(stat="identity") + theme_bw() + theme(axis.text.x=element_text(angle = 45, hjust = 1)) + ggtitle("preguntas") + labs(x = "Palabras") + ylab("Frecuencia") 
pdf(file=paste("Tema_", "_histograma.pdf", sep=""), bg="white")
print(p)
dev.off()

# Compactamos la matriz de análisis
# y graficamos los conglomerados
dtms <- removeSparseTerms(dtm, 0.97)
d <- dist(t(dtms), method="euclidian")
kfit <- kmeans(d, 3)
pdf(file=paste("Tema_", i, "_cluster.pdf", sep=""), bg="white")
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)
dev.off()

# Hacemos conglomerados por el metodo de Ward
# Graficamos un dendrograma para identificar 
# grupos estadísticamente independientes
fit <- hclust(d=d, method="ward.D")
fit
pdf(file=paste("Tema_", i, "_dendrograma.pdf", sep=""), bg="white")
plot(fit, hang=-1, main=paste("Palabras ", tolower(preguntas[i]), sep=""), ylab="Niveles de Grupos", xlab="Grupos (Método de Ward)")
groups <- cutree(fit, k=8)
rect.hclust(fit, k=8, border="red")
dev.off()

# Extraemos las palabras relevantes que aparecen
# al menos 6 veces
corwords <- as.data.frame(subset(wf, freq>5))
corwords <- as.vector(corwords$word)

# Encontramos palabras con asociación más fuerte para
# cada una de las palabras que más se repiten
# poniendo un umbral mínimo de 25%.
# Esto significa que estas palabras aparecen juntas
# al menos 25% del tiempo.
asoc <- findAssocs(dtm, c(corwords), corlimit=0.25)

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
saveWorkbook(wb, paste("Tema_", i, "_corrPalabras.xlsx", sep=""), overwrite=TRUE)
}


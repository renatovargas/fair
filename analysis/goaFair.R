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
autodiag <- as.data.frame(read.csv("fair.csv"))

# Cambiamos el texto
autodiag$PREGUNTA[autodiag$PREGUNTA=="Barrera de educación formación laboral emprendimiento"] <- "Barreras de formación laboral y emprendimiento"

# Extraemos los segmentos
segmentos <- unique(autodiag$Segmento.al.que.pertenece)

for (i in 1:length(segmentos)){
	


# Extraemos unicamente las observaciones correspondientes a
# Barreras de Acceso a la Educacion (Pregunta 3)
adeduc <- as.data.frame(autodiag[ which(autodiag$Segmento.al.que.pertenece==segmentos[i]), ])

# Y nos quedamos solamente con las respuestas
txt <- adeduc$Entorno.político
txt <- autodiag$Entorno.político

# Limpiamos el texto de puntuacion y palabras comunes como preposiciones y articulos
corpus <- gsub("[[:punct:]]", "", txt)
corpus <- gsub("[[:digit:]]", "", corpus)
corpus <- gsub("http\\w+", "", corpus)

# Creamos un corpus (cuerpo de texto) para analizar
corpus <- Corpus(VectorSource(corpus))
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removeWords, c(stopwords("spanish")))
corpus <- tm_map(corpus, removeWords, "falta")
corpus <- tm_map(corpus, removeWords, "factor")
corpus <- tm_map(corpus, removeWords, "así")
corpus <- tm_map(corpus, removeWords, "casi")
corpus <- tm_map(corpus, removeWords, "ven")
corpus <- tm_map(corpus, removeWords, "tema")
corpus <- tm_map(corpus, removeWords, "temas")
corpus <- tm_map(corpus, removeWords, "dan")
corpus <- tm_map(corpus, removeWords, "eis")


# Corregimos errores de digitación y consolidamos palabras
# similares que se refieren a lo mismo en un solo término.
# Se restringe la búsqueda y reemplazo a lo que se encuentre 
# dentro de \\b para no reemplazar palabras derivadas de
# manera equivocada o inesperada. 

for (j in seq(corpus)){
	corpus[[j]] <- gsub("\\bfamiliar\\b", "familia", corpus[[j]])
	corpus[[j]] <- gsub("\\btabbues\\b", "tabú", corpus[[j]])
	corpus[[j]] <- gsub("\\btabu\\b", "tabú", corpus[[j]])
	corpus[[j]] <- gsub("\\bautoridad\\b", "autoridad", corpus[[j]])
	corpus[[j]] <- gsub("\\baautoridad\\b", "autoridad", corpus[[j]])
	corpus[[j]] <- gsub("\\bcreacion\\b", "creación", corpus[[j]])
	corpus[[j]] <- gsub("\\bdesconeccion\\b,", "desconexión", corpus[[j]])
	corpus[[j]] <- gsub("\\bacademico\\b", "académica", corpus[[j]])	
	corpus[[j]] <- gsub("\\bformacion\\b", "formación", corpus[[j]])		
	corpus[[j]] <- gsub("\\bformacion\\b", "formación", corpus[[j]])
	corpus[[j]] <- gsub("\\bformal\\b", "formal", corpus[[j]])
	corpus[[j]] <- gsub("\\bgeografica\\b", "geográfica", corpus[[j]])
	corpus[[j]] <- gsub("\\bgratuidad\\b", "gratuita", corpus[[j]])
	corpus[[j]] <- gsub("\\bhijos\\b", "hijos", corpus[[j]])
	corpus[[j]] <- gsub("\\bhogar\\b", "hogar", corpus[[j]])
	corpus[[j]] <- gsub("\\bacademico\\b", "académico", corpus[[j]])
	corpus[[j]] <- gsub("\\bacceden\\b", "acceso", corpus[[j]])
	corpus[[j]] <- gsub("\\badquiriendo\\b", "adquirir", corpus[[j]])
	corpus[[j]] <- gsub("\\baltos\\b", "alto", corpus[[j]])	
	corpus[[j]] <- gsub("\\bapoyen\\b", "apoyo", corpus[[j]])
	corpus[[j]] <- gsub("\\barea\\b", "área", corpus[[j]])
	corpus[[j]] <- gsub("\\bárea rural\\b", "área_rural", corpus[[j]])	
	corpus[[j]] <- gsub("\\batencion\\b", "atención", corpus[[j]])
	corpus[[j]] <- gsub("\\bbarreras\\b", "barrera", corpus[[j]])
	corpus[[j]] <- gsub("\\bbásica\\b", "básico", corpus[[j]])
	corpus[[j]] <- gsub("\\bbasico\\b", "básico", corpus[[j]])
	corpus[[j]] <- gsub("\\bbulling\\b", "bullying", corpus[[j]])
	corpus[[j]] <- gsub("\\bbrechas\\b", "brecha", corpus[[j]])
	corpus[[j]] <- gsub("\\bconsientes\\b", "conscientes", corpus[[j]])
	corpus[[j]] <- gsub("\\bconstruccion\\b", "construcción", corpus[[j]])
	corpus[[j]] <- gsub("\\bcostos\\b", "costo", corpus[[j]])
	corpus[[j]] <- gsub("\\bcreación\\b", "crear", corpus[[j]])
	corpus[[j]] <- gsub("\\bfalta de interes\\b", "desinterés", corpus[[j]])
	corpus[[j]] <- gsub("\\bfalta de interés\\b", "desinterés", corpus[[j]])
	corpus[[j]] <- gsub("\\bfalta interes\\b", "desinterés", corpus[[j]])
	corpus[[j]] <- gsub("\\bfalta interés\\b", "desinterés", corpus[[j]])
	corpus[[j]] <- gsub("\\bdesinteres\\b", "desinterés", corpus[[j]])
	corpus[[j]] <- gsub("\\bdesmotivacion\\b", "desinterés", corpus[[j]])
	corpus[[j]] <- gsub("\\bdistancias\\b", "distancia", corpus[[j]])	
	corpus[[j]] <- gsub("\\bcapacitacion\\b", "capacitación", corpus[[j]])
	corpus[[j]] <- gsub("\\báreas\\b", "área", corpus[[j]])
	corpus[[j]] <- gsub("\\bcentralizado\\b", "central", corpus[[j]])
  corpus[[j]] <- gsub("\\bpierden\\b", "pérdida", corpus[[j]])	
	corpus[[j]] <- gsub("\\bcomprometidos\\b", "compromiso", corpus[[j]])
	corpus[[j]] <- gsub("\\bcreacion\\b", "creación", corpus[[j]])
	corpus[[j]] <- gsub("\\bcrean\\b", "creación", corpus[[j]])
	corpus[[j]] <- gsub("\\bcultura\\b", "cultural", corpus[[j]])
	corpus[[j]] <- gsub("\\bculturales\\b", "cultural", corpus[[j]])
	corpus[[j]] <- gsub("\\bcultura\\b", "cultural", corpus[[j]])
	corpus[[j]] <- gsub("\\bdesconeccion\\b", "desconexión", corpus[[j]])
	corpus[[j]] <- gsub("\\beconomica\\b", "económico", corpus[[j]])
	corpus[[j]] <- gsub("\\beconómica\\b", "económico", corpus[[j]])
	corpus[[j]] <- gsub("\\beconomia\\b", "económico", corpus[[j]])
	corpus[[j]] <- gsub("\\beconomía\\b", "económico", corpus[[j]])
	corpus[[j]] <- gsub("\\beconomicas\\b", "económico", corpus[[j]])
	corpus[[j]] <- gsub("\\beconomico\\b", "económico", corpus[[j]])
	corpus[[j]] <- gsub("\\beconomicos\\b", "económico", corpus[[j]])
	corpus[[j]] <- gsub("\\beconómicos\\b", "económico", corpus[[j]])
	corpus[[j]] <- gsub("\\bsituación economica\\b", "económico", corpus[[j]])
	corpus[[j]] <- gsub("\\educadora\\b", "docentes", corpus[[j]])	
	corpus[[j]] <- gsub("\\beducacion\\b", "educación", corpus[[j]])
	corpus[[j]] <- gsub("\\beduación\\b", "educación", corpus[[j]])
	corpus[[j]] <- gsub("\\beducaicón\\b", "educación", corpus[[j]])
 	corpus[[j]] <- gsub("\\beducativa\\b", "educación", corpus[[j]])
 	corpus[[j]] <- gsub("\\beducativas\\b", "educación", corpus[[j]])
 	corpus[[j]] <- gsub("\\beducativos\\b", "educación", corpus[[j]])
	corpus[[j]] <- gsub("\\beducativo\\b", "educación", corpus[[j]])
	corpus[[j]] <- gsub("\\bcentros educación\\b", "centros_educativos", corpus[[j]])
	corpus[[j]] <- gsub("\\bcentros_educativos\\b", "centros_educativos", corpus[[j]])
	corpus[[j]] <- gsub("\\bprogramas educación\\b", "programas_educativos", corpus[[j]])
	corpus[[j]] <- gsub("\\bestablecimientos educativos\\b", "centros_educativos", corpus[[j]])
	corpus[[j]] <- gsub("\\bcentros_educativos\\b", "establecimientos", corpus[[j]])	
	corpus[[j]] <- gsub("\\bescuela\\b", "establecimientos", corpus[[j]])
	corpus[[j]] <- gsub("\\bempleadores\\b", "empleo", corpus[[j]])
	corpus[[j]] <- gsub("\\bempleode\\b", "empleo", corpus[[j]])
	corpus[[j]] <- gsub("\\bempleobarrera\\b", "empleo", corpus[[j]])
	corpus[[j]] <- gsub("\\bemprendedores\\b", "emprendimiento", corpus[[j]])
	corpus[[j]] <- gsub("\\bemprendedor\\b", "emprendimiento", corpus[[j]])
	corpus[[j]] <- gsub("\\bestudiante\\b", "estudiantes", corpus[[j]])
	corpus[[j]] <- gsub("\\bestudio\\b", "estudiar", corpus[[j]])
	corpus[[j]] <- gsub("\\bevalucaiones\\b", "evaluación", corpus[[j]])
	corpus[[j]] <- gsub("\\bexclusion\\b", "exclusión", corpus[[j]])
	corpus[[j]] <- gsub("\\bexcluyen\\b", "exclusión", corpus[[j]])
	corpus[[j]] <- gsub("\\bexiste\\b", "existencia", corpus[[j]])
	corpus[[j]] <- gsub("\\bpobreza extrema\\b", "pobreza_extrema", corpus[[j]])	
	corpus[[j]] <- gsub("\\bfamiliares\\b", "familia", corpus[[j]])
	corpus[[j]] <- gsub("\\bfamilias\\b", "familia", corpus[[j]])
	corpus[[j]] <- gsub("\\bfin\\b", "final", corpus[[j]])
	corpus[[j]] <- gsub("\\bflexible\\b", "flexibilidad", corpus[[j]])
	corpus[[j]] <- gsub("\\bflexibles\\b", "flexibilidad", corpus[[j]])
	corpus[[j]] <- gsub("\\bfformaqción\\b", "formación", corpus[[j]])
	corpus[[j]] <- gsub("\\bformativo\\b", "formación", corpus[[j]])
	corpus[[j]] <- gsub("\\bformmación\\b", "formación", corpus[[j]])
	corpus[[j]] <- gsub("\\bfuerza laboral\\b", "trabajo", corpus[[j]])
	corpus[[j]] <- gsub("\\blaboral\\b", "trabajo", corpus[[j]])
	corpus[[j]] <- gsub("\\bidiomatico\\b", "idiomático", corpus[[j]])
	corpus[[j]] <- gsub("\\binformacion\\b", "información", corpus[[j]])
	corpus[[j]] <- gsub("\\binstitucionales\\b", "institucional", corpus[[j]])	
	corpus[[j]] <- gsub("\\binstituciones\\b", "institucional", corpus[[j]])
	corpus[[j]] <- gsub("\\binteres\\b", "interés", corpus[[j]])
	corpus[[j]] <- gsub("\\bintrafamiliar\\b", "familia", corpus[[j]])
	corpus[[j]] <- gsub("\\bjovenes\\b", "jóvenes", corpus[[j]])
	corpus[[j]] <- gsub("\\bjoven\\b", "jóvenes", corpus[[j]])
	corpus[[j]] <- gsub("\\bjover\\b", "jóvenes", corpus[[j]])
	corpus[[j]] <- gsub("\\bmala\\b", "mal", corpus[[j]])	
	corpus[[j]] <- gsub("\\bmalas\\b", "mal", corpus[[j]])
	corpus[[j]] <- gsub("\\bmigracion\\b", "migración", corpus[[j]])	
	corpus[[j]] <- gsub("\\bmodelos\\b", "modelo", corpus[[j]])
	corpus[[j]] <- gsub("\\bmotivar\\b", "motivación", corpus[[j]])	
	corpus[[j]] <- gsub("\\bnecesario\\b", "necesidad", corpus[[j]])
	corpus[[j]] <- gsub("\\bnecesidades\\b", "necesidad", corpus[[j]])
	corpus[[j]] <- gsub("\\bniveles\\b", "nivel", corpus[[j]])
	corpus[[j]] <- gsub("\\bnumerosas\\b", "numeroso", corpus[[j]])
	corpus[[j]] <- gsub("\\bnumerosa\\b", "numeroso", corpus[[j]])
	corpus[[j]] <- gsub("\\bofertas\\b", "oferta", corpus[[j]])
	corpus[[j]] <- gsub("\\boficiales\\b", "oficial", corpus[[j]])
	corpus[[j]] <- gsub("\\bofrece\\b", "oferta", corpus[[j]])
	corpus[[j]] <- gsub("\\boportunidades\\b", "oportunidad", corpus[[j]])
	corpus[[j]] <- gsub("\\borganización\\b", "institucional", corpus[[j]])
	corpus[[j]] <- gsub("\\borganizaciones\\b", "institucional", corpus[[j]])
	corpus[[j]] <- gsub("\\bpedagogica\\b", "pedagógica", corpus[[j]])
	corpus[[j]] <- gsub("\\bpertinentes\\b", "pertinencia", corpus[[j]])
	corpus[[j]] <- gsub("\\bpocas\\b", "poca", corpus[[j]])
	corpus[[j]] <- gsub("\\bpolitica\\b", "política", corpus[[j]])
	corpus[[j]] <- gsub("\\bproblemas\\b", "problema", corpus[[j]])
	corpus[[j]] <- gsub("\\bprogramas\\b", "programa", corpus[[j]])
	corpus[[j]] <- gsub("\\bpromuevan\\b", "promover", corpus[[j]])
	corpus[[j]] <- gsub("\\bpublica\\b", "pública", corpus[[j]])
	corpus[[j]] <- gsub("\\brecurso\\b", "recursos", corpus[[j]])
	corpus[[j]] <- gsub("\\bresponsable\\b", "responsibilidad", corpus[[j]])
	corpus[[j]] <- gsub("\\bsexual\\b", "sexualidad", corpus[[j]])
	corpus[[j]] <- gsub("\\btecnica\\b", "técnica", corpus[[j]])
	corpus[[j]] <- gsub("\\btécnicas\\b", "técnica", corpus[[j]])
	corpus[[j]] <- gsub("\\btiempor\\b", "tiempo", corpus[[j]])
	corpus[[j]] <- gsub("\\btransito\\b", "tránsito", corpus[[j]])
	corpus[[j]] <- gsub("\\btraves\\b", "través", corpus[[j]])
	corpus[[j]] <- gsub("\\bvocación\\b", "vocacional", corpus[[j]])
	corpus[[j]] <- gsub("\\bdesintegracion\\b", "desintegración", corpus[[j]])
	corpus[[j]] <- gsub("\\bdiversificado\\b)", "diversificado", corpus[[j]])	
	corpus[[j]] <- gsub("\\bmetodologia\\b", "metodología", corpus[[j]])
	corpus[[j]] <- gsub("\\boportunidades\\b:", "oportunidades", corpus[[j]])
	corpus[[j]] <- gsub("\\bsituacion\\b", "situación", corpus[[j]])
	corpus[[j]] <- gsub("\\bempleo\\b", "trabajo", corpus[[j]])
	corpus[[j]] <- gsub("\\dinamica\\b", "dinámica", corpus[[j]])
	corpus[[j]] <- gsub("\\psicodinamica\\b", "psicodinámica", corpus[[j]])
	corpus[[j]] <- gsub("\\metodologicas\\b", "metodológicas", corpus[[j]])
	corpus[[j]] <- gsub("\\creditos\\b", "crédito", corpus[[j]])
	corpus[[j]] <- gsub("\\orientacion\\b", "orientación", corpus[[j]])
	corpus[[j]] <- gsub("\\fata\\b", "falta", corpus[[j]])
}
corpus <- tm_map(corpus, removeWords, "educación")
corpus <- tm_map(corpus, removeWords, "jóvenes")
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
p <- ggplot(subset(wf, freq>5), aes(word,freq)) + geom_bar(stat="identity") + theme_bw() + theme(axis.text.x=element_text(angle = 45, hjust = 1)) + ggtitle(preguntas[i]) + labs(x = "Palabras") + ylab("Frecuencia") 
p <- ggplot(subset(wf, freq>5), aes(word,freq)) + geom_bar(stat="identity") + theme_bw() + theme(axis.text.x=element_text(angle = 45, hjust = 1)) + ggtitle("preguntas") + labs(x = "Palabras") + ylab("Frecuencia") 
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


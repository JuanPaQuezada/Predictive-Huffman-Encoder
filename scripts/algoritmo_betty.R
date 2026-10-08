# Leer el texto del archivo y guardarlo en un vector de caracteres
# archivo <- readLines("file", encode = "UTF-8")
archivo <- c("La tarde
El sol se va lento y la tarde se torna serena.
El mar no termina y la arena se siente tranquila.
Ana mira el mar y sonríe.

La tarde termina y la noche se acerca.
El mar se serena y la arena se torna fría.
Ana mira la noche y sonríe.

La noche termina y el sol sale.
Ana se torna en arena.
El mar la mira y sonríe.")

texto_completo <- paste(archivo, collapse = " ")
texto <- strsplit(texto_completo, split = "")[[1]]

# Vectores de letras actuales y siguientes para comparar probabilidades en pares
letras_actuales <- texto[1:length(texto) - 1]
letras_siguientes <- texto[2:length(texto)]

# Generar tabla de frecuencias absolutas de los pares
matriz_frecuencias <- table(letras_actuales, letras_siguientes)
print(matriz_frecuencias)

# Generar tabla de probabilidades a partir de las frecuencias acumuladas (frec. relativas)
matriz_probabilidades <- prop.table(matriz_frecuencias, margin = 1)
print(matriz_probabilidades)

# Calcular las probabilidades más altas y guardar sus posiciones
probabilidad_mayor <- apply(matriz_probabilidades, MARGIN = 1, FUN = which.max)
prediccion <- colnames(matriz_probabilidades)[probabilidad_mayor]
# vector/diccionario para acceder a cada letra y a su predicción correspondiente
names(prediccion) <- rownames(matriz_probabilidades)
print(prediccion)
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
texto_caracteres <- strsplit(texto_completo, split = "")[[1]]

# Vectores de letras actuales y siguientes para comparar probabilidades en pares
letras_actuales <- texto_caracteres[1:length(texto_caracteres) - 1]
letras_siguientes <- texto_caracteres[2:length(texto_caracteres)]

# Generar tabla de frecuencias absolutas de los pares
matriz_frecuencias <- table(letras_actuales, letras_siguientes)

# Generar tabla de probabilidades a partir de las frecuencias acumuladas (frec. relativas)
matriz_probabilidades <- prop.table(matriz_frecuencias, margin = 1)

# Calcular las probabilidades más altas y guardar sus posiciones
probabilidad_mayor <- apply(matriz_probabilidades, MARGIN = 1, FUN = which.max)
prediccion <- colnames(matriz_probabilidades)[probabilidad_mayor]
# Vector/diccionario para acceder a cada letra y a su predicción correspondiente
names(prediccion) <- rownames(matriz_probabilidades)

# Vector vacío con misma longitud que el texto original
n <- length(texto_caracteres)
texto_codificado <- character(n)

texto_codificado[1] <- texto_caracteres[1]

# Evaluar predicción y aplicar sustitución Betty
for(i in 2:n){
    letra_anterior <- texto_caracteres[i - 1]
    letra_actual <- texto_caracteres[i]

    letra_esperada <- prediccion[letra_anterior]

    if(!is.na(letra_esperada) && letra_esperada == letra_actual){
        texto_codificado[i] <- "_"
    } else{
        texto_codificado[i] <- letra_actual
    }
}

# Sustitución Betty aplicada guardada en vector texto
texto <- paste(texto_codificado, collapse = "")
print(texto)
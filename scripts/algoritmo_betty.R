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

procesar_archivo <- function(archivo){
    texto_completo <- paste(archivo, collapse = "\n")
    texto_caracteres <- strsplit(texto_completo, split = "")[[1]]

    return(texto_caracteres)
}

generador_predicciones <- function(texto_caracteres){
    # Vectores de letras actuales y siguientes para comparar probabilidades en pares
    letras_actuales <- texto_caracteres[1:length(texto_caracteres) - 1]
    letras_siguientes <- texto_caracteres[2:length(texto_caracteres)]

    # Generar tabla de frecuencias absolutas de los pares
    matriz_frecuencias <- table(letras_actuales, letras_siguientes)

    # Generar tabla de probabilidades a partir de las frecuencias acumuladas (frec. relativas)
    matriz_probabilidades <- prop.table(matriz_frecuencias, margin = 1)

    # Calcular las 2 probabilidades más altas y guardar sus posiciones
    probabilidades <- apply(matriz_probabilidades, MARGIN = 1, FUN = function(filas){ order(filas, decreasing = TRUE)[1:2] })
    prediccion_top1 <- colnames(matriz_probabilidades)[probabilidades[1, ]]
    prediccion_top2 <- colnames(matriz_probabilidades)[probabilidades[2, ]]

    # Diccionario para acceder a cada letra y a su predicción correspondiente
    predicciones <- data.frame(
        op1 = prediccion_top1,
        op2 = prediccion_top2,
        row.names = rownames(matriz_probabilidades)
    )

    return(predicciones)
}

susticion_Betty <- function(texto_caracteres, predicciones){
    # Vector vacío con misma longitud que el texto original
    n <- length(texto_caracteres)
    texto_codificado <- character(n)

    texto_codificado[1] <- texto_caracteres[1]

    # Evaluar predicción y aplicar sustitución Betty2
    for(i in 2:n){
        letra_anterior <- texto_caracteres[i - 1]
        letra_actual <- texto_caracteres[i]

        letra_esperada_top1 <- predicciones[letra_anterior, "op1"]
        letra_esperada_top2 <- predicciones[letra_anterior, "op2"]

        # Evaluación de predicciones y asignación de símbolos especiales en caso de que sea correcta
        if(!is.na(letra_esperada_top1) && letra_esperada_top1 == letra_actual){
            texto_codificado[i] <- "_"
        } else if(!is.na(letra_esperada_top2) && letra_esperada_top2 == letra_actual){
            texto_codificado[i] <- "="
        } else{
            texto_codificado[i] <- letra_actual
        }
    }

    # Sustitución Betty2 aplicada guardada en vector texto
    texto <- paste(texto_codificado, collapse = "")
    
    return(texto)
}

revertir_sustitucion_Betty<-function(texto_betty, tabla_predicciones){
    caracteres<-strsplit(texto_betty,split="")[[1]]
    if(length(caracteres)<=1) return(caracteres)
    texto_recuperado<-character(length(caracteres))
    texto_recuperado[1]<-caracteres[1]
    caracter_anterior<-caracteres[1]
    for(i in 2:length(caracteres)){
        caracter_actual<-caracteres[i]
        if(caracter_actual=="_"){
            letra_predicha<-tabla_predicciones[caracter_anterior,"op1"]
        }
        else if(caracter_actual=="="){
            letra_predicha<-tabla_predicciones[caracter_anterior,"op2"]
        }
        else{
            letra_predicha<-caracter_actual
        }
        texto_recuperado[i]<-letra_predicha
        caracter_anterior<-letra_predicha
    }
    texto_final<-paste(texto_recuperado,collapse="")
    return(texto_final)
}

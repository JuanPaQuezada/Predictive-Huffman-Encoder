source("./scripts/algoritmo_betty.R")
source("./scripts/algoritmo_huffman.R")
source("./scripts/operaciones_binarias.R")

archivo_comprimido<-"archivo_comprimido.bin"
archivo_salida<-"archivo_descomprimido.txt"
cat("cargando metadatos...\n")
metadatos<-readRDS("metadatos.rds")
cat("Contenido del diccionario guardado:\n")
tabla_predicciones<-metadatos$predicciones
diccionario_huffman<-metadatos$huffman
bits_validos<-metadatos$bits_validos_ultimo_byte

cat("Leyendo archivo binario comprimido...\n")
secuencia_bits<-leer_archivo_comprimido(archivo_comprimido, bits_validos)
cat("Decodificando con Huffman...\n")
texto_betty<-decodificar_texto(secuencia_bits,diccionario_huffman,bits_validos)
cat("Revirtiendo sustitucion de Betty...\n")
texto_char<-revertir_sustitucion_Betty(texto_betty,tabla_predicciones)
cat("Guardando archivo descomprimido...\n")
cat("--- DIAGNÓSTICO ---\n")
cat("Llaves reales en metadatos:", paste(names(metadatos), collapse=", "), "\n")
cat("Cantidad de bits leídos:", length(secuencia_bits), "\n")
cat("Letras tras decodificar Huffman:", nchar(texto_betty), "\n")
cat("Letras tras revertir Betty:", nchar(texto_char), "\n")
cat("-------------------\n")
writeLines(texto_char, archivo_salida, useBytes=TRUE)
cat("Descompresion completada. Archivo descomprimido guardado como ", archivo_salida, "\n")


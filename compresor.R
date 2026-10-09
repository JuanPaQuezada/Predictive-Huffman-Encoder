
#cargar los modulos necesarios para la compresion 
source("./scripts/algoritmo_betty.R")
source("./scripts/algoritmo_huffman.R")
source("./scripts/operaciones_binarias.R")
#definir archivos de entrada y salida
archivo_entrada <- "texto_prueba.txt"
nombre_archivo_comprimido<-"archivo_comprimido.bin"

#leer el archivo de texto original
lineas_archivo<-readLines(archivo_entrada, encoding = "UTF-8")

#fase de sustitucion de sustitucion de Betty 
texto_char<-procesar_archivo(lineas_archivo)
tabla_predicciones<-generador_predicciones(texto_char)
texto_betty<-susticion_Betty(texto_char,tabla_predicciones)

#fase de huffman 
df_frecuencias<-obtener_frecuencias(texto_betty)
diccionario_huffman<-arbol_huffman(df_frecuencias)
secuencia_bits<-codificar_texto(texto_betty,diccionario_huffman)

#operaciones binarias I/O 
bits_validos<-guardar_archivo_comprimido(secuencia_bits,nombre_archivo_comprimido)

#empaquetar metadatos 
# guardar metadatos en un archivo 
metadatos<-list(
                predicciones=tabla_predicciones,
                huffman=diccionario_huffman,
                bits_validos_ultimo_byte=bits_validos
                )
saveRDS(metadatos,"metadatos.rds")
cat("Compresion completada. Archivo comprimido guardado como ", nombre_archivo_comprimido, " y metadatos guardados en metadatos.rds\n")


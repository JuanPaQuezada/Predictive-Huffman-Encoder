
guardar_archivo_comprimido <- function(secuencia_bits, nombre_archivo) {
    residuo<-nchar(secuencia_bits)%%8
    bits_validos_ultimo_byte<-ifelse(residuo==0,8,residuo)
    ceros_faltantes<-(8-residuo)%%8
    secuencia_bits<-paste0(secuencia_bits,paste(rep("0",ceros_faltantes),collapse = ""))
    #dvidir en bloques de 8 y convertir en valores decimales
    bloques <- strsplit(secuencia_bits, "(?<=\\G.{8})", perl = TRUE)[[1]]
    valores_decimales <- sapply(bloques, function(bloque) {
        sum(as.numeric(strsplit(bloque, "")[[1]]) * 2^(7:0))
    })
  
  bytes <- as.raw(valores_decimales)
  writeBin(bytes, nombre_archivo)
  return(bits_validos_ultimo_byte)
}

leer_archivo_comprimido <- function(nombre_archivo, bits_validos_ultimo_byte) {
    tamano <- file.info(nombre_archivo)$size
    bytes <- readBin(nombre_archivo, what = "raw", n = tamano)
    #convertir  bytes de regreso a valores numericos
    valores_decimales<-as.numeric(bytes)
    #convertir valores decimales a  una cadena de 8 bits
    bloques<-sapply(valores_decimales,function(x){
        #intToBits devuelve un vector de 32 bits 
        #tomamos los primeros 8 bits y los invertimos [8:1] para leer de izquierda a derecha (MSB)
        paste(as.integer(intToBits(x))[8:1],collapse = "")
    })
    cadena_bits<-paste(bloques,collapse = "")
    #eliminando el relleno calculando la longitud exacta de la cadena de bits 
    total_bits_sin_relleno<-(length(bloques)-1)*8+bits_validos_ultimo_byte
    cadena_bits_limpia<-substr(cadena_bits,1,total_bits_sin_relleno)
    return(cadena_bits_limpia) 
}


#funcion para obtener las frecuencias de cada simbolo en el texto
obtener_frecuencias<-function(texto){
    #convertir el texto en un vector de caracteres individuales
    caracteres<-strsplit(texto,split="")[[1]]
    #contar las ocurrencias de cada simbolo
    frecuencias<-table(caracteres)
    #copnvertir a un dataframe y renombrar columnas
    df_frecuencias<-as.data.frame(frecuencias)
    colnames(df_frecuencias)<-c("simbolo","frecuencia")
    df_frecuencias<-df_frecuencias[order(df_frecuencias$frecuencia),]
    return(df_frecuencias)
}

#funcion para construir el arbol de huffman
arbol_huffman<-function(df_frecuencias){
    #crear una lista de nodos
    nodos<-list()
    for(i in 1:nrow(df_frecuencias)){
        nodos[[i]]<-list(simbolo=df_frecuencias$simbolo[i],frecuencia=df_frecuencias$frecuencia[i],izquierda=NULL,derecha=NULL)
    }
    #construir el arbol
    while(length(nodos)>1){
        #ordenar los nodos por frecuencia
        nodos<-nodos[order(sapply(nodos,function(x)x$frecuencia))]
        #encontrar nodo raiz (fusionar los nodos de menos frecuencia y sumassu valores y asignar hijos izquierdos y derecho y dejar listo la raiz
        nodo_raiz<-list(simbolo=NULL,frecuencia=nodos[[1]]$frecuencia+nodos[[2]]$frecuencia,izquierda=nodos[[1]],derecha=nodos[[2]])
        #eliminar los nodos fusionados de la lista
        nodos<-nodos[-c(1,2)]
        #agregar el nodo raiz a la lista de nodos
        nodos<-c(nodos,list(nodo_raiz))
    }
    raiz<-nodos[[1]]
    diccionario<-list()
    #funcion recursiva para recorrer el arbol_huffman y asignar los codigos binarios a cada simbolo
    asignar_codigos<-function(nodo, codigo_acumulado){
        if(is.null(nodo$simbolo)){
            asignar_codigos(nodo$izquierda, paste0(codigo_acumulado, "0"))
            asignar_codigos(nodo$derecha, paste0(codigo_acumulado, "1"))
        }else{
            diccionario[[nodo$simbolo]]<<-codigo_acumulado
        }
    }

    asignar_codigos(raiz,"")
    return(diccionario)
    
}

#sustituir cada letra por su codigo binario obtenido del arbol. uniendo todo en una cadena de ceros y unos conectados a mi funcion guardar_archivo_comprimido 
codificar_texto<-function(texto,diccionario){
    caracteres<-strsplit(texto,split="")[[1]]
    x<-sapply(caracteres, function(x) diccionario[[x]])
    secuencia_bits<-paste(x,collapse = "")
    return(secuencia_bits)
}


#funcion para obtener las frecuencias de cada simbolo en el texto
obtener_frecuencias<-function(texto){
    #convertir el texto en un vector de caracteres individuales
    caracteres<-strsplit(texto,split="")[[1]]
    #contar las ocurrencias de cada simbolo
    frecuencias<-table(caractereso)
    #copnvertir a un dataframe y renombrar columnas
    df_frecuencias<-as.data.frame(frecuencias)
    colnames(df_frecuencias)<-c("simbolo","frecuencia")
    df_frecuencias<-df_frecuencias[order(df_frecuencias$frecuencia),]
    return(df_frecuencias)
}




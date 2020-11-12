#Este Script se pretende utilizar para realizar un agrupamiento
#no supervisado de aplicaciones similares atendiendo a su distribución
#de la localidad temporal. Se considera un umbral Tu a partir del cual
#el acceso a una posición de memoria es considerado como primer acceso.
#En consecuencia, se establece un máximo en la distribución de la distancia.

#Librerías
library(data.table)


#Funciones

#Lee el fichero loctmp, que contiene información sobre la localidad temporal.
#Devuelve las observaciones sobre la localidad temporal en crudo.
distribucionTemporal <- function(file){
  datos <- fread(file)
  y <- datos$distTemp
  rm(datos)
  return(y)
}


#Lee todos los datos de la carpeta loctmp, con extensión .loctmp
#Lo almacena en una lista en ese mismo orden y lo devuelve
leerTodo <- function(nombres){
  devolver <- list()
  i <- 1
  n <- length(nombres)
  for(nombre in nombres){
    nombreCompleto <- paste("loctmp/",nombre,".loctmp",sep="")
    cat("Leyendo fichero ",nombre," (",i," de ",n,")\n",sep="")
    devolver[[i]] <- distribucionTemporal(nombreCompleto)
    i <- i+1
  }
  names(devolver) <- nombres
  return(devolver)
}

#Esta función preprocesa todas las distribuciones que se le envían
#como argumento. Elimina todos los valores de la distancia temporal que
#se encuentran fuera del máximo umbral establecido para el olvido: Tu
eliminarValoresAltos <- function(distribuciones,Tu = 100000){
  for(i in 1:length(distribuciones)){
    y <- distribuciones[[i]]
    distribuciones[[i]] <- y[y <= Tu]
  }
  return(distribuciones)
}

#Elabora un resumen de la distribución.
#Para ello, se elabora un histograma con intervalos equidistantes
#cuyo número viene dado por el argumento breaks.
#El parámetro Tu indica el máximo posible de la distribución de la
#distancia temporal. Todos los histogramas generados tienen la misma escala.
#Elimina todos los valores de la distancia temporal que
#se encuentran fuera del máximo umbral establecido para el olvido: Tu
resumenDistribucionTemporal <- function(y,breaks=100,Tu=100000){
  cutpoints <- seq(0,Tu+1,length=breaks+1)
  y <- y[y <= Tu]
  h <- as.numeric(table(cut(y,breaks=cutpoints)))
  h <- h/sum(h)
  return(h)
}



#Elabora un resumen para todas las distribuciones con el numero de bins
#indicado. Son separadas en funcion de su asociatividad y tamaño de la LLC
#Si remove > 0, se eliminan en el resumen aquellos programas que tengan menos datos
#que los especificados. Si cero, entonces no se elimina nada
#El parámetro Tu es el parámetro para el olvido definido anteriormente
resumenTodoDistribucionTemporal <- function(distribuciones,breaks=100,remove=5*breaks,Tu=100000){
  Ass1Tam16 <- matrix(nrow=0,ncol=breaks)
  Ass1Tam32 <- matrix(nrow=0,ncol=breaks)
  Ass8Tam16 <- matrix(nrow=0,ncol=breaks)
  Ass8Tam32 <- matrix(nrow=0,ncol=breaks)
  nombres <- names(distribuciones)
  for(i in 1:length(distribuciones)){
    nombre <- nombres[i]
    if((length(distribuciones[[i]]) < remove) & (remove > 0)){
      cat("Se ha eliminado ",nombre," (",i," de ",length(distribuciones),")\n",sep="")
    } else {
      cat("Generando resumen de ",nombre," (",i," de ",length(distribuciones),")\n",sep="")
      if(grepl("Ass1Tam16",nombre)){
        nombreFila <- strsplit(nombre,"_Ass1Tam16")[[1]]
        Ass1Tam16 <- rbind(Ass1Tam16,resumenDistribucionTemporal(distribuciones[[i]],breaks,Tu))
        rownames(Ass1Tam16)[nrow(Ass1Tam16)] <- nombreFila
      }
      if(grepl("Ass1Tam32",nombre)){
        nombreFila <- strsplit(nombre,"_Ass1Tam32")[[1]]
        Ass1Tam32 <- rbind(Ass1Tam32,resumenDistribucionTemporal(distribuciones[[i]],breaks,Tu))
        rownames(Ass1Tam32)[nrow(Ass1Tam32)] <- nombreFila
      }
      if(grepl("Ass8Tam16",nombre)){
        nombreFila <- strsplit(nombre,"_Ass8Tam16")[[1]]
        Ass8Tam16 <- rbind(Ass8Tam16,resumenDistribucionTemporal(distribuciones[[i]],breaks,Tu))
        rownames(Ass8Tam16)[nrow(Ass8Tam16)] <- nombreFila
      }
      if(grepl("Ass8Tam32",nombre)){
        nombreFila <- strsplit(nombre,"_Ass8Tam32")[[1]]
        Ass8Tam32 <- rbind(Ass8Tam32,resumenDistribucionTemporal(distribuciones[[i]],breaks,Tu))
        rownames(Ass8Tam32)[nrow(Ass8Tam32)] <- nombreFila
      } 
    }
  }
  Ass1Tam16 <- as.data.frame(Ass1Tam16)
  Ass1Tam32 <- as.data.frame(Ass1Tam32)
  Ass8Tam16 <- as.data.frame(Ass8Tam16)
  Ass8Tam32 <- as.data.frame(Ass8Tam32)
  return(list(Ass1Tam16=Ass1Tam16,Ass1Tam32=Ass1Tam32,
              Ass8Tam16=Ass8Tam16,Ass8Tam32=Ass8Tam32))
}

#Ejecución
nombres <- c("astar_Ass1Tam16",
             "astar_Ass1Tam32",
             "astar_Ass8Tam16",
             "astar_Ass8Tam32",
             "bzip2_Ass1Tam16",
             "bzip2_Ass1Tam32",
             "bzip2_Ass8Tam16",
             "bzip2_Ass8Tam32",
             "Firefox_Ass1Tam16",
             "Firefox_Ass1Tam32",
             "Firefox_Ass8Tam16",
             "Firefox_Ass8Tam32",
             "g++_Ass1Tam16",
             "g++_Ass1Tam32",
             "g++_Ass8Tam16",
             "g++_Ass8Tam32",
             "gcc_Ass1Tam16",
             "gcc_Ass1Tam32",
             "gcc_Ass8Tam16",
             "gcc_Ass8Tam32",
             "gobmk_Ass1Tam16",
             "gobmk_Ass1Tam32",
             "gobmk_Ass8Tam16",
             "gobmk_Ass8Tam32",
             "h264ref_Ass1Tam16",
             "h264ref_Ass1Tam32",
             "h264ref_Ass8Tam16",
             "h264ref_Ass8Tam32",
             "hmmer_Ass1Tam16",
             "hmmer_Ass1Tam32",
             "hmmer_Ass8Tam16",
             "hmmer_Ass8Tam32",
             "lbm_Ass1Tam16",
             "lbm_Ass1Tam32",
             "lbm_Ass8Tam16",
             "lbm_Ass8Tam32",
             "libquantum_Ass1Tam16",
             "libquantum_Ass1Tam32",
             "libquantum_Ass8Tam16",
             "libquantum_Ass8Tam32",
             "mcf_Ass1Tam16",
             "mcf_Ass1Tam32",
             "mcf_Ass8Tam16",
             "mcf_Ass8Tam32",
             "milc_Ass1Tam16",
             "milc_Ass1Tam32",
             "milc_Ass8Tam16",
             "milc_Ass8Tam32",
             "namd_Ass1Tam16",
             "namd_Ass1Tam32",
             "namd_Ass8Tam16",
             "namd_Ass8Tam32",
             "omnetpp_Ass1Tam16",
             "omnetpp_Ass1Tam32",
             "omnetpp_Ass8Tam16",
             "omnetpp_Ass8Tam32",
             "Openoffice_Ass1Tam16",
             "Openoffice_Ass1Tam32",
             "Openoffice_Ass8Tam16",
             "Openoffice_Ass8Tam32",
             "povray_Ass1Tam16",
             "povray_Ass1Tam32",
             "povray_Ass8Tam16",
             "povray_Ass8Tam32",
             "sjeng_Ass1Tam16",
             "sjeng_Ass1Tam32",
             "sjeng_Ass8Tam16",
             "sjeng_Ass8Tam32",
             "specrand_Ass1Tam16",
             "specrand_Ass1Tam32",
             "specrand_Ass8Tam16",
             "specrand_Ass8Tam32",
             "sphinx3_Ass1Tam16",
             "sphinx3_Ass1Tam32",
             "sphinx3_Ass8Tam16",
             "sphinx3_Ass8Tam32",
             "Xalan_Ass1Tam16",
             "Xalan_Ass1Tam32",
             "Xalan_Ass8Tam16",
             "Xalan_Ass8Tam32")



#todo <- leerTodo(nombres)
#save(todo,file="todo.RData")
load("todo.RData")

library(philentropy)

dendograma <- function(datos,type="euclidean"){
  if(type == "euclidean"){
    distancias <- dist(datos)
  }
  if(type == "JSD"){
    distancias <- JSD(as.matrix(datos))
    rownames(distancias) <- rownames(datos)
    distancias <- as.dist(distancias)
  }
  clus <- hclust(distancias,"ward.D2")
  plot(clus,hang=-1)
}

clustering <- function(datos,type="euclidean",nDiv=2){
  if(type == "euclidean"){
    distancias <- dist(datos)
  }
  if(type == "JSD"){
    distancias <- JSD(as.matrix(datos))
    rownames(distancias) <- rownames(datos)
    distancias <- as.dist(distancias)
  }
  clus <- hclust(distancias,"ward.D2")
  division <- cutree(clus,nDiv)
  grupos <- list()
  for(i in 1:nDiv){
    grupos[[i]] <- which(division == i)
  }
  names(grupos) <- paste("grupo",1:length(grupos),sep="")
  return(grupos)
}

graficosClustering <- function(grupos,histogramas,asociatividad,tamanyo) {
  nDiv <- length(grupos)
  tipo <- paste("Ass",asociatividad,"Tam",tamanyo,sep="")
  for(i in 1:nDiv){
    #X11()
    ncolumnas <- min(3,length(grupos[[i]]))
    nfilas <- ceiling(length(grupos[[i]])/ncolumnas)
    par(mfrow=c(nfilas,ncolumnas))
    for(j in 1:length(grupos[[i]])){
      nombre <- names(grupos[[i]])[j]
      elemento <- grupos[[i]][j]
      acceso <- accesosRepetidos[paste(nombre,tipo,sep="_")]
      plot(1:ncol(histogramas),histogramas[elemento,],type="h",lwd=4,
           ylab = "Frecuencia relativa",xlab="")
      title(main = paste("LLC ",tamanyo,"MB, asociat. ",asociatividad,"\n",nombre," (grupo ",i,")",sep=""),
                         sub = paste("Número de accesos repetidos: ",acceso,sep=""))
    }
  }
  par(mfrow=c(1,1))
}


graficosAplicacion <- function(bins,nombres){
  
  par(mfrow=c(2,2))
  
  for(nombre in nombres){
    
    png(paste("graficos Analisis LocTemp/",nombre,sep=""),width=650,height=480)
    
    dibujar <- bins$Ass1Tam16[nombre,]
    dibujar[is.na(dibujar)] <- 0
    plot(1:ncol(bins$Ass1Tam16),dibujar,type="h",lwd=4,
         ylab = "Percentaje",xlab="")
    title(main = paste("LLC 16MB, ASSOC. 1,",nombre),
          sub = paste("Off-chip repeated accesses: ",accesosRepetidos[paste(nombre,"_Ass1Tam16",sep="")],
                      sep=""))
    
    dibujar <- bins$Ass8Tam16[nombre,]
    dibujar[is.na(dibujar)] <- 0
    plot(1:ncol(bins$Ass8Tam16),dibujar,type="h",lwd=4,
         ylab = "Percentaje",xlab="")
    title(main = paste("LLC 16MB, ASSOC. 8,",nombre),
          sub = paste("Off-chip repeated accesses: ",accesosRepetidos[paste(nombre,"_Ass8Tam16",sep="")],
                      sep=""))
    
    dibujar <- bins$Ass1Tam32[nombre,]
    dibujar[is.na(dibujar)] <- 0
    plot(1:ncol(bins$Ass1Tam32),dibujar,type="h",lwd=4,
         ylab = "Percentaje",xlab="")
    title(main = paste("LLC 32MB, ASSOC. 1,",nombre),
          sub = paste("Off-chip repeated accesses: ",accesosRepetidos[paste(nombre,"_Ass1Tam32",sep="")],
                      sep=""))
    
    dibujar <- bins$Ass8Tam32[nombre,]
    dibujar[is.na(dibujar)] <- 0
    plot(1:ncol(bins$Ass8Tam32),dibujar,type="h",lwd=4,
         ylab = "Percentaje",xlab="")
    title(main = paste("LLC 32MB, ASSOC. 8,",nombre),
          sub = paste("Off-chip repeated accesses: ",accesosRepetidos[paste(nombre,"_Ass8Tam32",sep="")],
                      sep=""))
    
    dev.off()
  }
  
  par(mfrow=c(1,1))
  
}

par(mfrow=c(2,2))
plot(1:ncol(histogramas),histogramas["lbm",],type="h",lwd=4,
       ylab = "Percentaje",xlab="")
title(main = paste("LLC 16MB, ASSOC. 1, lbm"),
      sub = paste("Repeated accesses: ",accesosRepetidos["lbm_Ass1Tam16"],sep=""))



#Definición de un umbral y cálculo de los histogramas

Tu <- 1000000
todoOlvido <- eliminarValoresAltos(todo,Tu)
bins10 <- resumenTodoDistribucionTemporal(todoOlvido,breaks=10,remove=50,Tu)
bins100 <- resumenTodoDistribucionTemporal(todoOlvido,breaks=100,remove=50,Tu)


accesosRepetidos <- c()
for(i in 1:length(todo)){
  accesosRepetidos[i] <- length(todoOlvido[[i]])
}
names(accesosRepetidos) <- names(todo)


Tu <- 1000000
todoOlvido <- eliminarValoresAltos(todo,Tu)
bins100 <- resumenTodoDistribucionTemporal(todoOlvido,breaks=100,remove=50,Tu)
histogramas <- bins100$Ass1Tam16
graficosAplicacion(bins100,c("lbm","libquantum","omnetpp"))




par(mfrow=c(1,2))
plot(1:100,bins100$Ass1Tam16["Firefox",],type="h",
     xlab = "",ylab = "Frecuencia relativa",lwd=7)
title(main = "Firefox, LLC 16MB, asociat. 1",
      sub = paste("Número de accesos repetidos: ",accesosRepetidos["Firefox_Ass1Tam16"],sep=""))

plot(0:100,c(0,cumsum(as.numeric((bins100$Ass1Tam16["Firefox",])))),type="l",
     xlab = "",ylim=c(0,1),ylab = "Frecuencia relativa acumulada",lwd=7)
title(main = "Firefox, LLC 16MB, asociat. 1",
      sub = paste("Número de accesos repetidos: ",accesosRepetidos["Firefox_Ass1Tam16"],sep=""))
par(mfrow=c(1,1))



par(mfrow=c(2,2))
plot(1:100,bins100$Ass1Tam16["Firefox",],type="h",
     xlab = "",ylab = "Frecuencia relativa",lwd=7)
title(main = "Firefox, LLC 16MB, asociat. 1",
      sub = paste("Número de accesos repetidos: ",accesosRepetidos["Firefox_Ass1Tam16"],sep=""))

plot(1:100,bins100$Ass1Tam32["Firefox",],type="h",
     xlab = "",ylab = "Frecuencia relativa",lwd=7)
title(main = "Firefox, LLC 32MB, asociat. 1",
      sub = paste("Número de accesos repetidos: ",accesosRepetidos["Firefox_Ass1Tam32"],sep=""))

plot(1:100,bins100$Ass8Tam16["Firefox",],type="h",
     xlab = "",ylab = "Frecuencia relativa",lwd=7)
title(main = "Firefox, LLC 16MB, asociat. 8",
      sub = paste("Número de accesos repetidos: ",accesosRepetidos["Firefox_Ass8Tam16"],sep=""))

plot(1:100,bins100$Ass8Tam32["Firefox",],type="h",
     xlab = "",ylab = "Frecuencia relativa",lwd=7)
title(main = "Firefox, LLC 32MB, asociat. 8",
      sub = paste("Número de accesos repetidos: ",accesosRepetidos["Firefox_Ass8Tam32"],sep=""))

###############################
# Agrupamiento para Ass1Tam16 #
###############################

par(mfrow=c(1,1))
dendograma(bins10$Ass1Tam16,type="JSD")
grupos <- clustering(bins10$Ass1Tam16,type="JSD",nDiv=5)
grupos
graficosClustering(grupos,bins100$Ass1Tam16,1,16)


###############################
# Agrupamiento para Ass1Tam32 #
###############################

par(mfrow=c(1,1))
dendograma(bins10$Ass1Tam32,type="JSD")
grupos <- clustering(bins10$Ass1Tam32,type="JSD",nDiv=5)
grupos
graficosClustering(grupos,bins100$Ass1Tam32,1,32)


###############################
# Agrupamiento para Ass8Tam16 #
###############################

par(mfrow=c(1,1))
dendograma(bins10$Ass8Tam16,type="JSD")
grupos <- clustering(bins10$Ass8Tam16,type="JSD",nDiv=4)
grupos
graficosClustering(grupos,bins100$Ass8Tam16,8,16)



###############################
# Agrupamiento para Ass8Tam32 #
###############################

par(mfrow=c(1,1))
dendograma(bins10$Ass8Tam32,type="JSD")
grupos <- clustering(bins10$Ass8Tam32,type="JSD",nDiv=2)
grupos
graficosClustering(grupos,bins100$Ass8Tam32,8,32)


# Gráficos Paper
graficosAplicacion(bins100,c("lbm","libquantum","omnetpp"))

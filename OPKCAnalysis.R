#Este script se va a utilizar para analizar la densidad de llamadas a memoria,
#medida en OPKC (operaciones por kilo ciclo).
#Para cada aplicacion, se tienen datos sobre el OPKC cada 1000 ciclos.

#Librerías
library(parallel)

#FUNCIONES

#Lee el fichero opkc, que contiene información sobre la densidad de accesos a
#memoria. Devuelve los OPKC "instantáneos", midiéndo el número de accesos cada
#1000 ciclos
densidad <- function(file){
  y <- scan(file,skip=1)
  return(y)
}


#Lee todos los datos de la carpeta opkc, con extensión .opkc
#Lo almacena en una lista en ese mismo orden y lo devuelve
leerTodo <- function(file){
  devolver <- list()
  i <- 1
  n <- length(nombres)
  for(nombre in nombres){
    nombreCompleto <- paste("opkc/",nombre,".opkc",sep="")
    cat("Leyendo fichero ",nombre," (",i," de ",n,")\n",sep="")
    devolver[[i]] <- densidad(nombreCompleto)
    i <- i+1
  }
  names(devolver) <- nombres
  return(devolver)
}

#Esta función recibe un vector con datos sobre el OPKC a lo largo de la
#ejecucion, y reduce la granularidad del mismo para que el vector resultante
#tenga b valores. Esto supone hacer la media del OPKC por trozos, y permite
#representar el comportamiento de la aplicación de forma gŕafica para ver
#la densidad de accesos a memoria.
reducirGranularidad <- function(y,b=2000){
  n <- length(y)
  aux <- 1:n
  indices <- cut(aux,breaks=b,labels=as.character(1:b))
  yred <- tapply(y,indices,mean)
  return(yred)
}

#Establece una representación gráfica para el OPKC de la aplicación seleccionada. El argumento b
#es la granularidad de la representación. Si la aplicacion no se ha ejecutado el mismo número de ciclos
#en las cuatro configuraciones, entonces se rellena con ceros. La media se calcula antes de rellenar con ceros.
#Si export = T, se guarda en el directorio especificado
representar <- function(nombre,b=3000,export=F,dir=NULL){
  
  nombreAss1Tam16 <- paste(nombre,"_Ass1Tam16",sep="")
  nombreAss1Tam32 <- paste(nombre,"_Ass1Tam32",sep="")
  nombreAss8Tam16 <- paste(nombre,"_Ass8Tam16",sep="")
  nombreAss8Tam32 <- paste(nombre,"_Ass8Tam32",sep="")
  yAss1Tam16 <- todo[[nombreAss1Tam16]]
  yAss1Tam32 <- todo[[nombreAss1Tam32]]
  yAss8Tam16 <- todo[[nombreAss8Tam16]]
  yAss8Tam32 <- todo[[nombreAss8Tam32]]
  
  mediaAss1Tam16 <- mean(yAss1Tam16)
  mediaAss1Tam32 <- mean(yAss1Tam32)
  mediaAss8Tam16 <- mean(yAss8Tam16)
  mediaAss8Tam32 <- mean(yAss8Tam32)
  
  n <- max(length(yAss1Tam16),length(yAss1Tam32),length(yAss8Tam16),length(yAss8Tam32))
  
  yAss1Tam16 <- c(yAss1Tam16,rep(0,n-length(yAss1Tam16)))
  yAss1Tam32 <- c(yAss1Tam32,rep(0,n-length(yAss1Tam32)))
  yAss8Tam16 <- c(yAss8Tam16,rep(0,n-length(yAss8Tam16)))
  yAss8Tam32 <- c(yAss8Tam32,rep(0,n-length(yAss8Tam32)))
  
  obj <- mclapply(list(yAss1Tam16,yAss1Tam32,yAss8Tam16,yAss8Tam32), reducirGranularidad, b,mc.cores=4)
  yredAss1Tam16 <- obj[[1]]
  yredAss1Tam32 <- obj[[2]]
  yredAss8Tam16 <- obj[[3]]
  yredAss8Tam32 <- obj[[4]]
  opkcmax <- max(yredAss1Tam16,yredAss1Tam32,yredAss8Tam16,yredAss8Tam32)
  opkcleg <- 8*max(mediaAss1Tam16,mediaAss1Tam32,mediaAss8Tam16,mediaAss8Tam32)
  while(2*opkcleg > opkcmax){
    opkcleg <- opkcleg/2
  }
  numInt <- 5
  ejeAt <- round(0:numInt*b/numInt)
  ejeLabels <- round(0:numInt*n/(numInt*1000))
  
  nombreOPKCMax <- paste(dir,"/",nombre,"_escalaGrande.jpg",sep="")
  if(export) jpeg(nombreOPKCMax,width=650,height = 400)
  par(mfrow=c(2,2))
  plot(yredAss1Tam16,type="l",ylim=c(0,opkcmax),xaxt="n",xlab="Millions of cycles",ylab="OPKC")
  axis(1,at=ejeAt,labels = ejeLabels)
  title(main=paste(nombre,", LLC 16MB ASSOC. 1",sep=""),sub=paste("Mean OPKC:",round(mediaAss1Tam16,3)))
  
  plot(yredAss1Tam32,type="l",ylim=c(0,opkcmax),xaxt="n",xlab="Millions of cycles",ylab="OPKC")
  axis(1,at=ejeAt,labels = ejeLabels)
  title(main=paste(nombre,", LLC 32MB ASSOC. 1",sep=""),sub=paste("Mean OPKC:",round(mediaAss1Tam32,3)))
  
  plot(yredAss8Tam16,type="l",ylim=c(0,opkcmax),xaxt="n",xlab="Millions of cycles",ylab="OPKC")
  axis(1,at=ejeAt,labels = ejeLabels)
  title(main=paste(nombre,", LLC 16MB ASSOC. 8",sep=""),sub=paste("Mean OPKC:",round(mediaAss8Tam16,3)))
  
  plot(yredAss8Tam32,type="l",ylim=c(0,opkcmax),xaxt="n",xlab="Millions of cycles",ylab="OPKC")
  axis(1,at=ejeAt,labels = ejeLabels)
  title(main=paste(nombre,", LLC 32MB ASSOC. 8",sep=""),sub=paste("Mean OPKC:",round(mediaAss8Tam32,3)))
  if(export) dev.off()
  
  nombreOPKCLeg<- paste(dir,"/",nombre,"_escalaCorta.jpg",sep="")
  if(export) jpeg(nombreOPKCLeg,width=650,height = 400)
  par(mfrow=c(2,2))
  plot(yredAss1Tam16,type="l",ylim=c(0,opkcleg),xaxt="n",xlab="Millions of cycles",ylab="OPKC")
  axis(1,at=ejeAt,labels = ejeLabels)
  title(main=paste(nombre,", LLC 16MB ASSOC. 1",sep=""),sub=paste("Mean OPKC:",round(mediaAss1Tam16,3)))
  
  plot(yredAss1Tam32,type="l",ylim=c(0,opkcleg),xaxt="n",xlab="Millions of cycles",ylab="OPKC")
  axis(1,at=ejeAt,labels = ejeLabels)
  title(main=paste(nombre,", LLC 32MB ASSOC. 1",sep=""),sub=paste("Mean OPKC:",round(mediaAss1Tam32,3)))
  
  plot(yredAss8Tam16,type="l",ylim=c(0,opkcleg),xaxt="n",xlab="Millions of cycles",ylab="OPKC")
  axis(1,at=ejeAt,labels = ejeLabels)
  title(main=paste(nombre,", LLC 16MB ASSOC. 8",sep=""),sub=paste("Mean OPKC:",round(mediaAss8Tam16,3)))
  
  plot(yredAss8Tam32,type="l",ylim=c(0,opkcleg),xaxt="n",xlab="Millions of cycles",ylab="OPKC")
  axis(1,at=ejeAt,labels = ejeLabels)
  title(main=paste(nombre,", LLC 32MB ASSOC. 8",sep=""),sub=paste("Mean OPKC:",round(mediaAss8Tam32,3)))
  if(export) dev.off()
  
  par(mfrow=c(1,1))
}

representarTodo <- function(apps,b=3000,dir=NULL){
  for(app in apps){
    cat("Representando:",app,"\n")
    representar(app,b,export=T,dir=dir)
  }
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

#nombres <- c(
#  "mcf_Ass1Tam16_5M",
#  "mcf_Ass1Tam32_5M",
#  "mcf_Ass8Tam16_5M",
#  "mcf_Ass8Tam32_5M"
#)

todo <- leerTodo(nombres)

#names(todo) <- c("mcf_Ass1Tam16","mcf_Ass1Tam32","mcf_Ass8Tam16","mcf_Ass8Tam32")

apps <- c("astar","bzip2","Firefox","g++","gcc","gobmk","h264ref","hmmer","lbm","libquantum","mcf","milc","namd","omnetpp","Openoffice",
          "povray","sjeng","specrand","sphinx3","Xalan")


representar("mcf",3000,export = T, dir = ".")

representarTodo(apps,3000,"graficosOPKC")

#Para caracterizar los datos:
#Numero de outliers
#Varianza muestral
#Media
#Num secciones con OPKC > 0
#Duracion de la rafaga más grande
#Duracion de la rafaga de alta densidad más grande
#ACF

#Calcula la media de todos los elementos que hay en lista
media <- function(lista){
  resultados <- c()
  for(y in lista){
    v <- mean(y)
    resultados <- c(resultados,v)
  }
  names(resultados) <- names(lista)
  return(resultados)  
}

#Calcula la varianza de todos los elementos que hay en lista
varianza <- function(lista){
  resultados <- c()
  for(y in lista){
    v <- var(y)
    resultados <- c(resultados,v)
  }
  names(resultados) <- names(lista)
  return(resultados)
}

#Calcula el porcentaje de actividad de la memoria.
#Se considera que la memoria esta activa si en una seccion de 1000 ciclos (1KC)
#ha habido al menos un acceso a memoria
actividad <- function(lista,porcentaje=T){
  resultados <- c()
  for(y in lista){
    num <- sum(as.numeric(y > 0))
    if(porcentaje){
      num <- num/length(y)
    }
    resultados <- c(resultados,num)
  }
  names(resultados) <- names(lista)
  return(resultados)  
}

#Calcula el porcentaje de outliers de la serie temporal que es representada
#por el número de accesos a memoria. El valor de k determina a partir de cuanto
#se consideran outliers. Se consudera outlier si > 3Q + k*IQR
numOutliers <- function(lista,k=3,porcentaje=T){
  resultados <- c()
  for(y in lista){
    quartiles <- quantile(y,c(0.25,0.75))
    q25 <- quartiles[1]
    q75 <- quartiles[2]
    IQR <- q75-q25
    limite <- q75 + 3*IQR
    num <- sum(as.numeric(y > limite))
    if(porcentaje){
      num <- num/length(y)
    }
    resultados <- c(resultados,num)
  }
  names(resultados) <- names(lista)
  return(resultados)
}

#Calcula la duración de la ráfaga de actividad más grande.
#Por defecto, relative = F --> se tiene en cuenta el numero de kilociclos
#de la rafaga mas grande. Si relative = T, se mide en porcentaje con respecto
#a la duracion total del programa
rafaga <- function(lista,relative=F){
  resultados <- c()
  for(y in lista){
    aux <- as.numeric(y > 0)
    dur <- 0
    maxdur <- 0
    for(e in aux){
      if(e == 1){
        dur <- dur + 1
      } else {
        if(dur > 0){
          if(dur > maxdur){
            maxdur <- dur
          }
          dur <- 0
        }
      }
    }
    if(dur > maxdur){
      maxdur <- dur
    }
    resultados <- c(resultados,maxdur)
  }
  names(resultados) <- names(lista)
  return(resultados)
}


#Calcula la duración de la ráfaga de alta actividad más grande.
#Se considera una seccion de alta densidad si OPKC > 10*media
#Por defecto, relative = F --> se tiene en cuenta el numero de kilociclos
#de la rafaga mas grande. Si relative = T, se mide en porcentaje con respecto
#a la duracion total del programa
rafagaAltaDensidad <- function(lista,relative=F){
  resultados <- c()
  for(y in lista){
    limit <- 10*mean(y)
    aux <- as.numeric(y > limit)
    dur <- 0
    maxdur <- 0
    for(e in aux){
      if(e == 1){
        dur <- dur + 1
      } else {
        if(dur > 0){
          if(dur > maxdur){
            maxdur <- dur
          }
          dur <- 0
        }
      }
    }
    if(dur > maxdur){
      maxdur <- dur
    }
    resultados <- c(resultados,maxdur)
  }
  names(resultados) <- names(lista)
  return(resultados)
}

#Calcula la función de autocorrelación. Devuelve la autocorrelación
#en el retardo indicado (por defecto 60), que puede considerarse una medida
#de la dependencia de una observacion con las pasadas.
#Esto no mide estacionalidad
autocorrelacion <- function(lista,retardo=60){
  resultados <- c()
  for(y in lista){
    obj <- acf(y,type="correlation",plot=F)
    corr <- as.numeric(obj$acf[retardo+1])
    resultados <- c(resultados,corr)
  }
  names(resultados) <- names(lista)
  return(resultados)
}


#Calculamos algunos resumenes
medias <- media(todo)
varianzas <- varianza(todo)
actividades <- actividad(todo)
outliers <- numOutliers(todo)
rafagas <- rafaga(todo)
rafagasAltaDensidad <- rafagaAltaDensidad(todo)
autocorrelaciones <- autocorrelacion(todo)
save(medias,varianzas,actividades,outliers,rafagas,rafagasAltaDensidad,autocorrelaciones,
     file="resumenes.RData")

load("resumenes.RData")
resumenes <- cbind(medias,varianzas,100*actividades,100*outliers,rafagas,rafagasAltaDensidad)

library(xtable)
xtable(resumenes,digits=c(0,4,4,2,2,0,0),display=c("s","f","f","f","f","d","d"))


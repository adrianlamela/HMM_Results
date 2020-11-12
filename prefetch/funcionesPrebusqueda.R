library(MASS)
library(data.table)
library(forecast)
library(stringr)

options(digits=22)

#tamDireccion se indica en bits
direccionIntToHex <- function(x,tamDireccion = 48){
  from <- c("10","11","12","13","14","15")
  to <- c("A","B","C","D","E","F")
  reminder <- c()
  while(x > 0){
    reminder <- c(reminder,x%%16)
    x <- floor(x/16)
  }
  reminder <- as.character(rev(reminder))
  for(i in 1:length(reminder)){
    indReemplazo <- which(reminder[i] == from)
    if(length(indReemplazo)>0){
      reminder[i] <- to[indReemplazo]
    }
  }
  reminder <- c(rep("0",max(0,tamDireccion/4-length(reminder))),reminder)
  result <- paste(reminder, collapse = '')
  result <- paste("0x",result,sep="")
  return(result)
}

direccionIntToBin <- function(x, tamDireccion = 48){
  dir <- strsplit(direccionIntToHex(x,tamDireccion),"")[[1]][-c(1:2)]
  from <- c("0","1","2","3","4","5","6","7","8","9","A","B","C","D","E","F")
  to <- c("0000","0001","0010","0011","0100","0101","0110","0111","1000","1001","1010","1011","1100","1101","1110","1111")
  for(i in 1:length(dir)){
    indReemplazo <- which(dir[i] == from)
    dir[i] <- to[indReemplazo]
  }
  dir <- paste(dir, collapse = '')
  dir <- substr(dir,str_length(dir)-tamDireccion+1,str_length(dir))
  return(dir)
}

direccionBinToInt <- function(x){
  secuencia <- as.numeric(unlist(strsplit(x,"")))
  lonSec <- length(secuencia)
  numero <- sum(rev(2^(0:(lonSec-1)))*secuencia)
  return(numero)
}

decimalplaces <- function(x) {
  if (abs(x - round(x)) > .Machine$double.eps^0.5) {
    nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed = TRUE)[[1]][[2]])
  } else {
    return(0)
  }
}

leerMezcla <- function(file,limite=NULL){
  nombre <- paste("mezcla/",file,sep="")
  data <- fread(nombre,header=F,colClasses = c("double","string","double"))
  if(!is.null(limite)){
    data <- data[1:limite,]
  }
  colnames(data) <- c("t","type","tag")
  return(data)
}

leerOrdenar <- function(file,limite=NULL){
  nombre <- paste("prebus/",file,sep="")
  data <- fread(nombre,header=T,colClasses = c("integer","double","integer","string"))
  o <- order(data$t)
  dataord <- data[o,]
  if(!is.null(limite)){
    limite <- min(limite,nrow(data))
    dataord <- dataord[1:limite,]
  }
  return(dataord)
}

pertenece <- function(elemento,vector){
  for(e in vector){
    if (elemento == e) {
      return(TRUE)
    }
  }
  return(FALSE)
}

rango <- function(v){
  r <- range(v)
  return(r[2]-r[1])
}

#Inicializa una cache
#Hay que proporcionar el tamaño de la cache y una de las dos siguientes cosas:
#1. Asociatividad: número de líneas de cada conjunto
#2. Número de conjuntos
inicializarCache <- function(tamB,asociatividad=NULL,numConjuntos=NULL, umbralOlvido = Inf, pasoAumentoMax = 1000000, tamLineaB = 64){
  if((is.null(asociatividad) & is.null(numConjuntos)) | (!is.null(asociatividad) & !is.null(numConjuntos))){
    stop("Hay que proporcionar o la asociatividad o el numero de conjuntos")
  }
  if(is.null(numConjuntos)){
   numConjuntos <- tamB/(tamLineaB*asociatividad)
  }
  
  #Calculo del valor que falta
  if(is.null(asociatividad)){
   asociatividad <- tamB/(tamLineaB*numConjuntos)
  }
  if(decimalplaces(log2(tamB))>0 | decimalplaces(log2(asociatividad))>0 | decimalplaces(log2(numConjuntos))>0){
    stop("Los argumentos para el tamaño, asociatividad y numero de conjuntos deben ser potencias de 2")
  }
  cat("Inicializando cache... \n")
  
  
  cache <<- list(tamB=tamB,asociatividad=asociatividad,numConjuntos=numConjuntos,
                 umbralOlvido=umbralOlvido,pasoAumentoMax=pasoAumentoMax,
                 tamLineaB=tamLineaB)
  mAux <- matrix(NA_real_,nrow=min(pasoAumentoMax,asociatividad),ncol=4)
  colnames(mAux) <- c("tag","prebuscada","tiempo","numUsos")
  cache <<- c(cache,rep(list(mAux),numConjuntos))

  
  #Inicializar todos los conjuntos de la cache: a lo bestia
  cat("|--------------------------------------------------|\n")
  imprimirProgreso <- round((1:50)*numConjuntos/50)
  indImprimirProgreso <- 1
  imprimirProgresoSig <- imprimirProgreso[indImprimirProgreso]
  cat("|")
  for(i in 0:(numConjuntos-1)){
    if((i+1) == imprimirProgresoSig){
      cat("=")
      indImprimirProgreso <- indImprimirProgreso + 1
      imprimirProgresoSig <- imprimirProgreso[indImprimirProgreso]
    }
    dirConj <- direccionIntToBin(i,tamDireccion = log2(numConjuntos))
    names(cache)[i+7] <<- dirConj
  }
  cat("|\n")
  
  cat("Cache de ",tamB/2**20,"MB, asociatividad ",asociatividad," y ",numConjuntos," conjuntos\n",sep="")
  
  entradasOlvidadas <<- matrix(NA_real_,nrow=cache$pasoAumentoMax,ncol=4)
  colnames(entradasOlvidadas) <<- c("tag","prebuscada","tiempo","numUsos")
  entradasOlvidadas.indice <<- 1

}

partesDireccionBinaria <- function(direccionBinaria){
  numBitsOffset <- log2(cache$tamLineaB)
  numBitsConj <- log2(cache$numConjuntos)
  numBitsEtiqueta <- str_length(direccionBinaria)-numBitsOffset-numBitsConj
  parte1 <- substr(direccionBinaria,1,numBitsEtiqueta)
  parte2 <- substr(direccionBinaria,numBitsEtiqueta+1,numBitsEtiqueta+numBitsConj)
  if(str_length(parte2) == 0){
    parte2 <- "0"
  }
  return(list(etiqueta=parte1,numConj=parte2))
}


aumentarEspacioConjunto <- function(indConjunto){
  numFilasNuevas <- min(cache$pasoAumentoMax,cache$asociatividad-nrow(cache[[indConjunto]]))
  cache[[indConjunto]] <<- rbind(cache[[indConjunto]],matrix(nrow=numFilasNuevas,ncol=4))
}

aumentarEspacioEntradasOlvidadas <- function(){
  entradasOlvidadas <<- rbind(entradasOlvidadas,matrix(nrow=cache$pasoAumentoMax,ncol=4))
}

#Si existe una entrada en la cache (teniendo en cuenta un umbral de olvido si se ha establecido), 
#devuelve TRUE (acierto de cache) y registra su uso
#Si no existe una entrada en la cache, devuelve FALSE y crea la entrada
registrarUso <- function(tag,tiempo){
  
  #Comprobacion rutinaria para entradasOlvidadas
  if(entradasOlvidadas.indice > nrow(entradasOlvidadas)){
    aumentarEspacioEntradasOlvidadas()
  }
  
  dir <- direccionIntToBin(tag)
  conj <- partesDireccionBinaria(dir)[[2]]
  indConj <- direccionBinToInt(conj) + 7
  
  #Comprobamos si la entrada ya existe
  indice <- which(cache[[indConj]][,1] == tag)
  existe <- length(indice) == 1
  
  if(!existe){
    
    #Hay que añadir la entrada
    entrada <- c(tag,FALSE,tiempo,1)
    
    #Hay que comprobar si hay espacio
    hayEspacio <- any(is.na(cache[[indConj]][,1]))
    
    if(hayEspacio){
      indice <- min(which(is.na(cache[[indConj]][,1])))
      cache[[indConj]][indice,] <<- entrada
    } else {
      
      #Puede no haber espacio por dos motivos: el conjunto esta lleno o aun se pueden añadir entradas
      estaLleno <- nrow(cache[[indConj]]) == cache$asociatividad
      
      if(estaLleno){
        
        #Politica de reemplazo LRU
        tiempos <- cache[[indConj]][,3]
        minTiempos <- min(tiempos)
        posibilidades <- which(tiempos == minTiempos)
        indice <- sample(posibilidades,1)
        
        entradaVieja <- cache[[indConj]][indice,]
        entradasOlvidadas[entradasOlvidadas.indice,] <<- entradaVieja
        entradasOlvidadas.indice <<- entradasOlvidadas.indice + 1
        
        cache[[indConj]][indice,] <<- entrada
        
      } else {
        #Se pueden añadir más filas
        aumentarEspacioConjunto(indConj)
        indice <- min(which(is.na(cache[[indConj]][,1])))
        cache[[indConj]][indice,] <<- entrada
      }
      
    }
    
    #Devolvemos FALLO de cache
    return(FALSE)
    
  } else {
    
    #La entrada existe
    
    #Si el tiempo es menor que un umbral de olvido, se registra su uso
    if(cache[[indConj]][indice,3] + cache$umbralOlvido >= tiempo){
      cache[[indConj]][indice,3] <<- tiempo
      cache[[indConj]][indice,4] <<- cache[[indConj]][indice,4]+1
      return(TRUE)
      
      #En caso contrario, se toma como fallo de cache y se sobreescribe la entrada por otra nueva
    } else {
      entradaVieja <- cache[[indConj]][indice,]
      entradasOlvidadas[entradasOlvidadas.indice,] <<- entradaVieja
      entradasOlvidadas.indice <<- entradasOlvidadas.indice + 1
      
      entrada <- c(tag,FALSE,tiempo,1)
      cache[[indConj]][indice,] <<- entrada
      return(FALSE)
    }
  }
}

prebuscar <- function(tag,tiempo,tamDireccion=48){
  
  if(tag >= (2**tamDireccion)){
    return()
  }
  
  #Comprobacion rutinaria para entradasOlvidadas
  #Comprobacion rutinaria para entradasOlvidadas
  if(entradasOlvidadas.indice > nrow(entradasOlvidadas)){
    aumentarEspacioEntradasOlvidadas()
  }
  
  dir <- direccionIntToBin(tag)
  conj <- partesDireccionBinaria(dir)[[2]]
  indConj <- direccionBinToInt(conj) + 7
  
  #Comprobamos si la entrada ya existe
  indice <- which(cache[[indConj]][,1] == tag)
  existe <- length(indice) == 1
  
  if(!existe){
    
    #Hay que añadir la entrada
    entrada <- c(tag,TRUE,tiempo,0)
    
    #Hay que comprobar si hay espacio
    hayEspacio <- any(is.na(cache[[indConj]][,1]))
    
    if(hayEspacio){
      indice <- min(which(is.na(cache[[indConj]][,1])))
      cache[[indConj]][indice,] <<- entrada
    } else {
      
      #Puede no haber espacio por dos motivos: el conjunto esta lleno o aun se pueden añadir entradas
      estaLleno <- nrow(cache[[indConj]]) == cache$asociatividad
      
      if(estaLleno){
        
        #Politica de reemplazo LRU
        tiempos <- cache[[indConj]][,3]
        minTiempos <- min(tiempos)
        posibilidades <- which(tiempos == minTiempos)
        indice <- sample(posibilidades,1)
        
        entradaVieja <- cache[[indConj]][indice,]
        entradasOlvidadas[entradasOlvidadas.indice,] <<- entradaVieja
        entradasOlvidadas.indice <<- entradasOlvidadas.indice + 1
        
        cache[[indConj]][indice,] <<- entrada
        
      } else {
        #Se pueden añadir más filas
        aumentarEspacioConjunto(indConj)
        indice <- min(which(is.na(cache[[indConj]][,1])))
        cache[[indConj]][indice,] <<- entrada
      }
      
    }
    
  } else {
    
    #La entrada existe
    
    #Si el tiempo es menor que un umbral de olvido, se modifica el tiempo para que dure más
    if(cache[[indConj]][indice,3] + cache$umbralOlvido >= tiempo){
      cache[[indConj]][indice,3] <<- tiempo
      
      #En caso contrario, se toma como fallo de cache y se sobreescribe la entrada por otra nueva
    } else {
      entradaVieja <- cache[[indConj]][indice,]
      entradasOlvidadas[entradasOlvidadas.indice,] <<- entradaVieja
      entradasOlvidadas.indice <<- entradasOlvidadas.indice + 1
      
      entrada <- c(tag,TRUE,tiempo,0)
      cache[[indConj]][indice,] <<- entrada
    }
  }
}

moverEntradasViejas <- function(tiempo){
  
  if(cache$umbralOlvido != Inf){
    
    huecosEntradasOlvidadas <- sum(is.na(entradasOlvidadas[,1]))
    
    for(conj in 7:length(cache)){
      i <- 1
      while(i <= nrow(cache[[conj]])){
        if(is.na(cache[[conj]][i,1])){
          break
        }
        entrada <- cache[[conj]][i,]
        if(entrada[3] + cache$umbralOlvido < tiempo){
          if(huecosEntradasOlvidadas == 0){
            aumentarEspacioEntradasOlvidadas()
            huecosEntradasOlvidadas <- huecosEntradasOlvidadas + cache$pasoAumentoMax
          }
          entradasOlvidadas[entradasOlvidadas.indice,] <<- entrada
          entradasOlvidadas.indice <<- entradasOlvidadas.indice + 1
          huecosEntradasOlvidadas <- huecosEntradasOlvidadas - 1
          cache[[conj]] <<- cache[[conj]][-i,]
          i <- i-1
        }
        i <- i+1
      }
    }
  }
}

eliminarNAs <- function(){
  if(class(cache)=="list"){
    for(conj in 7:length(cache)){
      cache[[conj]] <<- cache[[conj]][!is.na(cache[[conj]][,1]),]
      if(class(cache[[conj]])!="matrix"){
        cache[[conj]] <<- t(as.matrix(cache[[conj]]))
      }
    }
  }
  if(class(cache)=="matrix"){
    cache <<- cache[!is.na(cache[,1]),]
  }
  entradasOlvidadas <<- entradasOlvidadas[!is.na(entradasOlvidadas[,1]),]
}


estadisticasCache <- function(prebusqueda,print=T){
  
  eliminarNAs()
  
  if(class(cache) == "list"){
    entradasTotales <- matrix(nrow=0,ncol=4)
    colnames(entradasTotales) <- c("tag","prebuscada","tiempo","numUsos")
    entradasTotales <- do.call("rbind",cache[7:length(cache)])
    entradasTotales <- entradasTotales[!is.na(entradasTotales[,1]),]
    numEntradas <- nrow(entradasTotales)
    
    entradasTotales <- rbind(entradasTotales,entradasOlvidadas)
    entradasTotales <- entradasTotales[!is.na(entradasTotales[,1]),]
    cachePrebuscada <- entradasTotales[entradasTotales[,2]==1,]
    cacheNoPrebuscada <- entradasTotales[entradasTotales[,2]==0,]
    
    n <- length(prebusqueda$S)
    numEntradasTotales <- nrow(entradasTotales)
  }
  
  if(class(cache) == "matrix"){
    entradasTotales <- rbind(cache,entradasOlvidadas)
    entradasTotales <- entradasTotales[!is.na(entradasTotales[,1]),]
    cachePrebuscada <- entradasTotales[entradasTotales[,2]==1,]
    cacheNoPrebuscada <- entradasTotales[entradasTotales[,2]==0,]
    
    n <- length(prebusqueda$S)
    numEntradas <- nrow(cache)
    numEntradasTotales <- nrow(entradasTotales)
  }
  
  numReemplazos <- nrow(entradasOlvidadas)

  if(any(prebusqueda$S==0)){
    porcentajeReconocido <- 100*(1-table(prebusqueda$S)[1]/length(prebusqueda$S))
  } else {
    porcentajeReconocido <- 100
  }
  porcentajePrebuscado <- 100*nrow(cachePrebuscada)/nrow(entradasTotales)
  porcentajeDesperdicio <- 100*sum(entradasTotales[,4] == 0)/nrow(entradasTotales)
  porcentajePredicho <- 100*sum(prebusqueda$acierto)/n
  if(class(cachePrebuscada) == "matrix"){
    porcentajeAciertosPorPrebusqueda <- 100*sum(cachePrebuscada[,4])/sum(prebusqueda$acierto)
    porcentajePrebusquedasUtiles <- 100*sum(cachePrebuscada[,4]>0)/nrow(cachePrebuscada)
  } else {
    porcentajeAciertosPorPrebusqueda <- 100*sum(cachePrebuscada[4])/sum(prebusqueda$acierto)
    porcentajePrebusquedasUtiles <- 100*sum(cachePrebuscada[4]>0)/nrow(cachePrebuscada)
  }
  
  if(print){
    cat("REPORT DE UTILIZACIÓN DE CACHÉ \n\n")
    cat("Se han examinado ",length(prebusqueda$S)," accesos de memoria\n")
    cat("Se ha reconocido el ",porcentajeReconocido,"% de las líneas dentro de grupos\n")
    cat("El número de entradas que han pasado por la cache es: ",numEntradasTotales,"\n",sep = "")
    cat("El número de entradas que hay actualmente en la caché es: ",numEntradas,"\n",sep = "")
    cat("El número de reemplazos que han ocurrido es: ",numReemplazos,"\n",sep = "")
    cat("(Dos líneas que se han usado muy lejanamente cuentan como dos distintas)\n")
    cat("El porcentaje de utilización de la caché debido a la prebúsqueda es: ",porcentajePrebuscado,"%\n",sep="")
    cat("El porcentaje de caché desperdiciada (líneas no usadas nunca) es: ",porcentajeDesperdicio,"%\n",sep="")
    cat("El porcentaje de aciertos de caché es: ",porcentajePredicho,"%\n",sep="")
    cat("De todos los aciertos de caché, el ",porcentajeAciertosPorPrebusqueda,"% se debe a la prebúsqueda\n",sep="")
    cat("El porcentaje de prebúsquedas que han sido realmente utilizadas es: ",porcentajePrebusquedasUtiles,"%\n",sep="")
    cat("-----------------------------------------------------------------------------------\n\n")
  }
  
  return(list(numAccesosExaminados = length(prebusqueda$S),porcentajeReconocido=porcentajeReconocido,numEntradasTotales=numEntradasTotales,
              numEntradasActuales=numEntradas,numReemplazos=numReemplazos,porcentajePrebuscado=porcentajePrebuscado,porcentajeDesperdicio=porcentajeDesperdicio,
              porcentajeExito=porcentajePredicho,porcentajeAciertosPorPrebusqueda=porcentajeAciertosPorPrebusqueda,
              porcentajePrebusquedasUtiles=porcentajePrebusquedasUtiles))
}

#No realiza prebúsqueda; solo pasa por la cache las direcciones de memoria
registrarUsoCompleto <- function(y,nombreSalida,tamLineaB = 64,verbose=T,limpiarCada=500000){
  y <- y/tamLineaB
  numGuardado <- 1
  acierto <- rep(FALSE,length(y))
  miny <- min(y)
  y <- y - miny
  for(i in 1:(length(y))){
    acierto[i] <- registrarUso(tamLineaB*y[i],i)
    if(verbose & (i %% 5000 == 0)) cat ("Examinados ",i," elementos (",round(100*i/length(y),2),"%)\n",sep="")
    if(i %% limpiarCada == 0){
      if(verbose) cat("Limpiando entradas antiguas de la cache\n")
      moverEntradasViejas(i)
      
      #Guardando resultados intermedios
      cat("Guardando resultados intermedios\n")
      aciertoAux <- acierto[1:i]
      S <- rep(0,i)
      muS <- NULL
      sigmaS <- NULL
      resultados <- list(S=S,muS=muS+miny,sigmaS=sigmaS,acierto=aciertoAux)
      
      save(resultados,cache,entradasOlvidadas,file=paste(nombreSalida,numGuardado,sep="-"))
      numGuardado <- numGuardado + 1
      cat("Se han guardado resultados intermedios\n")
    }
  }
  
  moverEntradasViejas(length(y))
  S <- rep(0,i)
  muS <- NULL
  sigmaS <- NULL
  resultados <- list(S=S,muS=muS+miny,sigmaS=sigmaS,acierto=acierto)
  
  save(resultados,cache,entradasOlvidadas,file=nombreSalida)
  cat("FIN\n")
  
  return(resultados)
}

prebusqueda <- function(y,nombreSalida,prebuscador=T,cachear=T,tamLineaB = 64,verbose=T,longini = 5,tol=0.00005, umbralAdmitidos = 100, 
                           umbralInicio = 3, maxSigma = 200, longitudModelo = 500, umbralOlvidoGrupos = 250000,
                           alpha = 0.9, limpiarCada = 500000, debug=F) {
  
  #Busqueda a nivel de linea, no de direccion de memoria
  y <- y/tamLineaB
  
  #Se suprimen los molestos warning en el ajuste
  oldw <- getOption("warn")
  options(warn = -1)
  
  #Inicializar la cache
  acierto <- rep(FALSE,length(y))
  
  #Se trabaja en una base mas pequeÃ±a para prevenir el desbordamiento
  miny <- min(y)
  y <- y - miny
  
  #Se empieza con un vector de longitud longini
  #Se va avanzando en el vector inicial y se construye un dendograma
  #hasta que la diferencia de alturas sea menor de 3 y todas las observaciones
  #se puedan considerar en el mismo grupo
  comienzo <- 1
  repeat{
    yini <- y[comienzo:(comienzo+longini-1)]
    cc <- hclust(dist(yini),method="complete")
    alturasFinal <- cc$height[-1]
    alturasPrincipio <- cc$height[-length(cc$height)]
    difAlturas <- alturasFinal/alturasPrincipio
    if(all(tail(difAlturas,2) <= umbralInicio)) break
    comienzo <- comienzo + 1
  }
  yini <- y[comienzo:(comienzo+longini-1)]
  cc <- hclust(dist(yini),method="complete")
  if(debug) plot(cc,hang=-1,main=paste("Dendograma inicial (inicio en ",comienzo,")"))
  numGrupos <- 1
  
  grupo <- rep(0,length(y))
  grupo[comienzo:(comienzo+longini-1)] <- 1
  
  
  ultimoAcceso <- comienzo+longini-1
  numElementos <- longini
  mu <- mean(yini)
  sigma <- min(sd(yini),maxSigma)
  
  if(debug) cat("Inicio en:",comienzo,"\n")
  
  #Registrar el uso de las primeras obs
  if(cachear){
    for(i in 1:(longini+comienzo-1)){
      acierto[i] <- registrarUso(tamLineaB*y[i],i)
    }
  }

  #Se recorre todo el vector organizando las observaciones en grupos
  for(i in (longini+comienzo):(length(y))){
    
    #Se registra el uso de la línea
    if(cachear){
      acierto[i] <- registrarUso(tamLineaB*y[i],i)
    }
    
    if(debug) cat("Examinando elemento",i,"\n")
    #Encontrar el grupo con mas probabilidad
    probs <- rep(0,numGrupos)
    for(k in 1:numGrupos){
      probs[k] <- 2*pnorm(-abs((y[i]-mu[k])/sigma[k]))
    }
    ganador <- which.max(probs)
    
    if(verbose & (i %% 5000 == 0)) cat ("Examinados ",i," elementos (",round(100*i/length(y),2),"%)\n",sep="")
    if(debug) cat("Grupo mas probable: ",ganador,"\n")
    if(debug) cat("Probabilidad de pertenencia: ",probs[ganador],"\n")
    
    #Si la probabilidad es igual o superior al minimo establecido, se aÃ±ade al grupo
    if(probs[ganador] >= tol){
      
      if(debug) cat("Se aÃ±ade al grupo ",ganador,"\n")
      #AÃ±adir al grupo
      numElementos[ganador] <- numElementos[ganador] + 1
      grupo[i] <- ganador
      ultimoAcceso[ganador] <- i
      
      #Si el grupo tiene mas de longini elementos, se actualizan los estimadores de mu y sigma
      #con un modelo de regresion robusta con los ultimos "longitudModelo" elementos
      if(numElementos[ganador] > longini){
        
        #Nos quedamos con los ultimos 500 elementos del grupo como maximo
        ymod <- tail(y[grupo==ganador],longitudModelo)
        
        #Ajustamos dos modelos: uno de regresion normal y otro de regresion robusta
        ind <- 1:length(ymod)
        m <- list()
        m[[1]] <- lm(ymod ~ ind)
        m[[2]] <- rlm(ymod~ ind, psi = psi.hampel)
        
        #Se elige el modelo con una menor estimacion de la varianza
        ss <- c(0,0)
        for(k in 1:2){
          ss[k] <- summary(m[[k]])$sigma
        }
        modelo <- which.min(ss)
        mm <- m[[modelo]]
        
        #Se estima la media y la desviacion estandar para el siguiente valor
        #La media viene dada por la prediccion del siguiente valor por el modelo
        #La desviacion estandar es la estimada por el modelo ganador
        mu[ganador] <- predict(mm,data.frame(ind = length(ymod)+1))
        sigma[ganador] <- min(max(summary(mm)$sigma,1),maxSigma)
        
        if(debug) cat("Nuema estimacion de la media: ",mu[ganador],"\n")
        if(debug) cat("Nuema estimacion de la desviaci?n est?ndar: ",sigma[ganador],"\n")
        
      } else { #En caso contrario, solo se actualiza la media
        
        #Si no hay mas de longini elementos, se actualiza la media con la media muestral
        mu[ganador] <- mean(y[grupo == ganador])
        
      }
      
      #Si se puede considerar un grupo, es decir, hay al menos un numero minimo de elementos,
      #se hace prebusqueda
      if(prebuscador & cachear & numElementos[ganador] >= umbralAdmitidos){
        
        ypreb <- ts(tail(y[grupo==ganador],umbralAdmitidos))
        mpreb <- tslm(ypreb ~ trend)
        pred.sig <- forecast(mpreb,h=1,level=c(alpha))
        low <- as.numeric(floor(pred.sig$lower))
        up <- as.numeric(ceiling(pred.sig$upper))
        for(p in low:up){
          prebuscar(tamLineaB*p,i)
        }
        
      }
      
    } else {
      
      if(debug) cat("Se crea un nuevo grupo con numero", numGrupos + 1,"\n")
      numGrupos <- numGrupos + 1
      nuevoGrupo <- numGrupos
      grupo[i] <- nuevoGrupo
      numElementos <- c(numElementos,1)
      ultimoAcceso <- c(ultimoAcceso,i)
      
      #La varianza estimada depende del grupo m?s cercano, para asegurar que no se enmascara
      #Se asegura que se diferencia lo suficiente del grupo mas cercano
      grupoMasCercano <- which.min(abs(y[i] - mu))
      nuevaSigma <- min(10*sigma[grupoMasCercano],maxSigma)
      while(2*nuevaSigma > abs(y[i]-mu[grupoMasCercano])){
        nuevaSigma <- nuevaSigma/2
      }
      nuevaSigma <- max(nuevaSigma,1)
      sigma <- c(sigma,nuevaSigma)
      
      #La media estimada es el propio valor nuevo
      mu <- c(mu,y[i])
      
      if(debug) cat("Media del nuevo grupo: ",mu[nuevoGrupo],"\n")
      if(debug) cat("Desviacion tipica del nuevo grupo: ",sigma[nuevoGrupo],"\n")
      
    }
    
    if(i %% limpiarCada == 0){
      
      if(cachear){
        if(verbose) cat("Limpiando entradas antiguas de la cache\n")
        moverEntradasViejas(i)
      }
      
      #Guardando resultados intermedios
      cat("Guardando resultados intermedios\n")
      grupoAux <- grupo[1:i]
      tablaGrupos <- table(grupoAux)
      if(any(grupoAux==0)){
        tablaGrupos <- tablaGrupos[-1]
      }
      admitidos <- which(tablaGrupos >= umbralAdmitidos)
      noAdmitidos <- setdiff(1:numGrupos,admitidos)
      S <- rep(0,length(grupoAux))
      for(i in 1:length(S)){
        if(pertenece(grupoAux[i],admitidos)){
          S[i] <- which(admitidos == grupoAux[i])
        }
      }
      
      muS <- rep(NA,length(admitidos))
      sigmaS <- rep(NA,length(admitidos))
      for(k in 1:length(admitidos)){
        g <- admitidos[k]
        muS[k] <- mu[g]
        sigmaS[k] <- sigma[g]
      }
      
      aciertoAux <- acierto[1:i]
      resultados <- list(S=S,muS=muS+miny,sigmaS=sigmaS,acierto=aciertoAux)
      
      if(cachear){
        save(resultados,cache,entradasOlvidadas,file=nombreSalida)
      } else {
        save(resultados,file=nombreSalida)
      }
      cat("Se han guardado resultados intermedios\n")
      
    }
    
    if(debug){
      cat("-----------------------------------------------\n")
      if(i > 2250) browser()
    }
  }
  
  #Se eliminan las entradas viejas de la cache
  if(cachear){
    moverEntradasViejas(length(y))
  }
  
  #Los grupos admitidos son aquellos que tienen como minimo umbralAdmitidos obs en ellos
  #Se eliminan los etiquetados como 0
  tablaGrupos <- table(grupo)
  if(any(grupo==0)){
    tablaGrupos <- tablaGrupos[-1]
  }
  admitidos <- which(tablaGrupos >= umbralAdmitidos)
  noAdmitidos <- setdiff(1:numGrupos,admitidos)
  S <- rep(0,length(grupo))
  for(i in 1:length(S)){
    if(pertenece(grupo[i],admitidos)){
      S[i] <- which(admitidos == grupo[i])
    }
  }
  
  muS <- rep(NA,length(admitidos))
  sigmaS <- rep(NA,length(admitidos))
  for(k in 1:length(admitidos)){
    g <- admitidos[k]
    muS[k] <- mu[g]
    sigmaS[k] <- sigma[g]
  }
  
  if(any(S==0)){
    proporcionReconocido <- 1-table(S)[1]/length(S)
  } else {
    proporcionReconocido <- 1
  }
  
  if(verbose) cat("Se ha reconocido el ",round(proporcionReconocido*100,6),"%\n")
  
  options(warn = oldw)
  
  resultados <- list(S=S,muS=muS+miny,sigmaS=sigmaS,acierto=acierto)
  
  if(cachear){
    save(resultados,cache,entradasOlvidadas,file=nombreSalida)
  } else {
    save(resultados,file=nombreSalida)
  }
  
  return(resultados)
  
}

#Plot de los grupos por orden de tamaño: primero los grupos mas grandes
plotGrupos <- function(y,S,predecir=T,tamMin = 0, tamMax = Inf, maxGrupos = NULL, gruposConcretos = NULL,
                       ordenInverso=F,alpha=0.1,maxFinal=100, maxGrafico=NULL){
  
  if(is.null(maxGrupos)){
    maxGrupos <- max(S)
  }
  
  y <- y/64
  o <- table(S)
  o <- o[o >= tamMin & o <= tamMax]
  o <- sort(o,decreasing=T)
  o <- as.numeric(names(o))
  o <- setdiff(o,0)
  if(ordenInverso){
    o <- rev(o)
  }
  if(!is.null(gruposConcretos)){
    o <- intersect(o,gruposConcretos)
  }
  o <- head(o,maxGrupos)
  
  ind <- 0
  for(i in o){
    
    ind <- ind+1
    cat("Calculando predicciones para el grupo ",i," (",ind," de ",length(o),")\n",sep="")
    ys <- escalaOrigen(y[S==i])
    
    if(is.null(maxGrafico)){
      maxGrafico <- length(ys)
    }
    maxGrafico <- min(maxGrafico,length(ys))
    
    if(predecir){
      low <- rep(0,maxGrafico)
      up <- rep(0,maxGrafico)
      point <- rep(0,maxGrafico)
      
      for(j in 10:(maxGrafico-1)){
        yy <- head(ys,j)
        yy <- ts(tail(yy,maxFinal))
        m <- tslm(yy ~ trend)
        pred.sig <- forecast(m,h=1,level=c(1-alpha))
        point[j+1] <- as.numeric(pred.sig$mean)
        low[j+1] <- as.numeric(pred.sig$lower)
        up[j+1] <- as.numeric(pred.sig$upper)
      }
      
      low[1:10] <- seq(ys[1],low[11],length=10)
      up[1:10] <- seq(ys[1],up[11],length=10)
    }
    
    if(predecir){
      plot(ys[1:maxGrafico],type="l",main=paste("Group ",i," (range = ",rango(y[S==i]),")",sep=""),
           ylim=c(min(low),max(up)),ylab="Memory line (scaled)",xlab="Time")
      points(low,type="l",col=2,lwd=2)
      points(up,type="l",col=2,lwd=2)
    } else {
      plot(ys[1:maxGrafico],type="l",main=paste("Grupo",i," (rango = ",rango(y[S==i]),")"))
    }
    
  }
}

prebusquedaRapida <- function(y,nombreSalida,ngroups=100,maxH=100,niter=1,eager=0.9){
  
  #Definicion de constantes
  tamLineaB <- 64
  longini <- 5
  tol <- 0.00005
  umbralInicio <- 3
  maxSigma <- 200
  alfa <- 1-eager
  limpiarCada <- 500000
  umbralAdmitidos <- min(100,maxH)
  numGuardado <- 1
  
  #Busqueda a nivel de linea, no de direccion de memoria
  y <- y/tamLineaB
  
  #Se suprimen los molestos warning en el ajuste
  oldw <- getOption("warn")
  options(warn = -1)
  
  #Inicializar la cache
  acierto <- rep(FALSE,length(y))
  
  #Inicializar la tabla de grupos y el array de información de grupo
  tablaGrupos <- matrix(nrow=ngroups,ncol=5)
  colnames(tablaGrupos) <- c("etiqueta","mu","sigma","nk","LRU")
  tablaGrupos[,"LRU"] <- rep(0,ngroups)
  
  grupo <- rep(0,length(y))
  
  #Se trabaja en una base mas pequeÃ±a para prevenir el desbordamiento
  miny <- min(y)
  y <- y - miny
  
  #Se empieza con un vector de longitud longini
  #Se va avanzando en el vector inicial y se construye un dendograma
  #hasta que la diferencia de alturas sea menor de 3 y todas las observaciones
  #se puedan considerar en el mismo grupo
  comienzo <- 1
  repeat{
    yini <- y[comienzo:(comienzo+longini-1)]
    cc <- hclust(dist(yini),method="complete")
    alturasFinal <- cc$height[-1]
    alturasPrincipio <- cc$height[-length(cc$height)]
    difAlturas <- alturasFinal/alturasPrincipio
    if(all(tail(difAlturas,2) <= umbralInicio)) break
    comienzo <- comienzo + 1
  }
  yini <- y[comienzo:(comienzo+longini-1)]
  numGrupos <- 1

  tablaGrupos[1,] <- c(numGrupos,mean(yini),min(sd(yini),maxSigma),longini,comienzo+longini-1)
  grupo[comienzo:(comienzo+longini-1)] <- 1
  
  #Registrar el uso de las primeras obs
  for(i in 1:(longini+comienzo-1)){
    acierto[i] <- registrarUso(tamLineaB*y[i],i)
  }
  
  #Se recorre todo el vector organizando las observaciones en grupos
  for(i in (longini+comienzo):(length(y))){
    
    #Se registra el uso de la línea
    acierto[i] <- registrarUso(tamLineaB*y[i],i)
    
    #Encontrar el grupo con mas probabilidad
    probs <- rep(0,ngroups)
    probs <- 2*pnorm(-abs((y[i]-tablaGrupos[,"mu"])/tablaGrupos[,"sigma"]))
    ganador <- which.max(probs)
    
    if(i %% 1000 == 0) cat ("Examinados ",i," elementos (",round(100*i/length(y),2),"%)\n",sep="")
    
    #Si la probabilidad es igual o superior al minimo establecido, se aÃ±ade al grupo
    if(probs[ganador] >= tol){
      
      #AÃ±adir al grupo
      tablaGrupos[ganador,"nk"] <- tablaGrupos[ganador,"nk"]+1
      tablaGrupos[ganador,"LRU"] <- i
      grupo[i] <- tablaGrupos[ganador,"etiqueta"]
      
      #Si el grupo tiene mas de longini elementos, se actualizan los estimadores de mu y sigma
      #con un modelo de regresion con los elementos del grupo, un maximo de maxH
      if(tablaGrupos[ganador,"nk"] > longini){
        
        #Nos quedamos con los ultimos maxH elementos del grupo como maximo
        #También eliminamos los dos primeros
        ymod <- y[grupo==tablaGrupos[ganador,"etiqueta"]]
        ymod <- ymod[-c(1,2)]
        ymod <- tail(ymod,maxH)
        
        ind <- 1:length(ymod)
        nk <- length(ymod)
        b0 <- sum((2*(2*nk+1)-6*ind)*ymod)/(nk*(nk-1))
        b1 <- sum((-6+12*ind/(nk+1))*ymod)/(nk*(nk-1))
        pred <- b0+b1*ind
        
        #Se estima la media y la desviacion estandar para el siguiente valor
        #La media viene dada por la prediccion del siguiente valor por el modelo
        #La desviacion estandar es la estimada por el modelo
        tablaGrupos[ganador,"mu"] <- b0+b1*(nk+1)
        tablaGrupos[ganador,"sigma"] <- min(maxSigma,max(sqrt(sum((ymod-pred)^2)/(nk-2)),1))
        
      } else { #En caso contrario, solo se actualiza la media
        
        #Si no hay mas de longini elementos, se actualiza la media con la media muestral
        tablaGrupos[ganador,"mu"] <- mean(y[grupo == tablaGrupos[ganador,"etiqueta"]])
        
      }
      
      #Si se puede considerar un grupo, es decir, hay al menos un numero minimo de elementos,
      #se hace prebusqueda
      if(tablaGrupos[ganador,"nk"] >= maxH){
        
        amp <- qt(1-alfa/2,nk-2)*sqrt(tablaGrupos[ganador,"sigma"]^2*(1+(2*(2*nk+1))/(nk*(nk-1))))
        low <- as.numeric(floor(tablaGrupos[ganador,"mu"]-amp))
        up <- as.numeric(ceiling(tablaGrupos[ganador,"mu"]+amp))
        
        for(p in low:up){
          prebuscar(tamLineaB*p,i)
        }
        
      }
      
    } else {
      
      numGrupos <- numGrupos + 1
      grupo[i] <- numGrupos
      
      #La varianza estimada depende del grupo m?s cercano, para asegurar que no se enmascara
      #Se asegura que se diferencia lo suficiente del grupo mas cercano
      grupoMasCercano <- which.min(abs(y[i] - tablaGrupos[,"mu"]))
      nuevaSigma <- min(10*tablaGrupos[grupoMasCercano,"sigma"],maxSigma)
      while(2*nuevaSigma > abs(y[i]-tablaGrupos[grupoMasCercano,"mu"])){
        nuevaSigma <- nuevaSigma/2
      }
      nuevaSigma <- max(nuevaSigma,1)
      
      entradaGrupo <- c(numGrupos,y[i],nuevaSigma,1,i)
      
      #Hay que buscarle un hueco, quitando el grupo más antiguo
      tablaGrupos[which.min(tablaGrupos[,"LRU"]),] <- entradaGrupo
      
      
    }
    
    if(i %% limpiarCada == 0){
      
      cat("Limpiando entradas antiguas de la cache\n")
      moverEntradasViejas(i)
      
      #Guardando resultados intermedios
      cat("Guardando resultados intermedios\n")
      grupoAux <- grupo[1:i]
      tableGrupos <- table(grupoAux)
      if(any(grupoAux==0)){
        tableGrupos <- tableGrupos[-1]
      }
      admitidos <- which(tableGrupos >= umbralAdmitidos)
      noAdmitidos <- setdiff(1:numGrupos,admitidos)
      S <- rep(0,length(grupoAux))
      for(i in 1:length(S)){
        if(pertenece(grupoAux[i],admitidos)){
          S[i] <- which(admitidos == grupoAux[i])
        }
      }
      
      aciertoAux <- acierto[1:i]
      resultados <- list(S=S,acierto=aciertoAux)
      
      save(resultados,cache,entradasOlvidadas,file=paste(nombreSalida,numGuardado,sep="-"))
      numGuardado <- numGuardado+1
      cat("Se han guardado resultados intermedios\n")
      
    }
  }
  
  #Se eliminan las entradas viejas de la cache
  moverEntradasViejas(length(y))
  
  #Los grupos admitidos son aquellos que tienen como minimo umbralAdmitidos obs en ellos
  #Se eliminan los etiquetados como 0
  tableGrupos <- table(grupo)
  if(any(grupo==0)){
    tableGrupos <- tableGrupos[-1]
  }
  admitidos <- which(tableGrupos >= umbralAdmitidos)
  noAdmitidos <- setdiff(1:numGrupos,admitidos)
  S <- rep(0,length(grupo))
  for(i in 1:length(S)){
    if(pertenece(grupo[i],admitidos)){
      S[i] <- which(admitidos == grupo[i])
    }
  }
  
  if(any(S==0)){
    proporcionReconocido <- 1-table(S)[1]/length(S)
  } else {
    proporcionReconocido <- 1
  }
  
  cat("Se ha reconocido el ",round(proporcionReconocido*100,6),"%\n")
  
  options(warn = oldw)
  
  resultados <- list(S=S,acierto=acierto)
  
  save(resultados,cache,entradasOlvidadas,file=nombreSalida)

  return(resultados)
}


escalaOrigen <- function(y){y - min(y)}

#Realiza la prebúsqueda basándose en el algoritmo G/DC
#Argumentos:
#y - representa las direcciones de memoria accedidas
#nombreSalida - nombre del fichero donde se guardan los resultados
#neit - número de entradas de la tabla de índices (IT)
#neghb - número de entradas de la tabla de historia global (GHB)
#depth - profundidad de búsqueda del algoritmo G/DC
#width - anchura de búsqueda del algoritmo G/DC
prebusquedaGDC <- function(y,nombreSalida,neit,neghb,depth,width){
  
  #Definicion de constantes
  tamLineaB <- 64
  limpiarCada <- 500000
  numGuardado <- 1
  
  #Busqueda a nivel de linea, no de direccion de memoria
  y <- y/tamLineaB
  
  #Inicializar los aciertos
  acierto <- rep(FALSE,length(y))
  
  #Se trabaja en una base mas pequeÃ±a para prevenir el desbordamiento
  miny <- min(y)
  y <- y - miny
  
  #Creación de la tabla IT
  IT <- matrix(ncol=3,nrow=neit)
  colnames(IT) <- c("Delta","Index","LRU")
  
  #Creación de la tabla GHB
  GHB <- matrix(ncol=3,nrow=neghb)
  colnames(GHB) <- c("Tag","Index","Delta")
  rownames(GHB) <- 1:neghb
  maxTamGHB <- FALSE
  neleghb <- 1
  
  #Primera dirección
  GHB[1,] <- c(y[1],NA,NA)
  acierto[1] <- registrarUso(tamLineaB*y[1],1)
  
  #Se examinan uno a uno todos los accesos a memoria
  for(i in 2:length(y)){
    
    if(i %% 1000 == 0) cat ("Examinados ",i," elementos (",round(100*i/length(y),2),"%)\n",sep="")
    
    #Registrar acierto o fallo
    acierto[i] <- registrarUso(tamLineaB*y[i],i)
    
    #Si el tamaño de la tabla es máximo, se rotan todos los elementos una posición
    #para hacer hueco al nuevo
    if(maxTamGHB){
      for(j in 1:(neghb-1)){
        GHB[j,] <- GHB[j+1,]
      }
      rownames(GHB) <- (as.numeric(rownames(GHB)[1])+1):(as.numeric(rownames(GHB)[1])+neghb)
    } else {
      neleghb <- neleghb + 1
      if(neleghb == neghb){
        maxTamGHB <- TRUE
      }
    }
    
    #Se crea la entrada en la tabla GHB para la nueva dirección
    GHB[neleghb,] <- c(y[i],NA,y[i]-y[i-1])
    
    #Buscamos si hay indicios anteriores de un Delta igual
    indIT <- which(IT[,"Delta"] == GHB[neleghb,"Delta"])
    
    #Si no los hay, creamos una entrada en la tabla IT
    if(length(indIT) == 0){
      
      #Si la tabla IT aún tiene algún hueco, se ubica ahí
      #En caso contrario, buscamos la entrada que lleva más tiempo sin usarse
      if(any(is.na(IT[,"Delta"]))){
        indIT <- which(is.na(IT[,"Delta"]))[1]
      } else {
        indIT <- which.min(IT[,"LRU"])
      }
      
      #Creamos la nueva entrada
      IT[indIT,] <- c(GHB[neleghb,"Delta"],as.numeric(rownames(GHB)[neleghb]),NA)
    }
    
    #Actualizamos los bits LRU para que sea más difícil deshacerse de esa entrada
    IT[indIT,"LRU"] <- i
    
    #En el caso de que NO sea una nueva entrada, hay que ajustar los índices
    #para mantener la estructura
    if(rownames(GHB)[neleghb] != IT[indIT,"Index"]){
      GHB[neleghb,"Index"] <- IT[indIT,"Index"] 
      IT[indIT,"Index"] <- as.numeric(rownames(GHB)[neleghb])
    }
    
    #Comienza la prebúsqueda desde la nueva entrada
    indActual <- IT[indIT,"Index"]
    indPrebus <- c()
    
    #Se repite tantas veces como la profundidad especifcada
    for(prof in 1:depth){
      
      #Si no hay indicios anteriores para ese Delta, se detiene la búsqueda
      if(is.na(GHB[format(indActual,scientific = F),"Index"])){
        break
      }
      
      #Se busca la entrada anterior con el mismo Delta. Si dicha entrada está fuera
      #de la tabla, se detiene la búsqueda
      indActual <- GHB[format(indActual,scientific = F),"Index"]
      indReal <- which(rownames(GHB) == indActual)
      if(length(indReal) == 0){
        break
      }
      
      #Se toman tantos Deltas como anchura especificada
      indPrebus <- c(indPrebus,GHB[indReal:min((indReal+width-1),neleghb),"Delta"])
    }
    
    #Se generan las direcciones de prebúsqueda
    indPrebus <- unique(indPrebus)
    dirPrebus <- y[i]+indPrebus
    
    #Prebúsqueda
    for(p in dirPrebus){
      prebuscar(tamLineaB*p,i)
    }
    
    if(i %% limpiarCada == 0){
      
      cat("Limpiando entradas antiguas de la cache\n")
      moverEntradasViejas(i)
      
      #Guardando resultados intermedios
      cat("Guardando resultados intermedios\n")
      aciertoAux <- acierto[1:i]
      resultados <- list(S=rep(0,i),acierto=aciertoAux)
      
      save(resultados,cache,entradasOlvidadas,file=paste(nombreSalida,numGuardado,sep="-"))
      numGuardado <- numGuardado + 1
      cat("Se han guardado resultados intermedios\n")
      
    }

  }
  
  cat("Limpiando entradas antiguas de la cache\n")
  moverEntradasViejas(i)
  
  #Guardando resultados intermedios
  cat("Guardando resultados finales\n")
  resultados <- list(S=rep(0,length(y)),acierto=acierto)
  
  save(resultados,cache,entradasOlvidadas,file=nombreSalida)
  cat("Se han guardado resultados finales\n")
  
  return(resultados)
  
}




#Realiza la prebúsqueda basándose en el algoritmo G/AC
#Argumentos:
#y - representa las direcciones de memoria accedidas
#nombreSalida - nombre del fichero donde se guardan los resultados
#neit - número de entradas de la tabla de índices (IT)
#neghb - número de entradas de la tabla de historia global (GHB)
#depth - profundidad de búsqueda del algoritmo G/AC
#width - anchura de búsqueda del algoritmo G/AC
prebusquedaGAC <- function(y,nombreSalida,neit,neghb,depth,width){
  
  #Definicion de constantes
  tamLineaB <- 64
  limpiarCada <- 500000
  numGuardado <- 1
  
  #Busqueda a nivel de linea, no de direccion de memoria
  y <- y/tamLineaB
  
  #Inicializar los aciertos
  acierto <- rep(FALSE,length(y))
  
  #Se trabaja en una base mas pequeÃ±a para prevenir el desbordamiento
  miny <- min(y)
  y <- y - miny
  
  #Creación de la tabla IT
  IT <- matrix(ncol=3,nrow=neit)
  colnames(IT) <- c("Tag","Index","LRU")
  
  #Creación de la tabla GHB
  GHB <- matrix(ncol=2,nrow=neghb)
  colnames(GHB) <- c("Tag","Index")
  rownames(GHB) <- 1:neghb
  maxTamGHB <- FALSE
  neleghb <- 1
  
  #Primera dirección
  GHB[1,] <- c(y[1],NA)
  IT[1,] <- c(y[1],1,1)
  acierto[1] <- registrarUso(tamLineaB*y[1],1)
  
  #Se examinan uno a uno todos los accesos a memoria
  for(i in 2:length(y)){
    
    if(i %% 1000 == 0) cat ("Examinados ",i," elementos (",round(100*i/length(y),2),"%)\n",sep="")
    
    #Registrar acierto o fallo
    acierto[i] <- registrarUso(tamLineaB*y[i],i)
    
    #Si el tamaño de la tabla es máximo, se rotan todos los elementos una posición
    #para hacer hueco al nuevo
    if(maxTamGHB){
      for(j in 1:(neghb-1)){
        GHB[j,] <- GHB[j+1,]
      }
      rownames(GHB) <- (as.numeric(rownames(GHB)[1])+1):(as.numeric(rownames(GHB)[1])+neghb)
    } else {
      neleghb <- neleghb + 1
      if(neleghb == neghb){
        maxTamGHB <- TRUE
      }
    }
    
    #Se crea la entrada en la tabla GHB para la nueva dirección
    GHB[neleghb,] <- c(y[i],NA)
    
    #Buscamos si hay indicios anteriores de un Tag igual
    indIT <- which(IT[,"Tag"] == GHB[neleghb,"Tag"])
    
    #Si no los hay, creamos una entrada en la tabla IT
    if(length(indIT) == 0){
      
      #Si la tabla IT aún tiene algún hueco, se ubica ahí
      #En caso contrario, buscamos la entrada que lleva más tiempo sin usarse
      if(any(is.na(IT[,"Tag"]))){
        indIT <- which(is.na(IT[,"Tag"]))[1]
      } else {
        indIT <- which.min(IT[,"LRU"])
      }
      
      #Creamos la nueva entrada
      IT[indIT,] <- c(GHB[neleghb,"Tag"],as.numeric(rownames(GHB)[neleghb]),NA)
    }
    
    #Actualizamos los bits LRU para que sea más difícil deshacerse de esa entrada
    IT[indIT,"LRU"] <- i
    
    #En el caso de que NO sea una nueva entrada, hay que ajustar los índices
    #para mantener la estructura
    if(rownames(GHB)[neleghb] != IT[indIT,"Index"]){
      GHB[neleghb,"Index"] <- IT[indIT,"Index"] 
      IT[indIT,"Index"] <- as.numeric(rownames(GHB)[neleghb])
    }
    
    #Comienza la prebúsqueda desde la nueva entrada
    indActual <- IT[indIT,"Index"]
    indPrebus <- c()
    
    #Se repite tantas veces como la profundidad especifcada
    for(prof in 1:depth){
      
      #Si no hay indicios anteriores para ese Delta, se detiene la búsqueda
      if(is.na(GHB[format(indActual,scientific = F),"Index"])){
        break
      }
      
      #Se busca la entrada anterior con el mismo Tag Si dicha entrada está fuera
      #de la tabla, se detiene la búsqueda
      indActual <- GHB[format(indActual,scientific = F),"Index"]
      indReal <- which(rownames(GHB) == indActual)
      if(length(indReal) == 0){
        break
      }
      
      #Se toman tantos Tag como anchura especificada
      indPrebus <- c(indPrebus,GHB[indReal:min((indReal+width-1),neleghb),"Tag"])
    }
    
    #Se generan las direcciones de prebúsqueda
    dirPrebus <- setdiff(unique(indPrebus),y[i])
    
    #Prebúsqueda
    for(p in dirPrebus){
      prebuscar(tamLineaB*p,i)
    }
    
    if(i %% limpiarCada == 0){
      
      cat("Limpiando entradas antiguas de la cache\n")
      moverEntradasViejas(i)
      
      #Guardando resultados intermedios
      cat("Guardando resultados intermedios\n")
      aciertoAux <- acierto[1:i]
      resultados <- list(S=rep(0,i),acierto=aciertoAux)
      
      save(resultados,cache,entradasOlvidadas,file=paste(nombreSalida,numGuardado,sep="-"))
      numGuardado <- numGuardado + 1
      cat("Se han guardado resultados intermedios\n")
      
    }
    
  }
  
  cat("Limpiando entradas antiguas de la cache\n")
  moverEntradasViejas(i)
  
  #Guardando resultados intermedios
  cat("Guardando resultados finales\n")
  resultados <- list(S=rep(0,length(y)),acierto=acierto)
  
  save(resultados,cache,entradasOlvidadas,file=nombreSalida)
  cat("Se han guardado resultados finales\n")
  
  return(resultados)
  
}

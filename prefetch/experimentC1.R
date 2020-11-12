
nombres <- c("astar","bzip2","Firefox","g++","gcc","gobmk","h264ref",
             "hmmer","lbm","libquantum","mcf","milc","namd","omnetpp",
             "Openoffice","povray","sjeng","specrand","sphinx3","Xalan")
longitudAnalisis <- c(131143,298556,3500000,4e+06,4e+06,445214,322393,472321,5e+06,4e+06,5e+06,150745,1112199,4e+06,4365853,
                      60452,5e+06,3508,634724,2500000)

library(doParallel)
ncores <- min(detectCores()-1,length(nombres))
cl <- makeCluster(ncores,type="FORK")
registerDoParallel(cl)


# EXPERIMENTO C1
# NGROUPS = 10
# MAXH = 100
# NITER = 1
# EAGER = 0.9
# LLC 32/8
# SDRAM 16/4

borrar <- foreach(i = 1:length(nombres),.export = c("nombres","longitudAnalisis")) %dopar% {
  
  nombre <- nombres[i]
  sink(paste("outputC1_",nombre,".out",sep=""))
  source("funcionesPrebusqueda.R",encoding="UTF8")
  nombreFichero <- paste(nombre,"_Ass8Tam32.prebus",sep="")
  longitud <- longitudAnalisis[i]
  data <- leerOrdenar(nombreFichero,longitud)
  
  nombreSalida <- paste("resultadosC1/resultadosC1_",nombre,".RData",sep="")
  inicializarCache(2**24,asociatividad = 4)
  resultados <- prebusquedaRapida(data$tag,nombreSalida,ngroups = 10,maxH = 100,eager = 0.9,niter = 1)
  sink()
}
rm(borrar)


stopCluster(cl)
registerDoSEQ()
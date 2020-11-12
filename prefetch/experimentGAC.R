
nombres <- c("astar","bzip2","Firefox","g++","gcc","gobmk","h264ref",
             "hmmer","lbm","libquantum","mcf","milc","namd","omnetpp",
             "Openoffice","povray","sjeng","specrand","sphinx3","Xalan")
longitudAnalisis <- c(131143,298556,3500000,4e+06,4e+06,445214,322393,472321,5e+06,4e+06,5e+06,150745,1112199,4e+06,4365853,
                      60452,5e+06,3508,634724,2500000)

library(doParallel)
ncores <- min(detectCores()-1,length(nombres))
cl <- makeCluster(ncores,type="FORK")
registerDoParallel(cl)


# EXPERIMENTO G/AC 4/4
# WIDTH 4
# DEPTH 4
# NEIT 512
# NEGHB 512

borrar <- foreach(i = 1:length(nombres),.export = c("nombres","longitudAnalisis")) %dopar% {
  
  nombre <- nombres[i]
  sink(paste("outputGAC_",nombre,".out",sep=""))
  source("funcionesPrebusqueda.R",encoding="UTF8")
  nombreFichero <- paste(nombre,"_Ass8Tam32.prebus",sep="")
  longitud <- longitudAnalisis[i]
  data <- leerOrdenar(nombreFichero,longitud)
  
  nombreSalida <- paste("resultadosGAC/resultadosGAC_",nombre,".RData",sep="")
  inicializarCache(2**24,asociatividad = 4)
  resultados <- prebusquedaGAC(data$tag,nombreSalida,neit=512,neghb=512,depth=4,width=4)
  sink()
}
rm(borrar)


stopCluster(cl)
registerDoSEQ()


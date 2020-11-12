#!/usr/bin/python

#Este script toma como entraza el ficher de traza nvt y lo convierte a un
#fichero con informacion sobre el OPKC (operaciones por kilociclo).
#Se cuentan las operaciones que se han producido cada un cierto intervalo
#para calcular el OPKC, y se escriben en el fichero de salida como si
#fuera una serie temporal.

import sys
import os
import shutil
from optparse import OptionParser
from blist import blist
import re
import common



#Cada cuanto tiempo se mide el OPKC: tamIntervalo
tamIntervalo = 1000


def escribir(texto):
    salidaS.write(texto)
    salidaS.write("\n")


def calcularOPKC():
  
  numOp = 0
  ciclos = 0
  numIntervalo = 1

  line = tFile.readline()
  while line:
      cols = line.split(" ")
      ciclos = int(cols[0])
      while(ciclos > numIntervalo*tamIntervalo):
          opkci = float(numOp)*1000.0/float(tamIntervalo)
          escribir(str(opkci))
          numIntervalo+=1
          numOp=0

          if(numIntervalo%1000000 == 0):
              print("Procesados " + str(numIntervalo*tamIntervalo) + " ciclos")
      numOp += 1
      line = tFile.readline()








parser = OptionParser()
parser.add_option("--ficheroTraza",action="store",dest="traceF",default="NONE",
            help="name of trace file")

parser.add_option("-s","--ficheroSalida",action="store",dest="salidaF",default="NONE",
            help="name of temporal stat file")


(options,args) = parser.parse_args(sys.argv[1:])

salidaFile = options.salidaF
traceFile = options.traceF



print ("Fichero Traza:" , traceFile)
print ("Fichero Salida:" , salidaFile)
if traceFile=="NONE" or salidaFile=="NONE":
    print ("nvt2statS.py --ficheroTraza=FicheroTraza --ficheroSalida=FicheroSalidaStatS")
    sys.exit(0)

if not os.path.isfile(traceFile):
    print ("File ", traceFile, " no existe")
    sys.exit(0)

tFile = file(traceFile, "r")
salidaS = file(salidaFile, "w")

escribir("tamIntervalo: " + str(tamIntervalo))
calcularOPKC()

tFile.close()
salidaS.close()

sys.exit(0)





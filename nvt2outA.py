#!/usr/bin/python

import sys
import os
import shutil
from optparse import OptionParser
from blist import blist
import re
import common


PERIODOCREAFICHERO=20000000

#Elabora el resumen en formato out PERO eliminando los reemplazos,
#es decir, no se considera como un acceso a memoria

#
# Funciones de analisis
#
def InsertaTag(ciclos, tag, tipo):
    if tag in bagOfLines:
        bagOfLines[tag][ciclos]=tipo
    else:
        bagOfLines[tag] = {}
        bagOfLines[tag][ciclos]=tipo

def RecorreTag(fichero):
    outputFile = file(fichero,"w")
    print ("RECORRE TAG")
    for tag, entradas in bagOfLines.items():
        cnt=0
        string= tag+" "   
        for listaCiclos in sorted(entradas.items()):
#            print "listaCiclos: ", listaCiclos
            ciclos,tipo=listaCiclos
            string=string+ciclos+" "+tipo+" "
        string=string+"\n"
#        print string
        outputFile.write(string)

    outputFile.close()

#
#
# Leo fichero e integro en bagOfLines
# Rapidisimo pero ocupa mucha memoria
def LeeFichero():
  with open(traceFile) as fp:
    cnt = 0
    for line in fp:
#        print ("line {} contents {}".format(cnt,line))
        cols = line.split(" ")
        ciclos = cols[0]
        tag = cols[2]
        tipo = cols[1]
#        print "Linea:", cnt, " Ciclos:", ciclos, " Tag:", tag, " Tipo:", tipo

        InsertaTag(ciclos, tag, tipo)
        cnt+=1



def RecorreTagAhorroMem(fichero):
    outputFile = file(fichero,"w")
    print ("RECORRE TAG AHORRO MEM")
    for tag,entradas in vectorTags:
        string= tag+" "   
        for cnt,tipo in entradas:
          string=string+str(cnt)+" "+tipo+" "
        string=string+"\n"
#        print string
        outputFile.write(string)

    outputFile.close()







def LeeFicheroAhorroMem3():
  print ("LEE FICHERO CON AHORR MEMORIA 3")

  fp=open(traceFile, "r")
  cnt = 0
  numAccesos = 0
  numFiles=0
  line=fp.readline()
  while line:
#        print ("line {} contents {}".format(cnt,line))
        cols = line.split(" ")
        ciclos = cols[0]
        tag = cols[2]
        tipo = cols[1]
        cnt+=1
        #Solo cuento lo que no son reemplazos
        if (tipo != "RE"):
          numAccesos += 1
  #        print "Linea:", cnt, " Ciclos:", ciclos, " Tag:", tag, " Tipo:", tipo
          fin=0
          # busca tag en la lista
          if tag in bagOfLines:
            entrada=bagOfLines[tag]
            tagT,datosT = vectorTags[entrada]
            datosT.append([numAccesos,tipo])
          else:
            entrada=len(vectorTags)
            bagOfLines[tag] = entrada
            datos=[]
            datos.append([numAccesos,tipo])
            vectorTags.append([tag,datos])


        if cnt%10000==0:
            print ("Lineas tratadas: ", cnt)
        if cnt%PERIODOCREAFICHERO==0:
            fichero=salidaFile+"-"+str(numFiles)
            print "Crea fichero", fichero
            numFiles+=1
            RecorreTagAhorroMem(fichero)

        line=fp.readline()

  fp.close()






parser = OptionParser()
parser.add_option("--ficheroTraza",action="store",dest="traceF",default="NONE",
            help="name of trace file")

parser.add_option("-s","--ficheroSalida",action="store",dest="salidaF",default="NONE",
            help="name of folder in which to generate experiment config")


(options,args) = parser.parse_args(sys.argv[1:])

salidaFile = options.salidaF
traceFile = options.traceF



print ("Fichero Traza:" , traceFile)
print ("Fichero Salida:" , salidaFile)
if traceFile=="NONE" or salidaFile=="NONE":
    print ("nvt2out.py --ficheroTraza=FicheroTraza --ficheroSalida=FicheroSalida")
    sys.exit(0)

if not os.path.isfile(traceFile):
    print ("File ", traceFile, " no existe")
    sys.exit(0)

tFile = file(traceFile, "r")

# Diccionario
bagOfLines={}

# Lista (ahorro memoria)
#vectorTags=[]

# Uso blist
vectorTags=blist([])

#Version con Diccionario y mucho consumo de memoria (poco tiempo)
#LeeFichero()
#RecorreTag(salidaFile)

#Version con listas y poco consumo de memoria (mucho tiempo). Muchisimo tiempo.
#LeeFicheroAhorroMem()
#RecorreTagAhorroMem(salidaFile)

#Version con listas y poco consumo de memoria. Uso blist. Rapido pero necesita mucha memoria
LeeFicheroAhorroMem3()
RecorreTagAhorroMem(salidaFile)

sys.exit(0)





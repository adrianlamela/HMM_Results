#!/usr/bin/python

import sys
import os
import shutil
from optparse import OptionParser
from blist import blist
import re
import common

#En todo el fichero se supone que un acceso a memoria supone acceder a un bloque de 64B
tamLinea = 64

#Numero de lineas de memoria adyacentes que caen dentro de la ventana espacial
ventanaEspacial = 100 

#Maxima distancia temporal para considerar el acceso dentro de la ventana temporal
ventanaTemporal = 1000

#
# Funciones de analisis
#


#
#
# Leo fichero e integro en bagOfLines
# Rapidisimo pero ocupa mucha memoria
# Elimina los Reemplazos: solo se tiene en cuenta R o W
def LeeFichero():
  print "Leyendo fichero ", datFile
  with open(datFile) as fp:
    cnt = 0
    for line in fp:
#        print ("line {} contents {}".format(cnt,line))
        cols = line.split(" ")
        tam = len(cols)-2  # uno lo quitamos por el barra-n y otro por tag
        tag = cols[0]
        lista=[]
        for i in range(1,tam,2):
          acceso = cols[i]
          tipo = cols[i+1]
          lista.append([acceso,tipo])
#          print " ciclos:", ciclos, " tipo:", tipo
        EliminaRE(lista)
        bagOfLines.append([tag,lista])
        cnt+=1

#
# Toma una lista y elimina todos los elementos RE
def EliminaRE(lista):
    for ciclos,op in lista:
        if op=="RE":
            lista.remove([ciclos,op])

def buscaRE(lista):
    cnt=0;
    for ciclos,op in lista:
        if op=="RE":
            cnt=cnt+1
    return cnt

#Traduce el fichero outA para mostrar informacion dedicada a la prebusqueda
#Para cada acceso a memoria, se almacenan las siguientes cosas:
# - Instante temporal
# - tag
# - Numero de acceso a esa linea
# - Tipo de acceso
def generarDatosPrebusqueda():
	print "Generando los datos de la localidad espacial"
	escribir = "t tag numAcceso tipo"
	salidaS.write(escribir)
	salidaS.write("\n")
	cnt = 0
	for i in range(0,len(bagOfLines)):
		tag,lista = bagOfLines[i]
		cnt+=1
		if cnt%1000000==0:
			print("Cnt: " + str(cnt))
		numEntradas = len(lista)
		for j in range (0,numEntradas):
			acceso,tipo = lista[j]
			escribir = str(acceso) + " " + str(int(tag,0)) + str(" ") + str(j+1) + " " + str(tipo)
			salidaS.write(escribir)
			salidaS.write("\n")



parser = OptionParser()
parser.add_option("--ficheroSoutA",action="store",dest="fichO",default="NONE",
            help="name of outA file")

parser.add_option("-s","--ficheroPrebus",action="store",dest="salidaS",default="NONE",
            help="name of folder in which to generate loctmp result file")


(options,args) = parser.parse_args(sys.argv[1:])

salidaStat = options.salidaS
datFile = options.fichO



if salidaStat=="NONE" or datFile=="NONE":
    print ("soutA2locspa.py --ficheroSoutA=FicheroOutA.soutA --ficheroPrebus=FicheroLocTmp.locspa")
    sys.exit(0)

if not os.path.isfile(datFile):
    print ("File ", datFile, " no existe")
    sys.exit(0)

print ("Fichero datos soutA:" , datFile)
print ("Fichero Salida:" , salidaStat)

salidaS = file(salidaStat, "w")

# Diccionario
bagOfLines=[]


#Version con Diccionario y mucho consumo de memoria (poco tiempo)

LeeFichero()
generarDatosPrebusqueda()



sys.exit(0)





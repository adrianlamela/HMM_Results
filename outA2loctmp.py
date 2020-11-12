#!/usr/bin/python

import sys
import os
import shutil
from optparse import OptionParser
from blist import blist
import re
import common



#
# Funciones de analisis
#


#
#
# Leo fichero e integro en bagOfLines
# Rapidisimo pero ocupa mucha memoria
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

#Traduce el fichero outA para mostrar informacion sobre la localidad temporal
#Se mide en numero de accesos a memoria (R o W, no RE) que ocurren entre dos accesos
#a una misma linea
def generarDistribucionLocalidadTemporal():
	print "Generando los datos de la localidad temporal"
	escribir = "linea accesoAntes accesoDespues distTemp opAntes opDespues"
	salidaS.write(escribir)
	salidaS.write("\n")
	cnt = 0
	for i in range(0,len(bagOfLines)):
		tag,lista = bagOfLines[i]
		EliminaRE(lista)
		cnt+=1
		if cnt%1000000==0:
			print("Cnt: " + str(cnt))
		numEntradas = len(lista)
		if (numEntradas > 1):
			for j in range(1,numEntradas):
				acceso2,tipo2 = lista[j]
				acceso1,tipo1 = lista[j-1]
				dif = long(acceso2) - long(acceso1)
				escribir = str(tag) + " " + str(acceso1) + " " + str(acceso2) + " " + str(dif) + " " + str(tipo1) + " " + str(tipo2)
				salidaS.write(escribir)
				salidaS.write("\n")




parser = OptionParser()
parser.add_option("--ficheroOutA",action="store",dest="fichO",default="NONE",
            help="name of outA file")

parser.add_option("-s","--ficheroLocTmp",action="store",dest="salidaS",default="NONE",
            help="name of folder in which to generate loctmp result file")


(options,args) = parser.parse_args(sys.argv[1:])

salidaStat = options.salidaS
datFile = options.fichO



if salidaStat=="NONE" or datFile=="NONE":
    print ("nvt2out.py --ficheroOutA=FicheroOutA.outA --ficheroLocTmp=FicheroLocTmp.loctmp")
    sys.exit(0)

if not os.path.isfile(datFile):
    print ("File ", datFile, " no existe")
    sys.exit(0)

print ("Fichero datos outA:" , datFile)
print ("Fichero Salida:" , salidaStat)

salidaS = file(salidaStat, "w")

# Diccionario
bagOfLines=[]


#Version con Diccionario y mucho consumo de memoria (poco tiempo)

LeeFichero()
generarDistribucionLocalidadTemporal()



sys.exit(0)





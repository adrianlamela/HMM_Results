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

#Traduce el fichero outA para mostrar informacion sobre la localidad espacial
#Para cada acceso a memoria, se almacenan las siguientes cosas:
# - tag
# - Numero de acceso a esa linea
# - Numero de lineas accedidas al menos una vez dentro en la zona anterior de la ventana espacial y dentro de la temporal
# - Numero de lineas accedidas al menos una vez dentro en la zona posterior de la ventana espacial y dentro de la temporal
# - Numero de lineas accedidas al menos una vez dentro de la ventana espacial y de la temporal
def generarDatosLocalidadEspacial():
	print "Generando los datos de la localidad espacial"
	escribir = "t tag numAcceso lineasEncimaAntes lineasDebajoAntes lineasEncimaDespues lineasDebajoDespues"
	salidaS.write(escribir)
	salidaS.write("\n")
	cnt = 0
	for i in range(0,len(bagOfLines)):
		tag,lista = bagOfLines[i]
		cnt+=1
		if cnt%100000==0:
			print("Cnt: " + str(cnt)+"\n")
		numEntradas = len(lista)
		for j in range (0,numEntradas):
			acceso,tipo = lista[j]
			xant = 0
			xpos = 0
			yant = 0
			ypos = 0
			for k in range(i-ventanaEspacial,i+ventanaEspacial+1):
				if(k >= 0 and k != i and k <= len(bagOfLines)-1):
					tagS, listaS = bagOfLines[k]
					diferenciaEspacial = abs(int(tag,0)-int(tagS,0))
					#Comprobacion ventana espacial
					if(diferenciaEspacial <= ventanaEspacial*tamLinea):
						for l in range(0,len(listaS)):
							accesoS,tipoS = listaS[l]
							diferenciaTemporal = int(accesoS)-int(acceso)
							#Comprobacion ventana temporal
							if(diferenciaTemporal <= ventanaTemporal and diferenciaTemporal > 0):
								if(int(tagS,0) - int(tag,0) >= 0):
									ypos += 1
								else:
									yant += 1
								break
						for l in range(0,len(listaS)):
							accesoS,tipoS = listaS[l]
							diferenciaTemporal = int(acceso)-int(accesoS) #Cambio en el orden
							#Comprobacion ventana temporal
							if(diferenciaTemporal <= ventanaTemporal and diferenciaTemporal > 0):
								if(int(tagS,0) - int(tag,0) >= 0):
									xpos += 1
								else:
									xant += 1
								break
			escribir = str(tag) + " " + str(j+1) + str(" ") + str(xant) + " " + str(xpos) + " " +  str(yant) + " " + str(ypos) + " " + str(yant+ypos)
			salidaS.write(escribir)
			salidaS.write("\n")


#Version mas rapida de la funcion anterior
# - Numero de acceso (instante temporal)
# - tag
# - Numero de acceso a esa linea
# - Numero de lineas accedidas al menos una vez con anterioridad dentro en la zona anterior de la ventana espacial y dentro de la temporal
# - Numero de lineas accedidas al menos una vez con anterioridad dentro en la zona posterior de la ventana espacial y dentro de la temporal
# - Numero de lineas accedidas al menos una vez dentro en la zona anterior de la ventana espacial y dentro de la temporal
# - Numero de lineas accedidas al menos una vez dentro en la zona posterior de la ventana espacial y dentro de la temporal
def generarDatosLocalidadEspacial2():
	print "Generando los datos de la localidad espacial"
	escribir = "t tag numAcceso lineasEncimaAntes lineasDebajoAntes lineasEncimaDespues lineasDebajoDespues"
	salidaS.write(escribir)
	salidaS.write("\n")
	cnt = 0

	#Para todas las lineas de memoria de las que se tienen datos:
	for i in range(0,len(bagOfLines)):
		tag,lista = bagOfLines[i]
		cnt+=1
		if cnt%100000==0:
			print("Cnt: " + str(cnt))

		#Determinar que lineas caen en la ventana espacial
		dentroVentanaEspacial = []
		for k in range(i-ventanaEspacial,i+ventanaEspacial+1):
			if(k >= 0 and k != i and k <= len(bagOfLines)-1):
				tagS, listaS = bagOfLines[k]
				diferenciaEspacial = abs(int(tag,0)-int(tagS,0))
				#Comprobacion ventana espacial
				if(diferenciaEspacial <= ventanaEspacial*tamLinea):
					dentroVentanaEspacial.append(k)

		#Creamos un vector para almacenar el indice de la busqueda en cada linea
		indiceBusqueda = [0]*len(dentroVentanaEspacial)

		#Hay que determinar el numero de lineas accedidas para cada uno de los accesos a la linea i
		numEntradas = len(lista)
		for j in range (0,numEntradas):

			acceso,tipo = lista[j]
			xant = 0
			xpos = 0
			yant = 0
			ypos = 0
			for k in range(0,len(dentroVentanaEspacial)):
				ind = dentroVentanaEspacial[k]
				tagS, listaS = bagOfLines[ind]
				l = indiceBusqueda[k]
				diferenciaTemporal = 0
				while(l < len(listaS)):
					accesoS, tipoS = listaS[l]
					diferenciaTemporal = int(accesoS)-int(acceso)
					if(diferenciaTemporal > 0):
						break
					else:
						l += 1
				indiceBusqueda[k] = l
				accesoA, tipoA = listaS[l-1]
				anteriorAcceso = int(acceso) - int(accesoA)
				if(anteriorAcceso > 0 and anteriorAcceso <= ventanaTemporal):
					if(int(tagS,0) - int(tag,0) >= 0):
						xpos += 1
					else:
						xant += 1
				if(diferenciaTemporal > 0 and diferenciaTemporal <= ventanaTemporal):
					if(int(tagS,0) - int(tag,0) >= 0):
						ypos += 1
					else:
						yant += 1

			escribir = str(acceso) + " " + str(tag) + " " + str(j+1) + str(" ") + str(xant) + " " + str(xpos) + " " +str(yant) + " " + str(ypos)
			salidaS.write(escribir)
			salidaS.write("\n")



parser = OptionParser()
parser.add_option("--ficheroSoutA",action="store",dest="fichO",default="NONE",
            help="name of outA file")

parser.add_option("-s","--ficheroLocSpa",action="store",dest="salidaS",default="NONE",
            help="name of folder in which to generate loctmp result file")


(options,args) = parser.parse_args(sys.argv[1:])

salidaStat = options.salidaS
datFile = options.fichO



if salidaStat=="NONE" or datFile=="NONE":
    print ("soutA2locspa.py --ficheroSoutA=FicheroOutA.soutA --ficheroLocSpa=FicheroLocTmp.locspa")
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
generarDatosLocalidadEspacial2()



sys.exit(0)





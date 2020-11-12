import sys
import glob

reportRoot = "/home/benja/LAB/RRAM/"


attrNames = {}

# Trace report entries
attrNames["executionTime"] = "System:executionTime"
attrNames["memoryRead"] = "P(0)_SysBus:memoryRead"
attrNames["RemoteDirtyRd"] = "P(0)_SysBus:remoteCleanRead"
attrNames["RemoteCleanRd"] = "P(0)_SysBus:remoteDirtyRead"
attrNames["squash"] = "System:squashedCycles("
attrNames["commit"] = "System:commitCycles("
attrNames["LargeMsg"] = "System:largeMsg"
attrNames["SmallMsg"] = "System:smallMsg"
attrNames["CommittedChunks"] = "System:committedChunks"
attrNames["CommitCycles"] = "System:oneTimeCommitCycles:v"
attrNames["PreSuccessOne"] = "System:preemptOneSucc"
attrNames["PreSuccessMore"] = "System:preemptMoreSucc"
attrNames["PreFailOne"] = "System:preemptOneFail"
attrNames["PreFailMore"] = "System:preemptMoreFail"




# Benchmarks
benchAppPrettyName = {}
benchAppPrettyName["Firefox"] = "Firefox"
benchAppPrettyName["g++"] = "g++"
benchAppPrettyName["Openoffice"] = "Openoffice"




# list of parameter name for nvmain based on the list of experiments ConfList
ConfListName={}
ConfListName["DRC"]="MEM_CTL"
ConfListName["DRC"]="MEM_CTL"
ConfListName["FCFS"]="MEM_CTL"
ConfListName["FRFCFS"]="MEM_CTL"
ConfListName["FRFCFS-WQF"]="MEM_CTL"
ConfListName["PerfectMemory"]="MEM_CTL"

#list of experiments
ConfListCache={}
ConfListCache["DRC"] = "DRC"

ConfListRRAM={}
ConfListRRAM["FCFS"] = "FCFS"

BaseExps={}
BaseExps["RRAM"]="RRAM"
BaseExps["DRAMCache"]="DRAMCache"

BANK=[4]
CLK=[800,2000]
BusWidth=[64,128]
DevideWidth=[8,128]
CHANNELS=[1,2,4]
DRCCHANNELS=[1,2,4]
Assoc=[1,2,7]





# Just get the name of the file
def getReportAppPath(benchName,baseE,memCtl,Aso,DRCCh,RRCh):
  if baseE=="RRAM":
    fileList = reportRoot + benchName + "/"+ benchName+"_"+baseE+"_"+memCtl+".res"
  else:
    fileList = reportRoot + benchName + "/"+ benchName+"_"+baseE+"_"+memCtl+"_Assoc"+str(Aso)+"_DRCCh"+str(DRCCh)+"_RRCh"+str(RRCh)+".res"

  return fileList



def getAttributeApp(benchName,baseE,memCtl,Aso,DRCCh,RRCh,attrType):
  reportPath = getReportAppPath(benchName,baseE,memCtl,Aso,DRCCh,RRCh)
  if reportPath == "":
    return 0
#  print "fichero %s" %(reportPath)
#  attrName = attrNames[attrType];
  attrName = attrType;
  return getReportAttribute(reportPath, attrName)


def getReportAttribute(filePath, attrName, isAvg=False):
  try:
    f = open(filePath,"r")
  except IOError:
    return 0

  while True:
    l = f.readline()
    if l.find(attrName) != -1:
      if isAvg:
        cols = l.split(":")
        l = cols[1]
        cols1 = l.split("=")
        value1 = cols1[1]
        l = cols[2]
      else:
        value1 = "1"
      cols = l.split(" ")
      value = cols[1].replace("\n","")
      value = cols[1].replace("W","")
      f.close
      if '.' in value:
        return float(value)*float(value1)
      else:
        if '.' in value1:
          return float(value)*float(value1)
        else:
          if "nan" in value1 or "nan" in value:
            return 0
          else:
            return int(value)*int(value1)
    if len(l)==0:
      break
  f.close
  return 0























def getAttributeAppDirect(benchName,proc,Mem,storeLat,WB,ACT,randLat,attrType):
  reportPath = getReportAppPath(benchName,proc,Mem,storeLat,WB,ACT,randLat)
#  print "file %s" %(reportPath)
  if reportPath == "":
    return 0
  return getReportAttribute(reportPath, attrType)


def getReportAppPath2(benchName,proc,Mem,storeLat,WB,ACT,randLat,FileRoot):
  fileList = FileRoot + "/sesc_" + str(proc) + "proc-Mem" + str(Mem) + "-Store" + str(storeLat) + "-WB"+str(WB)+"-ACT"+str(ACT)+"-RandLat"+randLat+"_"+benchName+ ".res"
  return fileList


def getAttributeApp2(benchName,proc,Mem,storeLat,WB,ACT,randLat,attrType,FileRoot):
  reportPath = getReportAppPath2(benchName,proc,Mem,storeLat,WB,ACT,randLat,FileRoot)
  if reportPath == "":
    return 0
  print "file %s" %(reportPath)
  attrName = attrNames[attrType];
  return getReportAttribute(reportPath, attrName)

def getAttributeAppDirect2(benchName,proc,Mem,storeLat,WB,ACT,randLat,attrType,FileRoot):
  reportPath = getReportAppPath2(benchName,proc,Mem,storeLat,WB,ACT,randLat,FileRoot)
  if reportPath == "":
    return 0
  return getReportAttribute(reportPath, attrType)





def getAttributeSpDirect(group, benchName, attrType):
  reportPath = getReportSpPath(group, benchName)
  if reportPath == "":
    return 0
  return getReportAttribute(reportPath, attrType)


def getReportSpPath(group, benchName):
  fileList = reportRoot + "/" + group + "-" + benchName + ".res"
  return fileList


def getAttributeSp(group, benchName, attrType):
  reportPath = getReportSpPath(group, benchName)
  if reportPath == "":
    return 0
  attrName = attrNames[attrType];
  return getReportAttribute(reportPath, attrName)








def getValueStatsOneTimeSp(groupName, benchName, attrType):
  reportPath = getReportSpPath(groupName, benchName)
  if reportPath == "":
    return 0
#  print "File %s" % (reportPath)
  attrName = attrNames[attrType];
  return getValueStats(reportPath, attrName, True)

def getValueStatsOneTimeApp(benchName, numP, lat, attrType):
  reportPath = getReportAppPath(benchName, numP, lat)
  if reportPath == "":
    return 0
#  print "File %s" % (reportPath)
  attrName = attrNames[attrType];
  return getValueStats(reportPath, attrName, True)

def getValueStats(filePath, attrName, isAvg=False):
  try:
    f = open(filePath,"r")
  except IOError:
    return 0

  valor = 0.0
  while True:
    l = f.readline()
    if l.find(attrName) != -1:
      if isAvg:
        cols = l.split(":")
        l = cols[3]
        cols1 = l.split("=")
        value1 = cols1[1]
#        print "%s" % (value1)
        return value1
        l = cols[2]
      else:
        value1 = "1"
      cols = l.split("=")
      value = cols[1].replace("\n","")
      valor = valor + float(value)*float(value1)
      return value1
#      print "valueA= %f valueB= %f\n" % (float(value), float(value1))
    if len(l)==0:
      break
  f.close
  return valor









# Benja
def getReportAttributeAllTimes(filePath, attrName, isAvg=False):
  try:
    f = open(filePath,"r")
  except IOError:
    return 0

  valor = 0.0
  l = f.readline()
  while True:
    l = f.readline()
    if l.find("#BEGIN") != -1:  # redundant
      break
    if l.find(attrName) != -1:
      if isAvg:
        cols = l.split(":")
        l = cols[1]
        cols1 = l.split("=")
        value1 = cols1[1]
        l = cols[2]
      else:
        value1 = "1"
      cols = l.split("=")
      value = cols[1].replace("\n","")
      valor = valor + float(value)*float(value1)
#      print "valueA= %f valueB= %f\n" % (float(value), float(value1))
    if len(l)==0:
      break
  f.close
  return valor


def getReportPath(benchName, exp, chunkSize, numP, dirSize):
  fileList = reportRoot + "/report-" + str(chunkSize) + "Sz-" + dirSize +"/" + exp + "_report." + benchName + "." + str(numP) 
  return fileList

def getAttributeAllTimes(benchName, exp, chunkSize, numP, dirSize, attrType):
  reportPath = getReportPath(benchName, exp, chunkSize, numP, dirSize)
  if reportPath == "":
    return 0
#  print "File %s" % (reportPath)
  attrName = attrNames[attrType];
  return getReportAttributeAllTimes(reportPath, attrName)

def getAttribute(benchName, exp, chunkSize, numP, dirSize, attrType):
  reportPath = getReportPath(benchName, exp, chunkSize, numP, dirSize)
  if reportPath == "":
    return 0
  attrName = attrNames[attrType];
  return getReportAttribute(reportPath, attrName)







# Benja
def getNumberStats(filePath, attrName, isAvg=False):
  try:
    f = open(filePath,"r")
  except IOError:
    return 0

  valor = 0.0
  while True:
    l = f.readline()
    if l.find(attrName) != -1:
      if isAvg:
        cols = l.split(":")
        l = cols[3]
        cols1 = l.split("=")
        value1 = cols1[1]
#        print "VALUE1: %s" % (value1)
        return value1


        l = cols[2]
      else:
        value1 = "1"
      cols = l.split("=")
      value = cols[1].replace("\n","")
      valor = valor + float(value)*float(value1)
      return value
#      print "valueA= %f valueB= %f\n" % (float(value), float(value1))
    if len(l)==0:
      break
  f.close
  return valor



def getAttributeAllTimesLE(benchName, exp, chunkSize, attrType):
  reportPath = reportRoot + "/sesc_" + exp + "_" + benchName  
  if reportPath == "":
    return 0
#  print "File %s" % (reportPath)
  attrName = attrNames[attrType];
  return getReportAttributeAllTimes(reportPath, attrName)



def getAttributeFrame(benchName, exp, chunkSize, frameSize, attrType):
  reportPath = getReportPathFrame(benchName, exp, chunkSize, frameSize)
  if reportPath == "":
    return 0
#  print "File %s" % (reportPath)
  attrName = attrNames[attrType];
  return getReportAttribute(reportPath, attrName)

def getAttributeAllTimesFrame(benchName, exp, chunkSize, frameSize, attrType):
  reportPath = getReportPathFrame(benchName, exp, chunkSize, frameSize)
  if reportPath == "":
    return 0
#  print "File %s" % (reportPath)
  attrName = attrNames[attrType];
  return getReportAttributeAllTimes(reportPath, attrName)

def getReportPathFrame(benchName, exp, chunkSize, frameSize):
  fileList = reportRoot + "/sesc_" + exp + "-" + str(chunkSize) + "chunkSz-" + str(frameSize) + "_" + benchName 
  return fileList

def getNumberStatsOneTime(benchName, exp, chunkSize, attrType):
  reportPath = getReportPath(benchName, exp, chunkSize)
  if reportPath == "":
    return 0
#  print "File %s" % (reportPath)
  attrName = attrNames[attrType];
  return getNumberStats(reportPath, attrName, True)


def getAttributeStatsAllTimes(benchName, exp, chunkSize, attrType):
  reportPath = getReportPath(benchName, exp, chunkSize)
  if reportPath == "":
    return 0
#  print "File %s" % (reportPath)
  attrName = attrNames[attrType];
  return getReportAttributeAllTimes(reportPath, attrName, True)

def getAttributeStatsAllTimesFrame(benchName, exp, chunkSize, frameSize, attrType):
  reportPath = getReportPathFrame(benchName, exp, chunkSize, frameSize)
  if reportPath == "":
    return 0
#  print "File %s" % (reportPath)
  attrName = attrNames[attrType];
  return getReportAttributeAllTimes(reportPath, attrName, True)

def getAttributeStatsVFrame(benchName, exp, chunkSize, frameSize, attrType):
  reportPath = getReportPathFrame(benchName, exp, chunkSize, frameSize)
  if reportPath == "":
    return 0
#  print "File %s" % (reportPath)
  attrName = attrNames[attrType];
  return getReportAttributeVStats(reportPath, attrName, True)

def getReportAttributeVStats(filePath, attrName, isAvg=False):
  try:
    f = open(filePath,"r")
  except IOError:
    return 0

  while True:
    l = f.readline()
    if l.find(attrName) != -1:
      if isAvg:
        cols = l.split(":")
#        print "COLS1 %s COLS2 %s COLS3 %s\n" % (cols[1], cols[2], cols[3])
        l = cols[2]
        cols1 = l.split("=")
        value1 = cols1[1]
        l = cols[3]
      else:
        value1 = "1"
      cols = l.split("=")
      value = cols[1].replace("\n","")
      f.close
#      print "CUT SIZE %d\n" % (float(value1))
      return float(value1)
    if len(l)==0:
      break
  f.close
  return 0







def getReportHistAttribute(filePath, attrName, index):
  attrName += "(" + str(index) + ")"
  return getReportAttribute(filePath, attrName)

def getReportHistAttribute1(filePath, attrName, index):
  attrName += "[" + str(index) + "]"
  return getReportAttribute(filePath, attrName)



def getReportProcAttribute1(filePath,attrName,index):
  attrName += "[" + str(index) + "]"
  return getReportAttribute(filePath, attrName)
 

def getReportProcAttribute(filePath, attrName, index):
  attrName = "P(" + str(index) + "):" + attrName
  return getReportAttribute(filePath, attrName)


def getAttributeAvg(benchName, exp, chunkSize, attrType):
  reportPath = getReportPath(benchName, exp, chunkSize)
  if reportPath == "":
    return 0
#  print "File %s" % (reportPath)
  attrName = attrNames[attrType];
  return getReportAttribute(reportPath, attrName, True)

def getHistAttribute(benchName, procs, attrType, index):
  reportPath = getReportPath(benchName, procs)
  if reportPath == "":
    return 0
  attrName = attrNames[attrType];
  return getReportHistAttribute(reportPath, attrName, index)

def getNoxReportPath(benchName, procs):
  fileList = glob.glob(reportNoxRoot + "/sbulk-report-final." + benchName + "." + str(procs) + ".re_exec_ratio-1.00.sig_gen_ratio-0.7")
  #print "report path: %s" % fileList
  if len(fileList) != 1:
    return ""
  return fileList[0]

def getNoxAttribute(benchName, procs, attrType, isAvg=False):
  reportPath = getNoxReportPath(benchName, procs)
  if reportPath == "":
    return 0
  attrName = attrNames[attrType];
  return getReportAttribute(reportPath, attrName, isAvg)

def getNoxHistAttribute1(benchName, procs, attrType, index):
  reportPath = getNoxReportPath(benchName, procs)
  if reportPath == "":
    return 0
  attrName = attrNames[attrType];
  return getReportHistAttribute1(reportPath, attrName, index)



def getNoxHistAttribute(benchName, procs, attrType, index):
  reportPath = getNoxReportPath(benchName, procs)
  if reportPath == "":
    return 0
  attrName = attrNames[attrType];
  return getReportHistAttribute(reportPath, attrName, index)

def getNoxProcAttribute(benchName, procs, attrType, index):
  reportPath = getNoxReportPath(benchName, procs)
  if reportPath == "":
    return 0
  attrName = attrNames[attrType];
  return getReportProcAttribute(reportPath, attrName, index)

def getNoxExecutionFraction(benchName, procs):
  totalChunks = getAttribute(benchName, procs, "committedChunks")
  noxChunks = getNoxAttribute(benchName, procs, "readChunks")
  if totalChunks > 0:
    return float(noxChunks) / float(totalChunks)
  else:
    return 0

def writeTexHeader(file, continued=False):
  file.write("\\begin{figure}[htb]\n")
  if continued:
    file.write("\\ContinuedFloat\n")
  file.write("\\begin{center}\n")

def writeTexBody(file, figureName, tag, width=1):
  file.write("\\begin{minipage}{%f\\columnwidth}\n" % (width))
  file.write("  \\includegraphics[width=\\columnwidth]{%s}\n" % (figureName))
  if tag != "":
    file.write("  \\subcaption{%s}\n" % (tag))
  file.write("\\end{minipage}\n")

def writeTexFooter(file, caption, label):
  file.write("\\begin{minipage}{\\columnwidth}\n")
  file.write("  \\caption{\\protect %s}.  \\label{fig:%s}\n" % (caption, label))
  file.write("\\end{minipage}\n")
  file.write("\\end{center}\n")
  file.write("\\end{figure}\n")

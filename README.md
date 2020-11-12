# HMM_Results
Results and proceedings for HMM Paper

This repository contains an example of the data analyzed in terms of temporal and spatial locality. It also includes the code necessary to simulate a prefetch using the HMM Model.

Due to the big size of some files, only a small portion of the applications are shown here.

 - The nvt folder contains the RAM access data captured by Pin.
 - The outA folder contains processed data of the RAM acceses, obtained via nvt2outA.py.
 - The soutA folder contains processed ordered data of the RAM acceses, obtained via outA2soutA.sh.
 - The loctmp folder contains data for temporal locality, obtained via outA2loctmp.py.
 - The locspa folder contains data for spatial locality, obtained via soutA2locspa.py.
 - The opkc folder contains summarized data about operations per kilocycle, obtained via nvt2opkc.py.
 - The prebus folder contains processed data memory access, ready to be simulated by a prefetch model, obtained via soutA2prebus.py.
 
Moreover, it is possible to find the temporal and spatial locality:
 
 - In TemporalLocalityAnalysis folder, graphics results about temporal locality are stored. They can be obtained using TemporalLocalityAnalysis.R
 - In TeOPKCAnalysis folder, graphics results about OPKC Analysis are stored. They can be obtained using OPKCAnalysis.R 
 
In the prefetch folder, necessary code to simulate the different prefetc experiments can be found, as well as the results obtained in csv format.

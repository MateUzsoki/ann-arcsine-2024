setwd(getSrcDirectory(function(){})[1])

source("ANN_AS_2023-S.R")
#source("YX_AS_2023.R")
library(tensorflow)



#tf$config$list_physical_devices("GPU")
#tf$version$VERSION

RemoveAllStartedMarkers()

#CalculateAnn('ANN-AS-S2ESMA1000T126K-SIGM',1000,126,TRUE,TRUE)

#CalculateAnn('ANN-AS-SESMA1000T126K-SIGM',1000,126,TRUE,TRUE)

#CalculateAnn(1000,126,FALSE,"", FALSE,"normal")
#CalculateAnn(2500,126,FALSE,"", FALSE,"t4")
CalculateAnn(2500,126,FALSE,"", FALSE,"normal")

#get_detailed_simulation_result_for_single_stock('normal')
#get_detailed_simulation_result_for_single_stock("t4")

#CalculateAnnXY('YX-AS-1000T126K',1000,126,TRUE,TRUE)

#CalculateAnn(1000,126,FALSE,"LIN")
#CalculateAnn(1500,126,FALSE,"LIN")

#CalculateAnn(2500,126,FALSE)

#require(devtools)
#install_version("VaRES", version = "1.0.1", repos = "http://cran.us.r-project.org")

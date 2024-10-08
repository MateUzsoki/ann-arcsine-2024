library(reticulate)

library(tensorflow)

use_condaenv("/home/muz/anaconda3/envs/new/bin/python",TRUE)

library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)    # alternatively, this also loads %>%

tf$config$list_physical_devices("GPU")

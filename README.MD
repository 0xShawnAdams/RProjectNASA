# R Project for coursework. Graphing a NASA Dataset.
This dataset describes lithium-ion battery aging experiments performed using four battery cells identified as B0005, B0006, B0007, and B0018. Each observation in this analysis represents one discharge cycle, producing more than 30 observations in total. The first numerical variable is discharge capacity, measured in ampere-hours (Ah), which indicates how much electrical charge the battery delivered during that cycle. The second numerical variable is the average measured battery temperature during the discharge cycle, measured in degrees Celsius. The categorical variable is battery ID, which identifies the specific battery cell that produced each observation. We chose this dataset because it provides real experimental data that make it possible to explore battery capacity decline and temperature variation across different cells; the source is NASA’s Li-ion Battery Aging Dataset, DASHlink resource 133

# installation / setup..?
install.packages(c("R.matlab", "dplyr", "purrr", "tibble", "readr", "ggplot2"))

library(R.matlab)
library(dplyr)
library(purrr)
library(tibble)
library(readr)
library(ggplot2)

# NASA dataset here, see datasetdownload.py
https://data.nasa.gov/dataset/li-ion-battery-aging-datasets



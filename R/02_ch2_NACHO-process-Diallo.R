# Stable Isotope Analysis Data Processing
# 2021-06-16

# the function in this script is written in another script: NACHO-function.R

# load packages
library(tidyverse)
library(ggpubr)
library(gridExtra)

# load function from function script (just run the entire thing)

# then execute the function with arguments
NACHO.data.process(RawEAfile = "data/02_mass_spec_raw_files/221208_Veggerby_EA_Run36.csv", 
                                # you have to re-save the raw data file as a .csv file
                   sequenceID = "set36", 
                   save.file = "YES", 
                   plot = "YES", 
                   seq.eval = "YES")
 


# Sequence Evaluation -----------------------------------------------------
# set up sequence evaluation file - this has to be done before using function
# with seq.eval = "YES"

# seq.eval.data <- data.frame(matrix(nrow = 1, ncol = 9))
# colnames(seq.eval.data) <- c("date", "sequenceID", "samples", "N.prec", "C.prec", "N.accur", 
#                             "C.accur", "GA1.N", "SAL.C")
# write_csv(x = seq.eval.data, file = "Sequence_Evaluation.csv", append = FALSE)

# after a sequence result row is added, then the first row of "NA" can be deleted

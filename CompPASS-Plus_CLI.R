##
#
# The file CompPASS-Plus_CLI.R is a script that runs the CompPASS-Plus algorithm.
# After importing a specified file, it runs the CompPASS-Plus algorithm and exports
# the results. 
#
# The copyright for this script belongs to Edward Huttlin and the President and 
# Fellows of Harvard University.
#
# Ed Huttlin (edward_huttlin@hms.harvard.edu)
# January 2021
#
##

##
# COMMAND LINE ARGUMENTS:
#
# This script accepts four command line arguments in the following order. 
# All are required.
#
# 1. dataFile - the name of the file to be imported containing data for classification.
# 2. outputFile - the name of the file to be written containing the results of classification.
# 3. cvUnit - the basic unit by which results should be grouped when performing cross-validation.
# 4. formula - this formula specifies the model that should be used for classification.
##

# Handle command line arguments
print("Reading command line agruments.", quote = FALSE)
args = commandArgs(trailingOnly = TRUE)

# Debugging only: manual specification of arguments
if(length(args) == 0){
  args = c("compPASS-Plus_Test_Data.tsv", 
          "output.tsv", 
          "plate_num", 
          'class~uPepBins+entropy_pct+nwdscore_pct+zscore_pct+plate_zscore_pct+ratio_pct+ratioTotalPSMs_pct+total_psms_pct+UtoTratio_pct'
          )
}

dataFile = args[1]
outputFile = args[2]
cvUnit = args[3]
formula = as.formula(args[4])

print(paste("DataFile: ", dataFile), quote = FALSE)
print(paste("OutputFile: ", outputFile), quote = FALSE)
print(paste("cvUnit: ", cvUnit), quote = FALSE)
print(paste("formula: ", formula), quote = FALSE)

# Load libraries
library("e1071")
source("CompPASS-Plus.R") # Change this line once this is a proper package.

# Load data from file
print("Reading data from file.", quote = FALSE)
data = read.delim(dataFile, header=TRUE)

data$class = as.factor(data$class)

data["uPepBins"] = data["uPeps"]
data[which(data$uPepBins > 10), "uPepBins"] = 10
data$uPepBins = as.factor(round(data$uPepBins))

# Add percentile factors for selected parameters
data = addPercentileFactors(data)

head(data)

# Run CompPASS-Plus
print("Running CompPASS-Plus", quote = FALSE)

results = comppassPlus(data, formula, cvUnit)

# Remove percentile features from output
results = subset(results, select=-c(uPepBins, ave_apsm_pct, entropy_pct, nwdscore_pct, zscore_pct, plate_zscore_pct, total_psms_pct, ratio_pct, ratioTotalPSMs_pct, UtoTratio_pct))

# Write results to file
print("Writing results to file.", quote = FALSE)

write.table(results, outputFile, row.names=FALSE, col.names=TRUE, sep="\t")

print("Done!", quote = FALSE)
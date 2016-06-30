setwd("~/Desktop/useR-scalable-ml-h2o-tutorial/scripts")
library(h2o)
h2o.init(nthreads = -1)
dataPath   <- normalizePath("~/Desktop/useR-scalable-ml-h2o-tutorial/data")
loanPath   <- normalizePath("~/Desktop/useR-scalable-ml-h2o-tutorial/data/loanStats/")
rejectPath <- normalizePath("~/Desktop/useR-scalable-ml-h2o-tutorial/data/rejectStats/")
loans.hex  <- h2o.importFile(path = loanPath, destination_frame = "loanStats")
reject.hex <- h2o.importFile(path = rejectPath, destination_frame = "rejectStats")

names(reject.hex) = c("loan_amnt", "issue_d", "title", "fico_range_high", 
                      "dti", "zip_code", "addr_state", "emp_length", "policy_code")

# Set binary response column. Rejected application = 0 and accepted application = 1.
reject.hex$app_status <- 0
loans.hex$app_status <- 1

# Create a year column for both dataset for uniformity
reject.hex$year <- h2o.year(reject.hex$issue_d) + 1900
loans.hex$year <- h2o.strsplit(loans.hex$issue_d, "-")[,2]

# Change dti in reject.hex from factor to numerics
reject.hex$dti <- h2o.strsplit(x = reject.hex$dti, split = "%")[,1]

# Select like columns to keep
cols <- setdiff(names(reject.hex), c("issue_d"))

# Export cleaned rejectStats and loanStats
loanPath   <- paste0(dataPath,"/loans.csv")
rejectPath <- paste0(dataPath,"/reject.csv")
h2o.exportFile(data = loans.hex[,cols], path = loanPath, force = T)
h2o.exportFile(data = reject.hex[,cols], path = rejectPath, force = T)

# Row bind data together
loans.hex  <- h2o.importFile(path = loanPath, destination_frame = "loanStats")
reject.hex <- h2o.importFile(path = rejectPath, destination_frame = "rejectStats")
data <- h2o.rbind(loans.hex, reject.hex)

h2o.exportFile(data = data, path = paste0(dataPath,"/LC_Acceptance.csv"))

data$app_status <- as.factor(data$app_status)

split <- h2o.splitFrame(data, 0.8)
train <- split[[1]]
valid <- split[[2]]

myY <- "app_status"
myX <- setdiff(names(data), c("policy_code", "title", "zip_code", myY))
gbm_m <- h2o.gbm(x = myX, y = myY, training_frame = train, validation_frame = valid,  
                  model_id = "gbm_model")

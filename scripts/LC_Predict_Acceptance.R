setwd("~/Desktop/useR-scalable-ml-h2o-tutorial/scripts")
library(h2o)
h2o.init(nthreads = -1, min_mem_size = "8G")
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
#h2o.exportFile(data = loans.hex[,cols], path = loanPath, force = T)
#h2o.exportFile(data = reject.hex[,cols], path = rejectPath, force = T)

# Row bind data together
loans.hex  <- h2o.importFile(path = loanPath, destination_frame = "loanStats")
reject.hex <- h2o.importFile(path = rejectPath, destination_frame = "rejectStats")
data <- h2o.rbind(loans.hex, reject.hex)

#h2o.exportFile(data = data, path = paste0(dataPath,"/LC_Acceptance.csv"))

data$app_status <- as.factor(data$app_status)

split <- h2o.splitFrame(data, 0.8)
train <- split[[1]]
valid <- split[[2]]

myY <- "app_status"
myX <- setdiff(names(data), c("policy_code", "title", "zip_code", myY))
gbm_m <- h2o.gbm(x = myX, y = myY, training_frame = train, validation_frame = valid,  
                  model_id = "gbm_model")

# Now, rather than training models manually one-by-one, we will make
# use of the h2o.grid function to train a bunch of models at once

# Cartesian Grid Search
# By default, h2o.grid will train a Cartesian
# grid search -- all models in the specified grid 


# GBM hyperparamters
gbm_params1 <- list(learn_rate = c(0.01, 0.1),
                    max_depth = c(3, 5, 9),
                    sample_rate = c(0.8, 1.0),
                    col_sample_rate = c(0.2, 0.5, 1.0))

# Train and validate a grid of GBMs
gbm_grid1 <- h2o.grid("gbm", x = myX, y = myY,
                      grid_id = "gbm_grid1",
                      training_frame = train,
                      validation_frame = valid,
                      ntrees = 100,
                      seed = 1,
                      hyper_params = gbm_params1)

# Get the grid results, sorted by AUC
gbm_gridperf1 <- h2o.getGrid(grid_id = "gbm_grid1", 
                             sort_by = "auc", 
                             decreasing = TRUE)
print(gbm_gridperf1)

# Random Grid Search
# This is set to run fairly quickly, increase max_runtime_secs 
# or max_models to cover more of the hyperparameter space.
# Also, you can expand the hyperparameter space of each of the 
# algorithms by modifying the hyper param code below.


# GBM hyperparamters
gbm_params2 <- list(learn_rate = seq(0.01, 0.1, 0.01),
                    max_depth = seq(2, 10, 1),
                    sample_rate = seq(0.5, 1.0, 0.1),
                    col_sample_rate = seq(0.1, 1.0, 0.1))
search_criteria2 <- list(strategy = "RandomDiscrete", 
                         max_models = 36)

# Train and validate a grid of GBMs
gbm_grid2 <- h2o.grid("gbm", x = MyX, y = MyY,
                      grid_id = "gbm_grid2",
                      training_frame = train,
                      validation_frame = valid,
                      ntrees = 100,
                      seed = 1,
                      hyper_params = gbm_params2,
                      search_criteria = search_criteria2)

gbm_gridperf2 <- h2o.getGrid(grid_id = "gbm_grid2", 
                             sort_by = "auc", 
                             decreasing = TRUE)
print(gbm_gridperf2)


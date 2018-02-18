#' Create a list of pre-defined models from a dataset.
#'
#' @param formula A model formula
#' @return List of caret train objects
#' @seealso \code{\link{train}}
#' @export
createModelLibrary <- function(formula, data, modellist = NULL, metric = "ROC", model_setup = NULL, model_control = NULL, mini = FALSE, preProcess = NULL, runParallel = FALSE){
  start.time <- Sys.time()
  
  ### SETUP ############
  # Enable parallelization
  if(runParallel > 0){
    if(runParallel == TRUE) runParallel = detectCores() -1
    # Setup up parallel backend
    ##### Setting up parallelization
    cl <- makeCluster(min(detectCores()-1, runParallel))
    #, outfile="") # This redirects the output to the R master console but not in RStudio
    # on.exit(stopCluster(cl))
    registerDoParallel(cl)
    message(paste("\n Registered number of cores:\n",getDoParWorkers(),"\n"))
  }else{runParallel = FALSE}
  
 if(is.null(modellist))  modellist = c("C5.0")
 
 if(is.null(model_setup)) model_setup = model_parameters()
  
  # Specify model estimation control settings
  if(is.null(model_control)){
  model_control <- trainControl(
    method = "cv", # 'cv' for cross validation
    number = ifelse(mini == TRUE, 2, 5), # number of folds in cross validation
    #repeats = 3, # number for repeated cross validation
    savePredictions = FALSE,
    classProbs = TRUE,
    #index = createFolds(data_tr$Class, 4),
    summaryFunction = twoClassSummary,
    allowParallel = runParallel > 0, # enable parallelization
    returnData = FALSE
  )}
  
  
  ### INITIALIZE FRAMEWORK ####
  #++++++++++++++++++++++++++++

  ### MODEL TRAINING ####
  # Build list of models with caret::train
  message("Start model training")
  modelLibrary <- list()
  for(model in modellist){
    message(paste("Start training model", model, "at", Sys.time()))
      modelObject <- try(do.call(caret::train, c(
        list(
          #as.formula(paste0(targetVariable,"~.")), # specify target variable and predictors. The dot '.' stands for 'all other variables' 
          #data=as.data.frame(data_tr), # specify training data.
          form = formula,
          data = data,
          trControl = model_control, # specify the control parameters
          preProcess = preProcess, # standardize variables by centering and scaling. Also exclude variables with no variance
          metric = metric # specify metric, e.g. RSME, ROC (auc). 
        ),
        # Specify the list of models and training parameters for training
        model_setup[[model]]
      ))# end of train do.call
      ) # end of try bracket
    if("train" %in% class(modelObject)) {modelLibrary[[model]] <- modelObject
    } else{warning("Failed to train model ", model, ". ", modelObject)}
    rm(modelObject)
  }
  end.time <- Sys.time() # Print time
  message(paste("Training time:",end.time - start.time))
  
  # Save trained model library
  #saveRDS(modelLibrary, sprintf("modelLibrary %s target_%s balanced_%s varSelection_%s.rds", analysisName, targetVariable, balanceDataset, variableSelectionMethod)) 
  #if(runParallel == TRUE)try(stopCluster(cl)) # Close connections to the parallelization cluster
  return(modelLibrary)
}


#' Load a list of pre-defined tuning grids for fast model training
#'
#' @param mini Toogle outputting mini set of parameters for debugging
#' @return A list of model tuning parameters sorted by model name
#' @export
model_parameters <- function(mini = FALSE){
  ### Specify learning algorithms and training grids
  if(mini == FALSE){
    model_setup = list("elasticNet" = list("tuneGrid" = expand.grid(alpha = seq(0.01,1,0.02),
                                                                    lambda = seq(0.1, 0.001,-0.001))
                                           ,"method" = "glmnet")
                       ,"lasso" = list("tuneGrid" = expand.grid(alpha = 1,
                                                                lambda = seq(0.1, 0.001,-0.001))
                                       ,"method" = "glmnet")
                       ,"ridge" = list("tuneGrid" = expand.grid(alpha = 0,
                                                                lambda = seq(0.1, 0.001,-0.001))
                                       ,"method" = "glmnet")
                       ,"flexLogit" = list("tuneGrid" = expand.grid(alpha = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1),
                                                                    lambda_c = c(0.1, 0.05, 0.01, 0.005, 0.001, 0.0005),
                                                                    softThresholding = c(TRUE, FALSE),
                                                                    dynamic = FALSE)
                                           , "maxiter" = 1000, startValues = "logit", popSize = 100)
                       ,"NaiveBayes" = list("tuneGrid"=expand.grid(.usekernel = TRUE
                                                                   ,fL = 2:10
                                                                   ,adjust = TRUE)
                                            , "method" = "nb")
                       , "C5.0"  = list("tuneGrid" = expand.grid(winnow = c(TRUE,FALSE),
                                                                 trials = 1,
                                                                 model = "tree")
                                        , "method" = "C5.0")
                       , "adaboost"  = list("tuneGrid" = expand.grid(nIter = 2^seq(3,11,2),
                                                                     method = c("Adaboost.M1",
                                                                                "Real adaboost"))
                                            , "method" = "adaboost")
                       , "NN" =  list("tuneGrid" = expand.grid(decay = 10^seq(-4, 0, 0.5)
                                                               ,size = seq(3,13,2))
                                      ,"maxit" = 1000
                                      , "MaxNWts" = 2000
                                      , "method" = "nnet")
                       , "RF" =  list("tuneGrid" = expand.grid(mtry= c(5,8,10,12,15,20))#,150,200))
                                      , "ntree"= 1000, "nodesize" = 20
                                      , "method" = "rf")
                       , "rpart" =  list("tuneGrid" = expand.grid(cp = seq(0.001,0.1,0.01))
                                         , "method" = "rpart")
                       , "SVMradial" =  list("tuneGrid" = expand.grid(sigma= 2^seq(-12,-1),
                                                                      C = 2^seq(-12,12))
                                             ,"method" = "svmRadial")
                       , "SVMlinear" =  list("tuneGrid" = expand.grid(C = 2^seq(-12,12))
                                             ,"method" = "svmLinear")
                       , "xgboost" = list("tuneGrid" = expand.grid(nrounds= c(10, 25, 50, 100, 250, 500) # max. number of iterations
                                                                   ,max_depth = seq(2,8,2) # maximum depth of tree
                                                                   ,eta = 10^seq(-4,-1,1) # shrinking parameter for the weights
                                                                   ,gamma = 0.001 # min. required improvement in loss function
                                                                   ,colsample_bytree= 0.7 # how many features to sample for tree
                                                                   ,min_child_weight=0.5 # min. sum of weights of all obs. in child node
                                                                   , subsample = 0.7
                       )
                       ,"method" = "xgbTree") #  xgbLinear also exists
    )
  }else{model_setup = list("elasticNet" = list("tuneGrid" = expand.grid(alpha = seq(0.01,1,0.4),
                                                                        lambda = seq(0.1, 0.001,-0.05))
                                               ,"method" = "glmnet")
                           ,"lasso" = list("tuneGrid" = expand.grid(alpha = 1,
                                                                    lambda = seq(0.1, 0.001,-0.05))
                                           ,"method" = "glmnet")
                           ,"ridge" = list("tuneGrid" = expand.grid(alpha = 0,
                                                                    lambda = seq(0.1, 0.001,-0.05))
                                           ,"method" = "glmnet")
                           ,"flexLogit" = list("tuneGrid" = expand.grid(alpha = 0.5,
                                                                        lambda_c = c(0.05, 0.01),
                                                                        softThresholding = FALSE,
                                                                        dynamic = FALSE)
                                               , "maxiter" = 10, "popSize" = 10, 'parallel' = FALSE)
                           ,"NaiveBayes" = list("tuneGrid"=expand.grid(.usekernel = TRUE
                                                                       ,fL = 2:3
                                                                       ,adjust = TRUE)
                                                , "method" = "nb")
                           , "C5.0"  = list("tuneGrid" = expand.grid(winnow = c(TRUE,FALSE),
                                                                     trials = 1,
                                                                     model = "tree")
                                            , "method" = "C5.0")
                           , "adaboost"  = list("tuneGrid" = expand.grid(nIter = 2^c(3,5),
                                                                         method = c("Adaboost.M1",
                                                                                    "Real adaboost"))
                                                , "method" = "adaboost")
                           , "NN" =  list("tuneGrid" = expand.grid(decay = 10^-4
                                                                   ,size = c(2,3))
                                          ,"maxit" = 100
                                          , "MaxNWts" = 2000
                                          , "method" = "nnet")
                           , "rpart" =  list("tuneGrid" = expand.grid(cp = c(0.01, 0.001))
                                             #, 'options' = list("ntree"= 1000, "nodesize" = 20)
                                             , "method" = "rpart")
                           , "RF" =  list("tuneGrid" = expand.grid(mtry= c(1,2))#,150,200))
                                          , "ntree"= 20, "nodesize" = 100
                                          , "method" = "rf")
                           , "SVMradial" =  list("tuneGrid" = expand.grid(sigma= 2^seq(-4,-1),
                                                                          C = 2^seq(-4,4))
                                                 ,"method" = "svmRadial")
                           , "SVMlinear" =  list("tuneGrid" = expand.grid(C = 2^c(-4,-3))
                                                 ,"method" = "svmLinear")
                           , "xgboost" = list("tuneGrid" = expand.grid(nrounds= c(10, 25) # max. number of iterations
                                                                       ,max_depth = 2 # maximum depth of tree
                                                                       ,eta = 10^-3 # shrinking parameter for the weights
                                                                       ,gamma = 0.001 # min. required improvement in loss function
                                                                       ,colsample_bytree= 0.7 # how many features to sample for tree
                                                                       ,min_child_weight=0.5 # min. sum of weights of all obs. in child node
                                                                       , subsample = 0.7
                           )
                           ,"method" = "xgbTree") #  xgbLinear also exists
  )}
  
  return(model_setup)
}


# Fitting MaxEnt species distributions
# Adapted to use SDMtune
# 16 May 2022
# Approx 35 min for all 5 species
startTime = Sys.time()
set.seed(20081104) # Obama elected prez

#### Load packages ----
library(raster)
library(sf)
library(sp)
library(SDMtune)
library(ENMeval)
library(zeallot)
library(tidyverse)
library(dismo)

#### Define parameters
## Output folder
destinationFolder = "models/sdm_10k"

if(!dir.exists(destinationFolder)){
  dir.create(destinationFolder)
}

## Predictor layers of interest 
## Layer order is: 
## 01-19: Bioclimatic layers
## 20-21: "DEM","TRI"    Terrain layers
## 22-38: Land use layers;
##        NET_tem, NET_bor, NDT_bor,      Needleleaf trees
##        BET_tro, BET_tem,               Broadleaf evergreens
##        BDT_tro, BDT_tem, BDT_bor,      Broadleaf deciduous
##        BES_tem, BDS_tem, BDS_bor,      Shrubs
##        C3_gra_arc,  C3_gra ,  C4_gra , Graminoids
##        AGR, Urban, Barren

## Species names
speciesList = c("Ovis canadensis",
                "Ovis dalli",
                "Oreamnos americanus",
                "Ovibos moschatus",
                "Bison bison")


#### Import data ----
# Load the dismo data
dismoData = read_sf("data/gbif/bovidae_thinned.shp")
names(dismoData)

# Load the background points
bg = read_sf("data/background/Background_10000.shp")
names(bg)

# Transform dismo data to match projected coord info in bg and predictors
pr = dismoData %>%
  st_transform(st_crs(bg))

# Load the raster data
predictors = raster::stack("data/worldclim_v2/bioclim/reprojected/biocl_centered.tif")
names(predictors)
## Aggregate the tree products
predictors[["treeTropical"]] <- predictors[["BET_tro"]] +
  predictors[["BDT_tro"]] 
predictors[["treeTemperate"]] <- predictors[["NET_tem"]] + 
  predictors[["BET_tem"]] + predictors[["BDT_tem"]]
predictors[["treeBoreal"]] <-  predictors[["NET_bor"]] + 
  predictors[["NDT_bor"]] + 
  predictors[["BDT_bor"]]
## Remove the unaggregated base layers
predictors = dropLayer(predictors,
                       c("BET_tro","BDT_tro",
                         "NET_tem","BET_tem","BDT_tem",
                         "NET_bor","NDT_bor","BDT_bor"))
names(predictors)

#### Define maxent function ----
## First, set the maxent method to avoid printing 
## "This is MaxEnt version 3.4.3" every line. Note however 
# that silencing this message means that you need to
## manually confirm your maxent version.
setMethod('maxent', signature(x='missing', p='missing'), 
          function(x, p, silent=TRUE, ...) {
            
            if (is.null(getOption('dismo_rJavaLoaded'))) {
              # to avoid trouble on macs
              Sys.setenv(NOAWT=TRUE)
              if ( requireNamespace('rJava') ) {
                rJava::.jpackage('dismo')
                options(dismo_rJavaLoaded=TRUE)
              } else {
                if (!silent) {
                  cat("Cannot load rJava\n")			
                }
                return(FALSE)
              }
            }
            
            if (is.null(getOption('dismo_maxent'))) {
              mxe <- rJava::.jnew("meversion") 
              v <- try(rJava::.jcall(mxe, "S", "meversion"), silent=TRUE)
              if (class(v) == 'try-error') {
                if (!silent) {
                  cat("MaxEnt is missing or incompatible with your version of Java\n")
                }
                return(FALSE)
              } else if (v == "3.3.3a") {
                if (!silent) {
                  cat("This is not a compatible version of Maxent\n")
                }
                return(FALSE)
              }
              options(dismo_maxent=v)
            } else {
              v = getOption("dismo_maxent")
            }
            if (!silent) {
              cat("This is MaxEnt version", v, "\n")
            }
            invisible(TRUE)
          }
)

#### calcME is a function to generate maxent models
## for:
## speciesName = Species name (character Genus species)
## preds = raster stack with named predictor layers 
## bgDat = background data (must contain species name column called `species`)
## prDat = presence data (must contain species name column called `species`)
## outPath = directory to store maxent model outputs
calcME = function(speciesName = speciesList[1],
                  preds = predictors,
                  bgDat = bg,
                  prDat = pr,
                  outPath = destinationFolder){
  #### Initial checks ----
  presenceInput = prDat %>%
    dplyr::filter(species == speciesName)
  backgroundInput = bgDat %>%
    dplyr::filter(species == speciesName)
  if(st_crs(presenceInput) != st_crs(backgroundInput)){
    stop("Inconsistent reference system! 
         Check CRS of background & dismo data")
  }
  if(st_crs(backgroundInput) != st_crs(preds)){
    stop("Inconsistent reference system! 
         Check CRS of background & predictors data")
  }
  ## Select data
  x = st_coordinates(presenceInput)
  y = st_coordinates(backgroundInput)
  # Get species name
  speciesDSM = presenceInput$species[1]
  speciesBG = backgroundInput$species[1]
  if(speciesDSM != speciesBG){
    stop("Inconsistent species ID! 
         Check names(background) and names(dismo)")
  }else{species <- speciesDSM}
  message(species)
  message(Sys.time())

  #### SDMtune workflow ----
  ## See Vignali et al 2020. The workflow is as follows:
  ## 2.1 Prep data. Create an SWD object with prepareSWD(), 
  ##     generate training and testing partitions with trainValTest(),
  ##     and define cross validation structure with e.g. ENMeval::get.block()
  ## 2.2 Train and evaluate model. Begin by generating a model with a defined
  ##     allowable feature classes, regularization multiplier, folds, 
  ##     and evaluation metric using all desired input predictors
  ## 2.3 Perform variable selection with varSel()
  ## 2.4 Tune hyperparameters with optimizeModel()
  ## 2.5 Optimize parsimony with reduceVar()
  ## 2.6 Evaluate final model using the held apart test data
  ## 2.7 Generate output, saving results and metadata using modelReport()
  
  ## > 2.1 Data preparation ----
  message("Organizing and partitioning data...")
  ## Create an SWD object
  mySWD <- prepareSWD(species = speciesName,
                      p = x, a = y,
                      env = preds)
  ## Partition training and testing data
  #  May 24, 1869: Powell starts off from the Grand Canyon
  c(occtrain, occval, occtest) %<-% trainValTest(mySWD,
                                                 val = 0.2,
                                                 test = 0.2, 
                                                 only_presence = TRUE,
                                                 seed = 18690524 
                                                 )
  ## Create spatial blocks - but using only training data
  check_folds <- get.checkerboard1(
    occs = occtrain@coords[occtrain@pa == 1, ],
    bg = occtrain@coords[occtrain@pa == 0, ],
    envs = preds,
    aggregation.factor = 2)
  evalplot.grps(pts = occtrain@coords[occtrain@pa == 1, ], 
                pts.grp = check_folds$occs.grp, 
                envs = preds)
  
  ## > 2.2 Training and evaluating ----
  message("Fitting initial model...")
  ## Model training
  maxent_model <- train(method = "Maxent", 
                        data = occtrain, 
                        fc = "lq", 
                        reg = 1, 
                        folds = check_folds)
  
  ## Variable importance
  vi <- maxentVarImp(maxent_model)
  # Variable importance
  varImp = plotVarImp(vi[, 1:2]) +
    ggtitle("Variable importance")
  # Permutation importance
  permImp = plotVarImp(vi[, c(1,3)]) +
    ggtitle("Permutation importance")
  cowplot::plot_grid(varImp, permImp, nrow = 1)
  
  
  ## > 2.3 Variable selection ----
  message("Selecting variables...")
  ## First, make background data
  bg_coords <- dismo::randomPoints(preds, 10000)
  bg_varSel <- prepareSWD(species = speciesName, 
                          a = bg_coords, 
                          env = preds)
  ## varSel procedure takes about 15 minutes for 14 layers
  selected_variables_model <- varSel(maxent_model, 
                                     metric = "auc", 
                                     bg4cor = bg_varSel,
                                     method = "spearman", 
                                     cor_th = 0.7,
                                     env = preds, 
                                     use_pc = TRUE)
  
  ## Check model AUC's
  message(paste0("Training AUC: ", 
                 round(auc(selected_variables_model),
                       4)))
  message(paste0("Testing AUC: ", 
                 round(auc(selected_variables_model, 
                           test = occtest),
                       4)))
  
  
  ## > 2.4 Optimize model ----
  message("Optimizing model...")
  ## Generate a set of hyperparameters to test
  h <- list(reg = seq(0.5, 10, 0.5), 
            fc = c("l", "lq"))
  ## Run the experiment using genetic algorithm to identify best model
  experiment1 <- optimizeModel(selected_variables_model, 
                               hypers = h,
                               metric = "auc", 
                               test = occval,
                               pop = 15,
                               gen = 10, 
                               seed = 18421104 # Abe marries Mary Todd
  )
  
  ## Select the best model and evaluate
  # Index of the best model in the experiment
  bestModelIndex <- which.max(experiment1@results$test_AUC)  
  # New train dataset containing only the selected variables
  new_trainingDat <- experiment1@models[[bestModelIndex]]@data  
  # Merge only presence data
  merged_data <- mergeSWD(new_trainingDat, 
                          occval, 
                          only_presence = TRUE) 
  optimized_model <- train("Maxent", 
                           data = merged_data, 
                           fc = experiment1@results[bestModelIndex, 1],
                           reg = experiment1@results[bestModelIndex, 2])
  
  
  ## > 2.5 Model parsimony
  message("Removing low-importance variables...")
  ## Use reduceVar() to identify low-impact variables that can be removed
  final_model <- reduceVar(optimized_model, 
                           th = 2, 
                           metric = "auc",
                           test = occval, 
                           permut = 10,
                           use_jk = TRUE)
  
  ## 2.6 > Model evaluation
  message("Evaluating model...")
  ## Evaluate model using the fully withheld test data
  message(paste0("Final model AUC: ", 
                 round(auc(final_model, test = occtest), digits = 4)))
  plotROC(final_model, test = occtest)
  message(paste0("Final model TSS: ", 
                 round(tss(final_model), digits = 4)))
  
  ## Identify thresholds
  ths <- thresholds(final_model, type = "cloglog", test = occtest)
  message("Thresholds:")
  # print(ths)
  
  
  ## > 2.7 Generate report ----
  message("Generating report...")
  ## Note that the modelReport() function from SDMtune has to be 
  ## modified as follows because modelReport.Rmd has bugs that I've 
  ## fixed on my machine but haven't been incorporated in SDMtools
  ## as of 18 May 2022. (Note therefore that this function calls 
  ## to scripts/modelReport.Rmd)
  modelReport = function (model, folder, test = NULL, 
                          type = NULL, response_curves = FALSE, 
                          only_presence = FALSE, jk = FALSE,
                          env = NULL, clamp = TRUE, 
                          permut = 10, factors = NULL){
    if (!requireNamespace("kableExtra", quietly = TRUE)) {
      stop("You need the packege \"kableExtra\" to run this function,", 
           " please install it.", call. = FALSE)
    }
    if (!requireNamespace("cli", quietly = TRUE)) {
      stop("You need the packege \"cli\" to run this function,", 
           " please install it.", call. = FALSE)
    }
    if (!requireNamespace("crayon", quietly = TRUE)) {
      stop("You need the packege \"crayon\" to run this function,", 
           " please install it.", call. = FALSE)
    }
    if (!requireNamespace("htmltools", quietly = TRUE)) {
      stop("You need the packege \"htmltools\" to run this function,", 
           " please install it.", call. = FALSE)
    }
    if (file.exists(file.path(getwd(), folder))) {
      msg <- message(crayon::red(cli::symbol$fancy_question_mark), 
                     " The folder '", folder, "' already exists, do you want to overwrite it?")
      continue <- utils::menu(choices = c("Yes", "No"), title = msg)
    }
    else {
      continue <- 1
    }
    if (continue == 1) {
      template <- "scripts/modelReport.Rmd"
      folder <- file.path(getwd(), folder)
      dir.create(file.path(folder, "plots"), recursive = TRUE, 
                 showWarnings = FALSE)
      species <- gsub(" ", "_", tolower(model@data@species))
      title <- paste(class(model@model), "model for", model@data@species)
      args <- c(paste0("--metadata=title:\"", title, "\""))
      output_file <- paste0(species, ".html")
      rmarkdown::render(template, output_file = output_file, 
                        output_dir = folder, 
                        params = list(model = model, 
                                      type = type,
                                      test = test, 
                                      folder = folder, 
                                      env = env, 
                                      jk = jk, 
                                      response_curves = response_curves, 
                                      only_presence = only_presence, 
                                      clamp = clamp, 
                                      permut = permut,
                                      factors = factors), 
                        output_options = list(pandoc_args = args), quiet = TRUE)
      utils::browseURL(file.path(folder, output_file))
    }
  }
  
  modelReport(final_model,
              type = "cloglog", 
              folder = 
                paste0(destinationFolder,"/",
                       gsub(pattern = " ",
                            replacement = "-",
                            x = speciesName)), 
              test = occtest,
              response_curves = TRUE, 
              only_presence = TRUE, 
              jk = TRUE,
              env = preds)
  
  BRRR::skrrrahh(12)
  #### Return results ----
  return(list("MaxEnt" = final_model,
              "Thresholds" = ths))
}


#### Apply function and save outputs ----
myMaxEnt <- lapply(X = speciesList,
                   FUN = calcME)
names(myMaxEnt) <- speciesList
str(myMaxEnt[[1]]$MaxEnt)


#### Save outputs ----
saveRDS(myMaxEnt,
        file = paste0(destinationFolder, "/MaxEnt.RDS"))

stopTime = Sys.time()
print(stopTime-startTime)
BRRR::skrrrahh(4)



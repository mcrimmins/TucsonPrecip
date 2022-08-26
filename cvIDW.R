# parameter tuning for IDW 
# from https://www.geo.fu-berlin.de/en/v/soga/Geodata-analysis/geostatistics/Inverse-Distance-Weighting/Model-selection/index.html
# MAC 08/17/2022

# loop through all combos
cv.IDW  <- function(spatialDF, stat.formula = NULL,
                    seqNeighbors = NULL, seqBeta = NULL,
                    evalGridSize = NULL, 
                    evalRaster = NULL, 
                    verbose = TRUE){
  
  ### LOAD LIBRARIES ###
  library(sp)
  #library(sf)
  library(gstat)
  library(raster)
  
  
  # spatialDF = daySp 
  # stat.formula = formula(rainAmount ~ 1) 
  # evalGridSize = 0.01 # in units of prs
  # evalRaster = r
  # seqNeighbors = seq(from = 6, to = 8, 1)
  # seqBeta = seq(from = 1, to = 2, 0.5)
  # verbose = T
  
  ### PROVIDE DEFAULT VALUES FOR FUNCTION ARGUMENTS ###
  if (is.null(seqNeighbors)){
    seqNeighbors <- round(seq(3, length(spatialDF), length.out = 10))
  }
  if (is.null(seqBeta)){
    seqBeta <- c(0.1, seq(0.5, 3, 0.5))
  }
  if (is.null(evalGridSize)){
    x.interval <- extent(spatialDF)@xmax - extent(spatialDF)@xmin
    y.interval <- extent(spatialDF)@ymax - extent(spatialDF)@ymin
    evalGridSize <- round(min(x.interval, y.interval) *0.05)
  }
  if (is.null(stat.formula)){
    print('Please provide a formula!!')
    return()
  }
  if (is.null(evalRaster)){
    extent.evalGrid <- extent(spatialDF)
  }else{
    extent.evalGrid <- extent(evalRaster)
  }
  
  
  ### BUILD A GRID FOR PARAMETER COMBINATIONS ###
  cv.Grid <- expand.grid(Beta = seqBeta,
                         Neighbors = seqNeighbors)
  cv.Grid$RMSE <- NA
  
  ### LOOP THROUGH ALL PARAMETER COMBINATIONS ###
  for (i in 1:nrow(cv.Grid)){
    ### BUILD IDW MODEL ###
    idw <- gstat(formula = stat.formula,
                 data = spatialDF, 
                 nmax = cv.Grid[i, 'Neighbors'], 
                 set = list(idp = cv.Grid[i, 'Beta']))
    ### PERFORM LOOCV ###
    crossval <- gstat.cv(idw, 
                         nmax = cv.Grid[i, 'Neighbors'],
                         beta = v.Grid[i, 'Beta'],
                         debug.level = 0)
    cv.Grid[i, 'RMSE'] <- RMSE(crossval$residual)
    if (verbose){
      print(paste('Function call', i, 'out of',  nrow(cv.Grid)))
      print(paste('Evaluating beta =', 
                  cv.Grid[i, 'Beta'], 
                  'and neighbors =',  
                  cv.Grid[i, 'Neighbors']))
      print(paste('RMSE=', RMSE(crossval$residual)))
    }
  }
  
  ### GET BEST PARAMTER VALUES ###
  idx.min <- which.min(cv.Grid$RMSE)
  best.Beta <- cv.Grid$Beta[idx.min]
  best.Neighbors <- cv.Grid$Neighbors[idx.min]
  min.RMSE <- cv.Grid$RMSE[idx.min]
  
  ### BUILD IDW MODEL BASED ON BEST PARAMTER VALUES ###
  idw.best <- gstat(formula = stat.formula,
                    data = spatialDF, 
                    nmax = best.Neighbors, 
                    set = list(idp = best.Beta))
  
  ### PREPARE EVALUATION GRID ###
  grid.evalGrid  <- expand.grid(x = seq(from = round(extent.evalGrid@xmin,2),
                                        to = round(extent.evalGrid@xmax,2),
                                        by = evalGridSize), 
                                y = seq(from = round(extent.evalGrid@ymin,2),
                                        to = round(extent.evalGrid@ymax,2), 
                                        by = evalGridSize))
  
  
  coordinates(grid.evalGrid) <- ~x + y
  proj4string(grid.evalGrid) <- proj4string(spatialDF)
  gridded(grid.evalGrid) <- TRUE
  
  
  ### INTERPOLATE VALUES FOR EVALUATION GRID USING THE BEST MODEL ###
  idw.best.predict <- predict(object = idw.best,
                              newdata = grid.evalGrid,
                              debug.level = 0)
  
  ### RETURN RESULTS AND OBJECTS ###
  return(list('idwBestModel' = idw.best,
              'idwBestRaster' = idw.best.predict,
              'bestBeta' = best.Beta,
              'bestNeighbors' = best.Neighbors,
              'bestRMSE' = min.RMSE,
              'gridCV' = cv.Grid))
}

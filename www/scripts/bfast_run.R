## modified from https://github.com/rosca002/FAO_Bfast_workshop/tree/master/tutorial

# #################################################################################################################################
############### # load packages
source("www/scripts/load_BFAST_packages.R",echo = TRUE)
library(shiny)
############### # read the data directory from the system argument
options(echo=TRUE)
args <- commandArgs(TRUE)
print(args[1])
data_dir <- args[1]
the_dir <- args[2]

############### # check if the processing text exists, create a new blank processing text file
load(paste0(data_dir,"/my_work_space.RData"))
# the_dir <- readRDS(paste0(data_dir,"/the_dir.rds"))
the_dir <- paste0(data_dir, the_dir, '/')
############### write the consule outputs 
sink(paste0(data_dir,"processing.txt"))
print(paste0('Running time series analysis for: ',basename(the_dir)))
# the_dir <- paste0(data_dir,basename(the_dir),"/")
# Start the clock!
# ptm <- proc.time()
# 
# print(paste0('The process has been running for: ', ptm))

# print('BFAST is processing, make sure you are running an instance with large CPU capacity, such as a c4.4xlarge (12) or c4.8xlarge (13)')
############### check if the time series input data exists
if(file.exists(paste0(the_dir,'/','stack.vrt'))){
  

  # print(paste0('Running BFAST from: ', the_dir))
  output_directory <- paste0(the_dir,"results/")
 
  print(paste0('The results will be found in the folder: ' ,paste0(output_directory)))
  
  dir.create(output_directory, recursive = T,showWarnings = F)
  
  dates          <- unlist(read.csv(paste0(the_dir,'/','dates.csv'),header = FALSE))
  
  data_input_vrt <- paste0(the_dir,'/','stack.vrt')
  the_stack      <- brick(data_input_vrt) 
  tryCatch({
    if(mask == "FNF Mask" ){
      print('Using the Forest/Nonforest mask')
    mask_file_path     <- mask_file_path
    data_input_msk     <- paste0(the_dir,'/','mask_FNF.tif')
    data_input_vrt_nd  <- paste0(the_dir,'/','stack_ND.tif')
    data_input_tif_msk <- paste0(the_dir,'/','stack_FNF.tif')
    
    #################### ALIGN 
    input  <- mask_file_path
    ouput  <- data_input_msk
    mask   <- data_input_vrt
    system(sprintf("gdalwarp -ot UInt16 -co COMPRESS=LZW -t_srs \"%s\" -te %s %s %s %s -tr %s %s %s %s -overwrite",
                   proj4string(raster(mask)),
                   extent(raster(mask))@xmin,
                   extent(raster(mask))@ymin,
                   extent(raster(mask))@xmax,
                   extent(raster(mask))@ymax,
                   res(raster(mask))[1],
                   res(raster(mask))[2],
                   input,
                   ouput
    ))
    
    #################### SET NODATA TO NONE IN THE TIME SERIES STACK
    system(sprintf("gdal_translate -a_nodata none -co COMPRESS=LZW %s %s",
                   data_input_vrt,
                   data_input_vrt_nd
                   ))
    
    
    #################### MULTIPLY THE TIME SERIES STACK BY MASK
    system(sprintf("gdal_calc.py -A %s -B %s --allBands=A --overwrite --co COMPRESS=LZW --outfile=%s --calc=\"%s\"",
                   data_input_vrt_nd,
                   data_input_msk,
                   data_input_tif_msk,
                   paste0("A*B")
    ))
    the_stack      <- brick(data_input_tif_msk)
    }
  }, error=function(e){})
  
  
  results_directory <- file.path(output_directory,paste0("bfast_",title,'/'))
  
  dir.create(results_directory,recursive = T,showWarnings = F)
  
  log_filename <- file.path(results_directory,paste0(format(Sys.time(), "%Y-%m-%d-%H-%M-%S"),"_bfast_", title, ".log"))
  start_time   <- format(Sys.time(), "%Y/%m/%d %H:%M:%S")
  
  tmpres       <- file.path(results_directory, paste0("tmp_bfast_",title, ".tif"))
  result       <- file.path(results_directory, paste0("bfast_",title, ".tif"))
  
  outputfile   <- paste0(results_directory,"bfast_",title,'_threshold.tif')
  
  if(!file.exists(result)){
    if(mode == "Overall"){
      print('Running BFAST this takes some time... if you are busy you can close the window and view the results later, or wait to see the results displayed when the algorthim finishes processing')
      print('If you close this window make sure the process runs by changing the Minimum time frame in the SEPAL user resources to at least 1 hour')
      print('If you have some time sit back, relax and wait for the results to finish processing.')
      time <- system.time(bfmSpatial(the_stack, 
                                     start        = c(monitoring_year_beg[1], 1),
                                     monend       = c(monitoring_year_end[1], 1),
                                     dates        = dates,
                                     formula      = as.Formula(formula),
                                     order        = order, 
                                     history      = history,
                                     filename     = result,
                                     type         = type,
                                     returnLayers = returnLayers,
                                     mc.cores     = detectCores()))
      
      
      #################### SET NODATA TO NONE IN THE TIME SERIES STACK
      # system(sprintf("gdal_translate -a_nodata none -co COMPRESS=LZW %s %s",
      #                tmpres,
      #                result
      # ))
      
      
      write(paste0("This process started on ", start_time," and ended on ",format(Sys.time(),"%Y/%m/%d %H:%M:%S")," for a total time of ", time[[3]]/60," minutes"), log_filename, append=TRUE)
      

      ## Post-processing ####
      # calculate the mean, standard deviation, minimum and maximum of the magnitude band
      # reclass the image into 10 classes
      # 0 = no data
      # 1 = no change (mean +/- 1 standard deviation)
      # 2 = negative small magnitude change      (mean - 2 standard deviations)
      # 3 = negative medium magnitude change     (mean - 3 standard deviations)
      # 4 = negative large magnitude change      (mean - 4 standard deviations)
      # 5 = negative very large magnitude change (mean - 4+ standard deviations)
      # 6 = postive small magnitude change       (mean + 2 standard deviations)
      # 7 = postive medium magnitude change      (mean + 3 standard deviations)
      # 8 = postive large magnitude change       (mean + 4 standard deviations)
      # 9 = postive very large magnitude change  (mean + 4+ standard deviations)
      
      tryCatch({
        means_b2   <- cellStats( raster(result,band=2) , "mean") 
        mins_b2    <- cellStats( raster(result,band=2) , "min")
        maxs_b2    <- cellStats( raster(result,band=2) , "max")
        stdevs_b2  <- cellStats( raster(result,band=2) , "sd")
        system(sprintf("gdal_calc.py -A %s --A_band=2 --co=COMPRESS=LZW --type=Byte --overwrite --outfile=%s --calc=\"%s\"",
                       result,
                       paste0(results_directory,"tmp_bfast_",title,'_threshold.tif'),
                       paste0('(A<=',(maxs_b2),")*",
                              '(A>' ,(means_b2+(stdevs_b2*4)),")*9+",
                              '(A<=',(means_b2+(stdevs_b2*4)),")*",
                              '(A>' ,(means_b2+(stdevs_b2*3)),")*8+",
                              '(A<=',(means_b2+(stdevs_b2*3)),")*",
                              '(A>' ,(means_b2+(stdevs_b2*2)),")*7+",
                              '(A<=',(means_b2+(stdevs_b2*2)),")*",
                              '(A>' ,(means_b2+(stdevs_b2)),")*6+",
                              '(A<=',(means_b2+(stdevs_b2)),")*",
                              '(A>' ,(means_b2-(stdevs_b2)),")*1+",
                              '(A>=',(mins_b2),")*",
                              '(A<' ,(means_b2-(stdevs_b2*4)),")*5+",
                              '(A>=',(means_b2-(stdevs_b2*4)),")*",
                              '(A<' ,(means_b2-(stdevs_b2*3)),")*4+",
                              '(A>=',(means_b2-(stdevs_b2*3)),")*",
                              '(A<' ,(means_b2-(stdevs_b2*2)),")*3+",
                              '(A>=',(means_b2-(stdevs_b2*2)),")*",
                              '(A<' ,(means_b2-(stdevs_b2)),")*2")
        ))
        
      }, error=function(e){})
      
      ####################  CREATE A PSEUDO COLOR TABLE
      cols <- col2rgb(c("white","beige","yellow","orange","red","darkred","palegreen","green2","forestgreen",'darkgreen'))
      pct <- data.frame(cbind(c(0:9),
                              cols[1,],
                              cols[2,],
                              cols[3,]
      ))

      write.table(pct,paste0(results_directory,"color_table.txt"),row.names = F,col.names = F,quote = F)
      

      ################################################################################
      ## Add pseudo color table to result
      system(sprintf("(echo %s) | oft-addpct.py %s %s",
                     paste0(results_directory,"color_table.txt"),
                     paste0(results_directory,"tmp_bfast_",title,'_threshold.tif'),
                     paste0(results_directory,"tmp_colortable.tif")
      ))
      ## Compress final result
      system(sprintf("gdal_translate -ot byte -co COMPRESS=LZW %s %s",
                     paste0(results_directory,"tmp_colortable.tif"),
                     outputfile
      ))
      ## Clean all
      ####################  CREATE A VRT OUTPUT
      
      system(sprintf("gdalbuildvrt %s %s",
                     paste0(data_dir,"/bfast_",title,"_threshold.vrt"),
                     paste0(data_dir,"/*/results/bfast_",title,"/bfast_",title,"_threshold.tif")
      ))
      print(paste0(data_dir,"/bfast_",title,"_threshold.vrt"))
      
      system(sprintf(paste0("rm ",results_directory,"tmp*.tif")))

    }else{##End of OVERALL loop and Beginning of SEQUENTIAL loop
      
      bfmSpatialSq <- function(start, end, timeStack, ...){
        lapply(start:end,
               function(year){
                 tmpoutfl <- paste0(results_directory,"tmp_bfast_",title,"_year",year,'.tif')
                 outfl <- paste0(results_directory,"bfast_",title,"_year",year,'.tif')
                 
                 bfm_year <- bfmSpatial(timeStack, 
                                        start    = c(year, 1), 
                                        monend   = c(year + 1, 1),
                                        dates    = dates,
                                        formula  = as.Formula(formula),
                                        order    = order, 
                                        history  = history,
                                        filename = outfl,
                                        type     = type,
                                        mc.cores = detectCores())
                 
                 #################### SET NODATA TO NONE IN THE TIME SERIES STACK
                 # system(sprintf("gdal_translate -a_nodata none -co COMPRESS=LZW %s %s",
                 #                tmpoutfl,
                 #                outfl
                 # ))
                 
                 outfl
               }
        )
      }
      
      time <- system.time(
        bfmSpatialSq(
          monitoring_year_beg,
          monitoring_year_end,
          the_stack
        ))
      
      ## Post-processing ####
      # output the maximum of the breakpoint dates for all sequential outputs
      numfiles<- length(list.files(results_directory,pattern='.tif'))
      outputfile   <- paste0(results_directory,"bfast_",title,'_breakpoints.tif')
      
      system(sprintf("gdal_calc.py %s --co=COMPRESS=LZW --type=Float32 --overwrite --outfile=%s --calc=\"%s\"",
                     paste(paste0('-',LETTERS[1:numfiles],' ',list.files(results_directory,pattern='.tif',full.names = T), ' --',LETTERS[1:numfiles],'_band=1'),collapse=" "),
                     outputfile,
                     if(LETTERS[numfiles]>3){
                       nummax<- numfiles-2
                       paste(
                         paste(replicate(nummax, "maximum"),'(', collapse = ""),
                         paste('maximum(',LETTERS[1:numfiles][1],',',LETTERS[1:numfiles][2],')'),
                         paste( ',',LETTERS[3:numfiles],')', collapse = "")
                         , collapse = "")
                       
                     }else if(LETTERS[numfiles]==2){
                       print(paste('maximum(',LETTERS[1:numfiles][1],',',LETTERS[1:numfiles][2],')'))
                       
                     }else if(LETTERS[numfiles]==1){
                       print(paste('maximum(',LETTERS[1:numfiles][1],')'))
                       
                     }
                     
      ))
      write(paste0("This process started on ", 
                   start_time," and ended on ",
                   format(Sys.time(),"%Y/%m/%d %H:%M:%S"),
                   " for a total time of ", 
                   time[[3]]/60," minutes"), 
            log_filename, append=TRUE)
      
    }##End of SEQUENTIAL loop
    
  }##End of RESULTS EXIST loop
  print(paste0('The result is ',basename(result)))

  print('Done processing!!! Click on DISPLAY THE RESULTS')
  # Stop the clock
  # proc.time() - ptm
  # print(time1())
  
  # #################### DELETE TMP
  # system(sprintf("rm %s",
  #                paste0(results_directory,"tmp_*")
  # ))
  
  sink()
} ### End of DATA AVAILABLE loop




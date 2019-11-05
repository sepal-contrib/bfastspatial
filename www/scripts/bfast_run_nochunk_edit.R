## modified from https://github.com/rosca002/FAO_Bfast_workshop/tree/master/tutorial

# #################################################################################################################################
############### # load packages

source("www/scripts/load_BFAST_packages.R",echo = TRUE)
chunk_size <- 512

options(echo=TRUE)
args <- commandArgs(TRUE)
print(args[1])

data_dir      <- args[1]
progress_file <- args[2]
res_dir       <- args[3]

load(paste0(res_dir,"/my_work_space.RData"))
overall_start_time <- Sys.time()

chunkerize <- function(infile, outfile, xmin, ymin, xmax, ymax) {
  
  gdalwarp(srcfile=infile, dstfile=outfile,
           t_srs='+proj=longlat +datum=WGS84 +no_defs',
           te=c(xmin, ymin, xmax, ymax), multi=TRUE, 
           output_Raster=TRUE,
           overwrite = TRUE,
           ot="Int16")
}


############### LOOP THROUGH EACH TILE
for(the_dir in tiles){
  print(paste0('BFAST running for ',the_dir))
  
  
  ############### # check if the processing text exists, create a new blank processing text file
  the_path_dir <- paste0(data_dir, the_dir, '/')
  the_path_dir
  
  
  ############### Write the console outputs 
  sink(progress_file)
  print("Preparing data...")
  print(paste0('Running time series analysis for: ',basename(the_path_dir)))
  
  
  ############### Get the list of stacks inside the tile
  main_stack_name <- paste0(the_path_dir,'/','stack.vrt')
  sub_stacks <- list.files(the_path_dir,pattern="_stack.vrt")
  list_stack <- list()
  
  if(length(sub_stacks) > 1){
    list_stack <- paste0(the_path_dir,'/',sub_stacks)
  }else{
    if(file.exists(main_stack_name)){
      list_stack <- main_stack_name}}
  
  list_stack <- paste0(the_path_dir,'/',sub_stacks)
  
  ################# CREATE THE MAIN OUTPUT DIRECTORY
  output_directory <- paste0(res_dir,the_dir,"/")
  dir.create(output_directory, recursive = T,showWarnings = F)
  
  ############### Write the console outputs 
  print(paste0('The results will be found in the folder: ' ,paste0(output_directory)))
  print(paste0('Number of GEE blocks: ',length(sub_stacks)))
  print(paste0('Number of cores: ',detectCores()))
  
  ############### LOOP THROUGH THE DIFFERENT STACKS
  for(i in 1:length(list_stack)){
    
    stack_name <- list_stack[i]
    stack_basename <- substr(basename(stack_name),1,nchar(basename(stack_name))-4)
    
    ############### Write the console output
    print(paste0('  Processing block: ',i,' of ',length(list_stack)))
    
    ################# READ THE DATES FROM THE CSV FILE
    dates          <- unlist(read.csv(paste0(the_path_dir,'/','dates.csv'),header = FALSE))
    
    ################# CREATE LOCAL STACK RESULTS DIRECTORY
    results_directory <- file.path(output_directory,paste0("bfast_stack_",i,'/'))
    
    dir.create(results_directory,recursive = T,showWarnings = F)

    log_filename <- file.path(results_directory,paste0(format(Sys.time(), "%Y-%m-%d-%H-%M-%S"),"_bfast_", title, ".log"))
    start_time   <- format(Sys.time(), "%Y/%m/%d %H:%M:%S")
    nf_start_time <- Sys.time()
    
    ################# MULTIPLY THE INPUT BY THE FNF MASK IF NEEDED
    tryCatch({
      if(mask == "FNF Mask" ){
        print('  Using the Forest/Nonforest mask')
        
        data_input_msk     <- paste0(results_directory,'/','mask_FNF.tif')
        data_input_vrt_nd  <- paste0(results_directory,'/','stack_ND.tif')
        data_input_tif_msk <- paste0(results_directory,'/','stack_FNF.tif')
        
        #################### ALIGN 
        input  <- mask_file_path
        ouput  <- data_input_msk
        mask   <- stack_name
        
        system(sprintf("gdalwarp -ot Int16 -co COMPRESS=LZW -t_srs \"%s\" -te %s %s %s %s -tr %s %s %s %s -overwrite",
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
                       mask,
                       data_input_vrt_nd
        ))
        
        
        #################### MULTIPLY THE TIME SERIES STACK BY MASK
        system(sprintf("gdal_calc.py -A %s -B %s --allBands=A --overwrite --co COMPRESS=LZW --outfile=%s --calc=\"%s\"",
                       data_input_vrt_nd,
                       data_input_msk,
                       data_input_tif_msk,
                       paste0("A*B")
        ))
        stack_name <- data_input_tif_msk
      }
    }, error=function(e){})
    
    
    ############# READ THE STACK METADATA WITHOUT WARNINGS
    info    <- GDALinfo(stack_name,silent = TRUE)
    
    ############# GET STACK SIZE
    stack_x <- as.numeric(info[2])
    stack_y <- as.numeric(info[1])
    
    orig_x <- as.numeric(info[4])
    orig_y <- as.numeric(info[5])
    
    res_x <- as.numeric(info[6])
    res_y <- as.numeric(info[7])
    
    nx <- floor(stack_x / chunk_size)
    ny <- floor(stack_y / chunk_size)
    
    xmin <- orig_x + cumsum(c(0,rep(chunk_size,nx)*res_x))
    ymin <- orig_y + cumsum(c(0,rep(chunk_size,ny)*res_y))
    
    xmax <- orig_x + res_x * stack_x
    ymax <- orig_y + res_y * stack_y

    
    ############# NAME OF RESULT FOR THE TILE
    result       <- bfast_name <- paste0(results_directory,"bfast_st_",i,"_p_",title,".tif")
    
    
    ############# IF RESULT EXISTS, SKIP
    if(!file.exists(result)){
      
      ############# PROCESS IF OVERALL APPROACH CHOSEN
      if(mode == "Overall"){
        
        start_time <- format(Sys.time(), "%Y/%m/%d %H:%M:%S")
        stack      <- brick(stack_name)
        
        # get list of years of raster dates only
        dates_y <- substr(dates,1,4)
        
        ############# cut historical start
        # only if earlier images available than start of historical period
        if(historical_year_beg>as.numeric(dates_y[1])){
          
          # get years to delete before start of historical period
          y_delete_h <- seq(dates_y[1],historical_year_beg-1) 
          
          # return vector of same length as dates_y, 0 if no match, 1 if match
          is_year_h <- match(dates_y,y_delete_h, nomatch=0)
          
          min_ind_h <- which(is_year_h==1)[1] # first match index
          max_ind_h <- tail(which(is_year_h==max(is_year_h)),n=1) # last match index
          
          # drop "gap year" raster layers to be ignored
          stack_year <- dropLayer(stack,c(min_ind_h:max_ind_h))
          print(paste0("   Layers from index ",min_ind_h," to ",max_ind_h," (",dates[min_ind_h]," to ",dates[max_ind_h],") are deleted from the time series stack before the start of the historical period."))
          
          # modify dates accordingly and save as csv
          dates <- dates[is_year_h==0]
          
          } else {
          print("   No layers were deleted before the start of the historical period.")
        } # end if delete until start of historical period
        
        ############# cut historical end 
        if(historical_year_end+1!=monitoring_year_beg){
          
          # get list of years of raster dates only (again as might be changed above)
          dates_y <- substr(dates,1,4)
          
          # get years to delete between end of historical period and start of monitoring period
          y_delete <- seq((historical_year_end+1),monitoring_year_beg-1) 
          
          # return vector of same length as dates_y, 0 if no match, 1 if match
          is_year <- match(dates_y,y_delete, nomatch=0)
          
          min_ind <- which(is_year==1)[1] # first match index
          max_ind <- tail(which(is_year==max(is_year)),n=1) # last match index
          
          # drop "gap year" raster layers to be ignored
          stack_year_2 <- dropLayer(stack,c(min_ind:max_ind))
          print(paste0("   Layers from index ",min_ind," to ",max_ind," (",dates[min_ind]," to ",dates[max_ind],") are deleted from the time series stack between end of historical and start of monitoring period."))
          
          # modify dates accordingly 
          dates_2 <- dates[is_year==0]
          
        } else { # use original time stack and dates
          stack_year_2 <- stack
          dates_2 <- dates
          print("   No layers are deleted from the time series stack between end of historical and start of monitoring period.")
          
        } # end if fixed historical period
        
        # write final dates to csv
        write.table(dates_2, file = paste0(results_directory,"dates_",monitoring_year_beg,"_",monitoring_year_end,".csv"),row.names=FALSE, col.names=FALSE, sep=",") 
        # different name? write original dates file as well?
        
        ############# CREATE A FUNCTION TO IMPLEMENT BFAST
        loop_process <- function(){
          
          cores <- detectCores()
          
          bfasttime <- system.time(bfmSpatial(stack_year_2, 
                                              start        = c(monitoring_year_beg[1], 1),
                                              monend       = c(monitoring_year_end[1], 1),
                                              dates        = dates_2,
                                              formula      = as.Formula(formula),
                                              order        = order, 
                                              history      = history,
                                              filename     = bfast_name,
                                              type         = type,
                                              returnLayers = returnLayers,
                                              mc.cores     = cores))
          
          
          ############# WRITE SUCCESS TO A LOG
          difftime <- difftime(Sys.time(), start_time, units='mins')
          
          write(paste(paste0("End time:",'\t',format(Sys.time(),"%Y/%m/%d %H:%M:%S")),
                      paste0("Bfast time:",'\t',bfasttime[[3]]/60),
                      paste0("Total time:",'\t',difftime),
                      paste0("Dates (original time stack):",'\t',length(dates)),
                      paste0("Dates (after reducing time stack):", '\t',length(dates_2))
                      ,sep="\n"),
                log_filename,
                append=TRUE)
          
          ############# WRITE PERFORMANCE PARAMETERS
          # more details/labels?
          write(paste(basename(data_dir),
                      title,
                      cores,
                      length(dates),
                      stack_x,
                      stack_y,
                      difftime,
                      as.numeric(difftime)/as.numeric(stack_x)/as.numeric(stack_y)/length(dates)*1000*60,
                      sep="\t"),
                paste0(res_dir,"performance.txt"),
                append=TRUE)
          
          bfasttime
        }
        
        tryCatch({
          
          loop_process()
          
          system(sprintf(paste0("rm -f ", results_directory,"tmp_*.tif")))
          
        },error=function(e){
          print(paste0("    Failed stack ",stack_name))
          
          fail_log_filename <- paste0(results_directory,"fail_params_",title, ".log")
          
          write(paste0("Failed stack: ", 
                       stack_name,
                       " Start time: ",start_time,
                       " End time: ",format(Sys.time(),"%Y/%m/%d %H:%M:%S"),
                       " Reason for failure ",
                       e),
                fail_log_filename, 
                append=TRUE)
        })
        
        result <- bfast_name
        
        total_time <- Sys.time()-nf_start_time
        
        print(total_time)
        
        ############# WRITE TIMING INFO TO A LOG
        write(paste0("This process started on ", start_time,
                     " and ended on ",format(Sys.time(),"%Y/%m/%d %H:%M:%S"),
                     "; Total time for the tile: ",total_time, 
                      "; Number of CPUs: ",detectCores()
        ),
        log_filename, 
        append=TRUE)
        
        ############# NAME OF THE THRESHOLDED OUTPUT
        outputfile   <- paste0(results_directory,"bfast_st_",i,"_p_",title,'_threshold.tif')
        
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
          mult_sd <- 1
          means_b2 <- as.numeric(unlist(str_split(gdalinfo(result, stats = T)[grep("STATISTICS_MEAN=",gdalinfo(result))],"="))[4])
          mins_b2 <- as.numeric(unlist(str_split(gdalinfo(result, stats = T)[grep("STATISTICS_MINIMUM=",gdalinfo(result))],"="))[4])
          maxs_b2 <- as.numeric(unlist(str_split(gdalinfo(result, stats = T)[grep("STATISTICS_MAXIMUM=",gdalinfo(result))],"="))[4])
          stdevs_b2 <- as.numeric(unlist(str_split(gdalinfo(result, stats = T)[grep("STATISTICS_STDDEV=",gdalinfo(result))],"="))[4]) 
          stdevs_b2 <- stdevs_b2 * mult_sd
          num_class <-9
          eq.reclass <-   paste0('(A<=',(maxs_b2),")*", '(A>',(means_b2+(stdevs_b2*floor(num_class/2))),")*",num_class,"+" ,
                                 paste( 
                                   " ( A >",(means_b2+(stdevs_b2*1:(floor(num_class/2)-1))),") *",
                                   " ( A <=",(means_b2+(stdevs_b2*2:floor(num_class/2))),") *",
                                   (ceiling(num_class/2)+1):(num_class-1),"+",
                                   collapse = ""), 
                                 '(A<=',(means_b2+(stdevs_b2)),")*",
                                 '(A>', (means_b2-(stdevs_b2)),")*1+",
                                 '(A>=',(mins_b2),")*",
                                 '(A<', (means_b2-(stdevs_b2*4)),")*",ceiling(num_class/2),"+",
                                 paste( 
                                   " ( A <",(means_b2-(stdevs_b2*1:(floor(num_class/2)-1))),") *",
                                   " ( A >=",(means_b2-(stdevs_b2*2:floor(num_class/2))),") *",
                                   2:(ceiling(num_class/2)-1),"+",
                                   collapse = "")
          )
          eq.reclass2 <- as.character(substr(eq.reclass,1,nchar(eq.reclass)-2))
          
          system(sprintf("gdal_calc.py -A %s --A_band=2 --co=COMPRESS=LZW --type=Byte --outfile=%s --calc='%s'
                         ",
                         result,
                         paste0(results_directory,"tmp_bfast_",title,'_threshold.tif'),
                         eq.reclass2
                         
          ))
          
        }, error=function(e){print('something went wrong...')})
        
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
        
        ################################################################################
        ## Compress final result
        system(sprintf("gdal_translate -ot byte -co COMPRESS=LZW %s %s",
                       paste0(results_directory,"tmp_colortable.tif"),
                       outputfile
        ))
        
        ####################  CREATE A VRT OUTPUT
        system(sprintf("gdalbuildvrt %s %s",
                       paste0(res_dir,"/bfast_",basename(data_dir),"_",title,"_threshold.vrt"),
                       paste0(res_dir,
                              "/*/",
                              "bfast_","*","/",
                              "bfast_","*","_threshold.tif")
        ))
        
        system(sprintf(paste0("rm -f ",results_directory,"tmp*.tif")))
        #system(sprintf(paste0("rm -f ", chunks_directory,"tmp*.tif")))
        
      }else{  #################### End of OVERALL loop and Beginning of SEQUENTIAL loop
        
        cores <- detectCores()
        
        bfmSpatialSq <- function(history_start, history_end, monitoring_start, monitoring_end, nyear_seq, timeStack, ...){
          
          ### check whether length of whole monitoring period and sequential blocks come out even --> warning
          y_remainder <- (monitoring_end-monitoring_start+1) %% nyear_seq
          if(y_remainder!=0){
            print(paste0("   ### WARNING: Length of monitoring period and sequential blocks do not come out even. The last ",
                         y_remainder," year(s) of the selected monitoring period will be dropped. ###"))
          }
          
          ### create vector with start years of sequential monitoring periods for lapply
          mon_start_years <- as.integer(seq(from=monitoring_start, to=(monitoring_end-(nyear_seq-1)), by=nyear_seq))
          
          lapply(mon_start_years,
                 function(year){
                   
                   ############# LOOP THROUGH CHUNKS
                   
                   # define output file name
                   if(nyear_seq == 1){
                     outfl <- paste0(results_directory,"bfast_st_",i,"_p_",title,"_year_",year,'.tif') 
                   } else {
                     outfl <- paste0(results_directory,"bfast_st_",i,"_p_",title,"_years_",year,"_",(year+nyear_seq-1),'.tif') 
                   }
                   
                   if(!file.exists(outfl)){
                     
                     start_time   <- format(Sys.time(), "%Y/%m/%d %H:%M:%S")
                     
                     stack_year   <- brick(timeStack)
                     
                     # get list of years of raster dates only
                     dates_y <- substr(dates,1,4)
                     
                     print(paste0("    Processing year:  ",year))
                     
                     # define log file name
                     if(nyear_seq == 1){
                       log_year_filename <- paste0(results_directory,"log_st_",i,"_year_",year,"_params_",title, ".log")
                     } else {
                       log_year_filename <- paste0(results_directory,"log_st_",i,"_years_",year,"_",(year+nyear_seq-1),"_params_",title, ".log")
                     }
                     
                     ############# cut historical start
                     # only if earlier images available than start of historical period
                     if(historical_year_beg>as.numeric(dates_y[1])){
                       
                       # get years to delete before start of historical period
                       y_delete_h <- seq(dates_y[1],historical_year_beg-1) 
                       
                       # return vector of same length as dates_y, 0 if no match, 1 if match
                       is_year_h <- match(dates_y,y_delete_h, nomatch=0)
                       
                       min_ind_h <- which(is_year_h==1)[1] # first match index
                       max_ind_h <- tail(which(is_year_h==max(is_year_h)),n=1) # last match index
                       
                       # drop "gap year" raster layers to be ignored
                       stack_year <- dropLayer(stack_year,c(min_ind_h:max_ind_h))
                       print(paste0("      Layers from index ",min_ind_h," to ",max_ind_h," (",dates[min_ind_h]," to ",dates[max_ind_h],") are deleted from the time series stack before the start of the historical period."))
                       
                       # modify dates accordingly 
                       dates <- dates[is_year_h==0]
                       
                       # create log info
                       str_y_del_1 <- paste0("  Layers from index ",min_ind_h," to ",max_ind_h," (",dates[min_ind_h]," to ",dates[max_ind_h],") were deleted from the time series stack before the start of the historical period.")
                       
                     } else {
                       print("      No layers were deleted before the start of the historical period.")
                       str_y_del_1 <- "  No layers were deleted from the time series stack before the historical period."
                     } # end if delete until start of historical 
                     
                     ############# Modify raster brick for stable monitoring period
                     
                     if(hist_end_fix==TRUE & year!=(historical_year_end+1)){
                       
                       # get list of years of raster dates only (again as it might have been changed above)
                       dates_y <- substr(dates,1,4) 
                       
                       # get years to delete between fixed monitoring period and current monitoring year
                       y_delete <- seq((historical_year_end+1),year-1) # years to delete between fixed monitoring period and current monitoring year
                       
                       # return vector of same length as dates_y, 0 if no match, 1 if match
                       is_year <- match(dates_y,y_delete, nomatch=0)
                       
                       min_ind <- which(is_year==1)[1] # first match index
                       max_ind <- tail(which(is_year==max(is_year)),n=1) # last match index
                       
                       # drop "gap year" raster layers to be ignored
                       stack_year_2 <- dropLayer(stack_year,c(min_ind:max_ind))
                       print(paste0("      Layers from index ",min_ind," to ",max_ind," (",dates[min_ind]," to ",dates[max_ind],") are deleted from the time series stack between historical and this sequential monitoring period."))
                       
                       # modify dates accordingly and save as csv
                       dates_2 <- dates[is_year==0]
                       write.table(dates_2, file = paste0(results_directory,"dates_",year,".csv"),row.names=FALSE, col.names=FALSE, sep=",")
                       
                       # create log info
                       str_y_del_2 <- paste0("  Layers from index ",min_ind," to ",max_ind," (",dates[min_ind]," to ",dates[max_ind],") were deleted from the time series stack between historical and this sequential monitoring period.")
                       
                     } else { # use original time stack and dates
                       stack_year_2 <- stack_year
                       dates_2 <- dates
                       
                       print(paste0("      No layers were deleted from the time series stack between historical and this sequential monitoring period."))
                       
                       # write dates file
                       write.table(dates_2, file = paste0(results_directory,"dates_",year,".csv"),row.names=FALSE, col.names=FALSE, sep=",")
                       
                       # create log info
                       str_y_del_2 <- "  No layers were deleted from the time series stack between historical and this sequential monitoring period."
                       
                     } # end if fixed historical period
                     
                     ############# run bfast sequentially 
                     loop_process <- function(){
                       bfm_year <- bfmSpatial(stack_year_2, 
                                              start    = c(year, 1), 
                                              monend   = c(year + nyear_seq, 1), 
                                              dates    = dates_2,
                                              formula  = as.Formula(formula),
                                              order    = order, 
                                              history  = history,
                                              filename = outfl,
                                              type     = type,
                                              returnLayers = returnLayers,
                                              mc.cores = cores)
                       
                       ############# WRITE THE TIME AND INFO ON MONITORING LENGTH + DELETED YEARS TO A LOG
                       write(paste(paste0("Stack: ",i,"; Year: ",year,"; Length of sequential monitoring period: ",nyear_seq," year(s)"),
                                   paste0(" Start time: ",start_time,"; End time: ",format(Sys.time(),"%Y/%m/%d %H:%M:%S")),
                                   str_y_del_1,
                                   str_y_del_2,
                                   sep="\n"),
                             file=log_year_filename,
                             append=TRUE)
                       
                       bfm_year #??
                       
                     } # end function loop_process()
                     
                     tryCatch({ 
                       
                       loop_process()
                       
                       system(sprintf(paste0("rm -f ", results_directory,"tmp_stack*.tif")))
                       
                     },error=function(e){
                       print(paste0("      Failed process on stack ",i))
                       
                       # define fail log file name
                       if(nyear_seq == 1){
                         fail_log_year_filename <- paste0(results_directory,"fail_stack_",i,"_year_",year,"_params_",title, ".log")
                       } else {
                         fail_log_year_filename <- paste0(results_directory,"fail_stack_",i,"_years_",year,"_",(year+nyear_seq-1),"_params_",title, ".log")
                       }
                       
                       write(paste0("Failed Stack: ",
                                    i,
                                    " Start time: ",start_time,
                                    " End time: ",format(Sys.time(),"%Y/%m/%d %H:%M:%S")),
                             fail_log_year_filename,
                             append=TRUE)
                     })
                     
                   } ### END OF CHUNK EXISTS
                   
                   outfl #??
                   
                 } ### END OF THE YEAR FUNCTION 
                 
          ) ### END OF THE YEAR LAPPLY
          
        } ### END OF THE BFASTSQ FUNCTION
        
        ############# RUN BFAST IN SEQUENTIAL - more parameters might need to be added later
        time <- system.time(
          bfmSpatialSq(
            historical_year_beg,
            historical_year_end,
            monitoring_year_beg,
            monitoring_year_end,
            nyear_seq,
            stack_name
          ))
        
        
        #### Post-processing ####
        print("Post-processing")
        
        # output the maximum of the breakpoint dates for all sequential outputs
        numfiles     <- length(list.files(results_directory,pattern='.tif'))
        outputfile   <- paste0(results_directory,"bfast_",title,'_breakpoints.tif')
        
        system(sprintf("gdal_calc.py %s --co=COMPRESS=LZW --type=Float32 --overwrite --outfile=%s --calc=\"%s\"",
                       paste(paste0('-',LETTERS[1:numfiles],' ',list.files(results_directory,pattern='.tif',full.names = T), ' --',LETTERS[1:numfiles],'_band=1'),collapse=" "),
                       outputfile,
                       if(numfiles>3){ #if(LETTERS[numfiles]>3){ # always TRUE, bug??
                         nummax<- numfiles-2
                         paste(
                           paste(replicate(nummax, "maximum"),'(', collapse = ""),
                           paste('maximum(',LETTERS[1:numfiles][1],',',LETTERS[1:numfiles][2],')'),
                           paste( ',',LETTERS[3:numfiles],')', collapse = "")
                           , collapse = "")
                         
                       }else if(numfiles==2){ #if(LETTERS[numfiles]==2){
                         paste('maximum(',LETTERS[1:numfiles][1],',',LETTERS[1:numfiles][2],')') #print(paste('maximum(',LETTERS[1:numfiles][1],',',LETTERS[1:numfiles][2],')'))
                         
                       }else if(numfiles==1){ #if(LETTERS[numfiles]==1){
                         paste('maximum(',LETTERS[1:numfiles][1],')') #print(paste('maximum(',LETTERS[1:numfiles][1],')'))
                       }
                       
        ))
        write(paste0("This process started on ", 
                     start_time," and ended on ",
                     format(Sys.time(),"%Y/%m/%d %H:%M:%S"),
                     " for a total time of ", 
                     time[[3]]/60," minutes"), 
              log_filename, append=TRUE)
        
      } ## End of SEQUENTIAL loop
      
    } ### End of STACKNAME loop
    
  } ### End of DATA AVAILABLE loop
  
  print(paste0('The result is ',basename(result)))
  
  overall_time <- Sys.time() - overall_start_time
  print(overall_time)
  
  print('Done with processing')
  

} ### END OF TILE LOOP

##########################################################################################
############################################# POST PROCESS ONLY IF OVERALL
##########################################################################################


############# PROCESS IF OVERALL APPROACH CHOSEN
if(mode == "Overall"){
  
  print("Post-processing")
  
  ############### MAKE A LIST OF RESULTS
  list_res <- list.files(res_dir,pattern = glob2rx(paste0("bfast*",title,".tif")),recursive = T)
  
  ####################  CREATE A VRT OUTPUT
  system(sprintf("gdalbuildvrt %s %s",
                 paste0(res_dir,"results_",title,".vrt"),
                 paste0(res_dir,list_res,collapse=' ')
  ))
  
  
  ## Compress final result
  system(sprintf("gdal_translate -co COMPRESS=LZW %s %s",
                 paste0(res_dir,"results_",title,".vrt"),
                 paste0(res_dir,"final_results_",title,".tif")
  ))
  
  ####################  EXTRACT MAGNITUDE
  system(sprintf("gdal_translate -b 2 %s %s",
                 paste0(res_dir,"results_",title,".vrt"),
                 paste0(res_dir,"results_",title,"_magnitude.vrt")
  ))
  
  ####################  COMPUTE  STATS FOR MAGNITUDE
  res   <- paste0(res_dir,"results_",title,"_magnitude.vrt")
  stats <- paste0(res_dir,"stats_",title,".txt")
  
  system(sprintf("gdalinfo -stats %s > %s",
                 res,
                 stats
  ))
  
  s <- readLines(stats)
  maxs_b2   <- as.numeric(unlist(strsplit(s[grepl("STATISTICS_MAXIMUM",s)],"="))[2])
  mins_b2   <- as.numeric(unlist(strsplit(s[grepl("STATISTICS_MINIMUM",s)],"="))[2])
  means_b2  <- as.numeric(unlist(strsplit(s[grepl("STATISTICS_MEAN",s)],"="))[2])
  stdevs_b2 <- as.numeric(unlist(strsplit(s[grepl("STATISTICS_STDDEV",s)],"="))[2])
  
  num_class <-9
  eq.reclass <-   paste0('(A<=',(maxs_b2),")*", '(A>',(means_b2+(stdevs_b2*floor(num_class/2))),")*",num_class,"+" ,
                         paste( 
                           " ( A >",(means_b2+(stdevs_b2*1:(floor(num_class/2)-1))),") *",
                           " ( A <=",(means_b2+(stdevs_b2*2:floor(num_class/2))),") *",
                           (ceiling(num_class/2)+1):(num_class-1),"+",
                           collapse = ""), 
                         '(A<=',(means_b2+(stdevs_b2)),")*",
                         '(A>', (means_b2-(stdevs_b2)),")*1+",
                         '(A>=',(mins_b2),")*",
                         '(A<', (means_b2-(stdevs_b2*4)),")*",ceiling(num_class/2),"+",
                         paste( 
                           " ( A <",(means_b2-(stdevs_b2*1:(floor(num_class/2)-1))),") *",
                           " ( A >=",(means_b2-(stdevs_b2*2:floor(num_class/2))),") *",
                           2:(ceiling(num_class/2)-1),"+",
                           collapse = "")
  )
  eq.reclass2 <- as.character(substr(eq.reclass,1,nchar(eq.reclass)-2))
  
  ####################  COMPUTE THRESHOLDS LAYER
  system(sprintf("gdal_calc.py -A %s --co=COMPRESS=LZW --type=Byte --overwrite --outfile=%s --calc=\"%s\"",
                 res,
                 paste0(res_dir,"tmp_results_",title,"_magnitude.tif"),
                 eq.reclass2
                 
  ))     
  
  ####################  CREATE A PSEUDO COLOR TABLE
  cols <- col2rgb(c("black","beige","yellow","orange","red","darkred","palegreen","green2","forestgreen",'darkgreen'))
  pct <- data.frame(cbind(c(0:9),
                          cols[1,],
                          cols[2,],
                          cols[3,]
  ))
  
  write.table(pct,paste0(rootdir,'bfast_results/color_table.txt'),row.names = F,col.names = F,quote = F)
  
  ################################################################################
  ## Add pseudo color table to result
  system(sprintf("(echo %s) | oft-addpct.py %s %s",
                 paste0(rootdir,'bfast_results/color_table.txt'),
                 paste0(res_dir,"tmp_results_",title,"_magnitude.tif"),
                 paste0(res_dir,"tmp_results_",title,"_magnitude_pct.tif")
  ))
  
  ## Compress final result
  system(sprintf("gdal_translate -ot Byte -co COMPRESS=LZW %s %s",
                 paste0(res_dir,"tmp_results_",title,"_magnitude_pct.tif"),
                 paste0(res_dir,"results_",title,"_threshold.tif")
  ))
  
  system(sprintf("rm -r -f %s",
                 paste0(res_dir,"tmp*.tif"))
         )
  
  overall_time <- Sys.time() - overall_start_time
  print(overall_time)
  
  print("Done with post-processing, you can click on display")

}else{
  overall_time <- Sys.time() - overall_start_time
  print(overall_time)
}

sink()
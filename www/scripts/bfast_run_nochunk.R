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
        
        ############# CREATE A FUNCTION TO IMPLEMENT BFAST
        loop_process <- function(){
          
          cores <- detectCores()
          
          bfasttime <- system.time(bfmSpatial(stack, 
                                              start        = c(monitoring_year_beg[1], 1),
                                              monend       = c(monitoring_year_end[1], 365),
                                              dates        = dates,
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
                      paste0("Dates:",'\t',length(dates))
                      ,sep="\n"),
                log_filename,
                append=TRUE)
          
          ############# WRITE PERFORMANCE PARAMETERS
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
          #print(paste0("    Processing  "))
          
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
        
        #print(paste0("    Finished  ",chunk))
        
        result <- bfast_name
        
        total_time <- Sys.time()-nf_start_time
        
        print(total_time)
        
        ############# WRITE TIMING INFO TO A LOG
        write(paste0("This process started on ", start_time,
                     " and ended on ",format(Sys.time(),"%Y/%m/%d %H:%M:%S"),
                     " Total time for the tile: ",total_time, 
                      " Number of CPUs: ",detectCores()
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
        
        bfmSpatialSq <- function(start, end, timeStack, ...){
          lapply(start:end,
                 function(year){
                   
                   ############# LOOP THROUGH CHUNKS
                   outfl <- paste0(results_directory,"bfast_st_",i,"_p_",title,"_year",year,'.tif')
                   

                   if(!file.exists(outfl)){

                     start_time   <- format(Sys.time(), "%Y/%m/%d %H:%M:%S")
                     
                     stack_year      <- brick(timeStack)
                     
                     print(paste0("    Processing year:  ",year))
                     
                     log_year_filename <- paste0(results_directory,"log_st_",i,"_year_",year,"_params_",title, ".log")
                     
                     loop_process <- function(){bfm_year <- bfmSpatial(stack_year, 
                                                                       start    = c(year, 1), 
                                                                       monend   = c(year + 1, 1),
                                                                       dates    = dates,
                                                                       formula  = as.Formula(formula),
                                                                       order    = order, 
                                                                       history  = history,
                                                                       filename = outfl,
                                                                       type     = type,
                                                                       mc.cores = cores)
                     
                     ############# WRITE THE TIME TO A LOG
                     write(paste0("Stack: ",
                                  i,
                                  " Start time: ",start_time,
                                  " End time: ",format(Sys.time(),"%Y/%m/%d %H:%M:%S")
                     ),
                     log_year_filename,
                     append=TRUE)
                     
                     bfm_year
                     
                     }
                     
                     tryCatch({ 
                       
                       loop_process()
                       
                       system(sprintf(paste0("rm -f ", results_directory,"tmp_stack*.tif")))
                       
                     },error=function(e){
                       print(paste0("      Failed process on stack ",i))
                       
                       fail_log_year_filename <- paste0(results_directory,"fail_stack_",i,"_year_",year,"_params_",title, ".log")
                       
                       write(paste0("Failed Stack: ",
                                    i,
                                    " Start time: ",start_time,
                                    " End time: ",format(Sys.time(),"%Y/%m/%d %H:%M:%S")),
                             fail_log_year_filename,
                             append=TRUE)
                     })
                     
                   } ### END OF CHUNK EXISTS
                   

                   outfl
                 } ### END OF THE YEAR FUNCTION 
                 
          ) ### END OF THE YEAR LAPPLY
          
        } ### END OF THE BFASTSQ FUNCTION
        
        ############# RUN BFAST IN SEQUENTIAL
        time <- system.time(
          bfmSpatialSq(
            monitoring_year_beg,
            monitoring_year_end,
            stack_name
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


print("Post-processing")
############# PROCESS IF OVERALL APPROACH CHOSEN
if(mode == "Overall"){
  
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
  

}else{}

overall_time <- Sys.time() - overall_start_time
print(overall_time)

print("Done with post-processing, you can click on display")
sink()

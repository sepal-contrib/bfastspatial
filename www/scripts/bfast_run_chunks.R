## modified from https://github.com/rosca002/FAO_Bfast_workshop/tree/master/tutorial

# #################################################################################################################################
############### # load packages

source("www/scripts/load_BFAST_packages.R",echo = TRUE)

options(echo=TRUE)
args <- commandArgs(TRUE)
print(args[1])
data_dir <- args[1]

load(paste0(data_dir,"/my_work_space.RData"))

for(the_dir in tiles){
  print(paste0('BFAST running for ',the_dir))
  
  ############### # check if the processing text exists, create a new blank processing text file
  the_path_dir <- paste0(data_dir, the_dir, '/')
  the_path_dir
  
  
  ############### Get the list of stacks
  main_stack_name <- paste0(the_path_dir,'/','stack.vrt')
  sub_stacks <- list.files(the_path_dir,pattern="_stack.vrt")
  list_stack <- list()
  
  if(length(sub_stacks) > 0){
    list_stack <- paste0(the_path_dir,'/',sub_stacks)
  }else{
    if(file.exists(main_stack_name)){
      list_stack <- main_stack_name}}
  
  ################# CREATE THE MAIN OUTPUT DIRECTORY
  output_directory <- paste0(the_path_dir,"results/")
  dir.create(output_directory, recursive = T,showWarnings = F)
  
  ############### Write the console outputs 
  sink(progress_file)
  
  print("Preparing data...")
  print(paste0('The results will be found in the folder: ' ,paste0(output_directory)))
  print(paste0('Running time series analysis for: ',basename(the_path_dir)))
  
  ############### LOOP THROUGH THE DIFFERENT STACKS
  for(stack_name in list_stack){
    stack_name <- list_stack[1]
    stack_basename <- substr(basename(stack_name),1,nchar(basename(stack_name))-4)
    
    
    ################# READ THE DATES FROM THE CSV FILE
    dates          <- unlist(read.csv(paste0(the_path_dir,'/','dates.csv'),header = FALSE))
    
    ################# CREATE LOCAL STACK RESULTS DIRECTORY
    results_directory <- file.path(output_directory,paste0("bfast_",
                                                           stack_basename,"_",title,'/'))
    dir.create(results_directory,recursive = T,showWarnings = F)
    
    chunks_directory <- file.path(results_directory,paste0("chunks",'/'))
    dir.create(chunks_directory,recursive = T,showWarnings = F)
    
    log_filename <- file.path(results_directory,paste0(format(Sys.time(), "%Y-%m-%d-%H-%M-%S"),"_bfast_", title, ".log"))
    start_time   <- format(Sys.time(), "%Y/%m/%d %H:%M:%S")
    
    
    tryCatch({
      if(mask == "FNF Mask" ){
        print('Using the Forest/Nonforest mask')
        mask_file_path     <- mask_file_path
        data_input_msk     <- paste0(the_path_dir,'/','mask_FNF.tif')
        data_input_vrt_nd  <- paste0(the_path_dir,'/','stack_ND.tif')
        data_input_tif_msk <- paste0(the_path_dir,'/','stack_FNF.tif')
        
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
        stack_name <- data_input_tif_msk
      }
    }, error=function(e){})
    
    ############# READ THE STACK METADATA WITHOUT WARNINGS
    info    <- GDALinfo(stack_name,silent = TRUE)
    
    ############# GET STACK SIZE
    stack_x <- as.numeric(info[2])
    stack_y <- as.numeric(info[1])
    
    nx <- floor(stack_x / chunk_size)
    ny <- floor(stack_y / chunk_size)
    
    sizes_x <- c(rep(chunk_size,nx),stack_x - nx*chunk_size)
    sizes_y <- c(rep(chunk_size,ny),stack_y - ny*chunk_size)
    
    start_x <- cumsum(c(0,rep(chunk_size,nx)))
    start_y <- cumsum(c(0,rep(chunk_size,ny)))
    
    ############# CALCULATE CHUNKS SIZES
    sizes   <- cbind(expand.grid(sizes_x,sizes_y),
                     expand.grid(start_x,start_y))
    
    names(sizes) <- c("size_x","size_y","start_x","start_y")
    
    ############# NAME OF OVERALL RESULT FROM THE TILE
    result       <- file.path(results_directory, paste0("bfast_",title, ".tif"))
    
    if(!file.exists(result)){
      if(mode == "Overall"){
        for(chunk in 1:nrow(sizes)){
          
          chunk_stack_name <- paste0(chunks_directory,"tmp_chunk_",chunk,"_stack.tif") 
          chunk_bfast_name <- paste0(chunks_directory,"chunk_",chunk,"_bfast_",title, ".tif")
          
          if(!file.exists(chunk_bfast_name)){
            chunk_start_time   <- format(Sys.time(), "%Y/%m/%d %H:%M:%S")
            
            print(paste0("Processed : ",ceiling((chunk-1)/nrow(sizes)*100),"%"))
            
            ############# CREATE THE CHUNK
            system(sprintf("gdal_translate -srcwin %s %s %s %s -co COMPRESS=LZW %s %s",
                           sizes[chunk,"start_x"],
                           sizes[chunk,"start_y"],
                           sizes[chunk,"size_x"],
                           sizes[chunk,"size_y"],
                           stack_name,
                           chunk_stack_name))

            chunk_stack      <- brick(chunk_stack_name)
            
            system(sprintf("rm -f %s",chunk_bfast_name))
            
            chunk_log_filename <- paste0(chunks_directory,"log_chunk_",chunk,"_params_",title, ".log")
            
            Sys.sleep(1)
            
            ############# CREATE A FUNCTION TO IMPLEMENT BFAST
            loop_process <- function(){
              chunktime <- system.time(bfmSpatial(chunk_stack, 
                                                  start        = c(monitoring_year_beg[1], 1),
                                                  monend       = c(monitoring_year_end[1], 1),
                                                  dates        = dates,
                                                  formula      = as.Formula(formula),
                                                  order        = order, 
                                                  history      = history,
                                                  filename     = chunk_bfast_name,
                                                  type         = type,
                                                  returnLayers = returnLayers,
                                                  mc.cores     = detectCores()))
              
              ############# WRITE THE TIME TO A LOG
              write(paste0("Chunk: ",
                           chunk,
                           " Start time: ",chunk_start_time,
                           " End time: ",format(Sys.time(),"%Y/%m/%d %H:%M:%S"),
                           " for a total time of ", chunktime[[3]]/60," minutes"),
                    chunk_log_filename,
                    append=TRUE)
              
              chunktime
            }
            
            tryCatch({
              print(paste0("Processing chunk ",chunk))
              
              loop_process()
              
              system(sprintf(paste0("rm -f", chunks_directory,"tmp_chunk*.tif")))
              
            },error=function(e){
              print(paste0("Still processing chunk ",chunk))
              
              fail_log_filename <- paste0(chunks_directory,"fail_chunk_",chunk,"_params_",title, ".log")
              
              write(paste0("Failed Chunk: ", 
                           chunk,
                           " Start time: ",chunk_start_time,
                           " End time: ",format(Sys.time(),"%Y/%m/%d %H:%M:%S")),
                    fail_log_filename, 
                    append=TRUE)
             })
            
            }# END OF TEST EXISTS CHUNK
        }# END OF THE CHUNK LOOP
        
        ############# COMBINE ALL THE CHUNKS
        system(sprintf("gdal_merge.py -co COMPRESS=LZW -o %s %s",
                       result,
                       paste0(chunks_directory, paste0("chunk_*","_bfast_",title, ".tif"))
        ))
        
        ############# WRITE TIMING INFO TO A LOG
        write(paste0("This process started on ", start_time,
                     " and ended on ",format(Sys.time(),"%Y/%m/%d %H:%M:%S"),
                     " Number of CPUs: ",detectCores(),
                     " Number of chunks: ",nrow(sizes)),
              log_filename, 
              append=TRUE)
        
        ############# NAME OF THE THRESHOLDED OUTPUT
        outputfile   <- paste0(results_directory,"bfast_",title,'_threshold.tif')
        
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
                       paste0(data_dir,
                              "/*/results/",
                              "bfast_","*",title,"/",
                              "bfast_","*",title,"_threshold.tif")
        ))
        
        system(sprintf(paste0("rm -f ",results_directory,"tmp*.tif")))
        system(sprintf(paste0("rm -f ", chunks_directory,"tmp*.tif")))
        
      }else{##End of OVERALL loop and Beginning of SEQUENTIAL loop
        
        bfmSpatialSq <- function(start, end, timeStack, ...){
          lapply(start:end,
                 function(year){
                   
                   outfl <- paste0(results_directory,"bfast_",title,"_year",year,'.tif')
                   
                   for(chunk in 1:nrow(sizes)){
                     
                     chunk_stack_year_name <- paste0(chunks_directory,"tmp_chunk_",chunk,"_year",year,"_stack.tif") 
                     chunk_bfast_year_name <- paste0(chunks_directory,"chunk_",chunk,"_year",year,"_bfast_",title, ".tif")
                     
                     if(!file.exists(chunk_bfast_year_name)){
                       chunk_start_time   <- format(Sys.time(), "%Y/%m/%d %H:%M:%S")
                       
                       
                       system(sprintf("gdal_translate -srcwin %s %s %s %s %s %s",
                                      sizes[chunk,"start_x"],
                                      sizes[chunk,"start_y"],
                                      sizes[chunk,"size_x"],
                                      sizes[chunk,"size_y"],
                                      timeStack,
                                      chunk_stack_year_name))
                       
                       chunk_stack_year      <- brick(chunk_stack_year_name)
                       
                       print(paste0("Processing : ",ceiling(chunk/nrow(sizes))*100,"%"," for year:  ",year))
                       system(sprintf("rm -f %s",chunk_bfast_year_name))
                       
                       Sys.sleep(1)
                       
                       chunk_log_year_filename <- paste0(chunks_directory,"log_chunk_",chunk,"_year_",year,"_params_",title, ".log")
                       
                       loop_process <- function(){bfm_year <- bfmSpatial(chunk_stack_year, 
                                                                         start    = c(year, 1), 
                                                                         monend   = c(year + 1, 1),
                                                                         dates    = dates,
                                                                         formula  = as.Formula(formula),
                                                                         order    = order, 
                                                                         history  = history,
                                                                         filename = chunk_bfast_year_name,
                                                                         type     = type,
                                                                         mc.cores = detectCores())
                       
                       # write(paste0("Chunk: ", 
                       #              chunk,
                       #              " Start time: ",chunk_start_time,
                       #              " End time: ",format(Sys.time(),"%Y/%m/%d %H:%M:%S"),
                       #              " for a total time of ", chunktime[[3]]/60," minutes"),
                       #       chunk_log_year_filename, 
                       #       append=TRUE)
                       
                       system(sprintf(paste0("rm -f", chunks_directory,"tmp_chunk*.tif")))
                       
                       bfm_year
                       
                       }
                       
                       tryCatch({ 
                         print(paste0("Processing chunk ",chunk))
                         loop_process()
                         
                         
                       },error=function(e){
                         print(paste0("Failed process on chunk ",chunk))

                         fail_log_year_filename <- paste0(chunks_directory,"fail_chunk_",chunk,"_year_",year,"_params_",title, ".log")

                         write(paste0("Failed Chunk: ",
                                      chunk,
                                      " Start time: ",chunk_start_time,
                                      " End time: ",format(Sys.time(),"%Y/%m/%d %H:%M:%S")),
                               fail_log_year_filename,
                               append=TRUE)
                         })
                       
                     } ### END OF CHUNK EXISTS
                     
                   } ### END OF THE CHUNK LOOP

                   ############# COMBINE ALL THE CHUNKS
                   system(sprintf("gdal_merge.py -co COMPRESS=LZW -o %s %s",
                                  outfl,
                                  paste0(chunks_directory,"chunk_*","_year",year,"_bfast_",title, ".tif")
                   ))
                   
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
        
      } ##End of SEQUENTIAL loop
      
    } ##End of STACKNAME loop
    
    print(paste0('The result is ',basename(result)))
    print('Done processing. Click on DISPLAY THE RESULTS')

    sink()
  } ### End of DATA AVAILABLE loop
  
  
} ### END OF TILE LOOP



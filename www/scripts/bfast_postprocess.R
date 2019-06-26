## modified from https://github.com/rosca002/FAO_Bfast_workshop/tree/master/tutorial

# #################################################################################################################################
############### # load packages
rootdir  <- paste0(path.expand("~"),"/")

source(paste0(rootdir,"/bfastspatial/www/scripts/load_BFAST_packages.R"),echo = TRUE)

options(echo=TRUE)
args <- commandArgs(TRUE)
print(args[1])
data_dir <- args[1]
#data_dir <- "/home/dannunzio/downloads/gcfrp_2000_2019_NDMI/"

load(paste0(data_dir,"/my_work_space.RData"))


username <- unlist(strsplit(rootdir,"/"))[3]

sink(progress_file)
print("create directory for results")

list_res <- list.files(data_dir,pattern = glob2rx(paste0("bfast*",title,".tif")),recursive = T)
bfst_dir <- paste0(rootdir,'bfast_results/',basename(data_dir),"_",username,"_PARAM_",title,"/")
bfst_tiles_dir <- paste0(bfst_dir,"tiles/")

dir.create(bfst_tiles_dir,recursive = T,showWarnings = F)

for(file in list_res){
  base <- strsplit(file,split = "/")[[1]][3]
  tile <- strsplit(file,split = "/")[[1]][1]
  file.copy(paste0(data_dir,file),paste0(bfst_tiles_dir,username,"_tile_",tile,"_",base,".tif"))
}


####################  CREATE A VRT OUTPUT
system(sprintf("gdalbuildvrt %s %s",
               paste0(bfst_dir,"results_",title,".vrt"),
               paste0(bfst_tiles_dir,"*.tif")
))

####################  EXTRACT MAGNITUDE
system(sprintf("gdal_translate -b 2 %s %s",
               paste0(bfst_dir,"results_",title,".vrt"),
               paste0(bfst_dir,"results_",title,"_magnitude.vrt")
               ))

####################  COMPUTE  STATS FOR MAGNITUDE
res   <- paste0(bfst_dir,"results_",title,"_magnitude.vrt")
stats <- paste0(bfst_dir,"stats_",title,".txt")

system(sprintf("gdalinfo -stats %s > %s",
               res,
               stats
))

s <- readLines(stats)
maxs_b2   <- as.numeric(unlist(strsplit(s[grepl("STATISTICS_MAXIMUM",s)],"="))[2])
mins_b2   <- as.numeric(unlist(strsplit(s[grepl("STATISTICS_MINIMUM",s)],"="))[2])
means_b2  <- as.numeric(unlist(strsplit(s[grepl("STATISTICS_MEAN",s)],"="))[2])
stdevs_b2 <- as.numeric(unlist(strsplit(s[grepl("STATISTICS_STDDEV",s)],"="))[2])

####################  COMPUTE THRESHOLDS LAYER
system(sprintf("gdal_calc.py -A %s --co=COMPRESS=LZW --type=Byte --overwrite --outfile=%s --calc=\"%s\"",
               res,
               paste0(bfst_dir,"tmp_results_",title,"_magnitude.tif"),
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
               paste0(bfst_dir,"tmp_results_",title,"_magnitude.tif"),
               paste0(bfst_dir,"tmp_results_",title,"_magnitude_pct.tif")
))

## Compress final result
system(sprintf("gdal_translate -ot Byte -co COMPRESS=LZW %s %s",
               paste0(bfst_dir,"tmp_results_",title,"_magnitude_pct.tif"),
               paste0(bfst_dir,"results_",title,"_threshold.tif")
))

system(sprintf("rm -r -f %s",
               paste0(bfst_dir,"tmp*.tif")))

sink()
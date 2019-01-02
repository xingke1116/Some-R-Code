
## change the working directory to the folder saves all the raw files
setwd("C:/WD/Model")

## make a list of all csv files in the folder
filelist <- list.files(pattern="*.csv")

## read all files and assign their file name as dataframe name
## option 1: the R way
#  set header = T or F depend on your data
mydata <- lapply(filelist,function(i){read.csv(i, header = F)})
names(mydata) <- gsub("\\.csv$","",filelist)

## option 2: for loop
# ##----- ctrl+shift+c if need to use
#  for (i in 1:length(filelist)){
#    filename <- filelist[i]
#     assign(paste(filename), read.csv(filelist[i], header = F))}
# ##----- end


## combining all the dataframes read into the list earlier
## use rbind.data.frame() instead if not 100% sure columns are the same from each dataframe
onebigfile <- rbind_list(mydata)
# #----- slower rbind function with column check
# onebigfile <- rbind.data.frame(mydata$`_1`,mydata$`_666001`)
# #----- end

## assign column names
colnames(onebigfile) <- c("Trans_ID","Time","Quantity","Net_Sales","Discount",
                           "Style_No","Dept_No","Class_No","Subclass_No")

## output your combinded file to csv
## make sure save to local drive!! otherwise will be super slow
write.csv(onebigfile,"C:/WD/Model/onefile.csv", row.names = F)




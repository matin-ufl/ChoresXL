setwd("~/../../Volumes/SHARE/ARRC/Active_Studies/CHORES-XL_087-2013/Participant Data/")

l <- dir("../Participant Data/")

uniqueDeviceIDs <- data.frame(matrix(ncol = 1, nrow = 0))
colnames(uniqueDeviceIDs) <- c("Device.ID")

for (i in 1:length(l)) {
     print(paste("(", i, " out of ", length(l), ") ", l[i], " >>> being checked.", sep = ""))
     # Checking only folders - participants data
     if(dir.exists(l[i])) {
          # Some folders should not be considered: {__rzi_0.401, _Cosmed Data..., 00 should not...}
          if(!grepl("^_|^00", l[i])) {
               participantFolder <- paste(l[i], "/", sep = "")
               for(visit in c("V1/csv/", "V2/csv/", "V3/csv/", "V4/csv/", "VH/csv/")) {
                    csvFolder <- paste(participantFolder, visit, sep = "")
                    if(dir.exists(csvFolder)) { # if the particular visit exists and the csv file is created
                         files <- dir(csvFolder)
                         for(j in 1:length(files)) {
                              file <- paste(csvFolder, files[j], sep = "")
                              temp <- tryCatch({
                                   read.csv(file = file, nrows = 10)
                              },
                              error = function(cond){
                                   print(paste("File (", file, ") could not be read"))
                                   return(NA)
                              },
                              finally = {
                              })
                              # Check if the file contained some data
                              if(!is.na(temp)){ 
                                   tempID <- as.character(temp[1, 1])
                                   str <- unlist(strsplit(tempID, split = ": "))
                                   if(length(str) > 1) {
                                        deviceID <- str[2]
                                        # Check if the device is already added
                                        if(length(which(uniqueDeviceIDs$Device.ID == deviceID)) == 0) {
                                             print(paste("Device ID (", deviceID, ") is added. Total devices: ", nrow(uniqueDeviceIDs), sep = ""))
                                             uniqueDeviceIDs <- rbind(uniqueDeviceIDs, data.frame(Device.ID = deviceID))
                                        }
                                   }
                              }
                         }
                    }
               }
          }
     }
}

write.csv(file = "device_ids_062716.csv", uniqueDeviceIDs, row.names = F)

setwd("~/Workspaces/R workspace/ChoresXL/toolboxes/")

# Functions ------------------------------------------------

check.one.participant.folder <- function(chores.directory, participant) {
     # Initializing the result
     ppt.report <- data.frame(matrix(nrow = 1, ncol = 53))
     ppt.report[1, ] <- c(participant, rep("MISSING", 51), "OK")
     gt3x.postfix <- paste("GT3X", c("ankle", "hip", "thigh", "upper_arm", "wrist"), sep = ".", collapse = ",")
     csv.postfix <- paste("CSV", c("ankle", "hip", "thigh", "upper_arm", "wrist"), sep = ".", collapse = ",")
     
     v1.postFix <- gsub(pattern = " ", replacement = ",",
                        x = paste(paste("V1", unlist(strsplit(gt3x.postfix, split = ",")), sep = ".", collapse = ","),
                                  paste("V1", unlist(strsplit(csv.postfix, split = ",")), sep = ".", collapse = ","),
                                  collapse = ","))
     
     v2.postFix <- gsub(pattern = " ", replacement = ",",
                        x = paste(paste("V2", unlist(strsplit(gt3x.postfix, split = ",")), sep = ".", collapse = ","),
                                  paste("V2", unlist(strsplit(csv.postfix, split = ",")), sep = ".", collapse = ","),
                                  collapse = ","))
     
     v3.postFix <- gsub(pattern = " ", replacement = ",",
                        x = paste(paste("V3", unlist(strsplit(gt3x.postfix, split = ",")), sep = ".", collapse = ","),
                                  paste("V3", unlist(strsplit(csv.postfix, split = ",")), sep = ".", collapse = ","),
                                  collapse = ","))
     
     v4.postFix <- gsub(pattern = " ", replacement = ",",
                        x = paste(paste("V4", unlist(strsplit(gt3x.postfix, split = ",")), sep = ".", collapse = ","),
                                  paste("V4", unlist(strsplit(csv.postfix, split = ",")), sep = ".", collapse = ","),
                                  collapse = ","))
     
     vh.postFix <- gsub(pattern = " ", replacement = ",",
                        x = paste(paste("VH", unlist(strsplit(gt3x.postfix, split = ",")), sep = ".", collapse = ","),
                                  paste("VH", unlist(strsplit(csv.postfix, split = ",")), sep = ".", collapse = ","),
                                  collapse = ","))
     colnames(ppt.report) <- c("ID", "TASK TIMES",
                               unlist(strsplit(v1.postFix, split = ",")),
                               unlist(strsplit(v2.postFix, split = ",")),
                               unlist(strsplit(v3.postFix, split = ",")),
                               unlist(strsplit(v4.postFix, split = ",")),
                               unlist(strsplit(vh.postFix, split = ",")),
                               "status")
     rm(csv.postfix, gt3x.postfix, v1.postFix, v2.postFix, v3.postFix, v4.postFix, vh.postFix)
     
     # Checking the directories and files
     d <- dir(paste(chores.directory, participant, sep = ""))
     
     taskTime.flag <- "MISSING"
     for(i in 1:length(d)) {
          if(substr(d[i], start = 1, stop = 9) == "tasktimes") {
               ppt.report$`TASK TIMES` <- "OK"
               break
          }
     }
     rm(i)
     for(visit in c("V1", "V2", "V3", "V4", "VH")) {
          if(dir.exists(paste(chores.directory, participant, "/", visit, sep = ""))) {
               visitFolder <- paste(chores.directory, participant, "/", visit, sep = "")  
               d <- dir(visitFolder, "/*.gt3x")
               for(fileName in d) {
                    bodyPlacement <- unlist(strsplit(fileName, split = "-"))[2]
                    for(part in c("ankle", "hip", "thigh", "upper_arm", "wrist")) {
                         if(tolower(bodyPlacement) == paste(part, ".gt3x", sep = "")) {
                              ppt.report[paste(visit, "GT3X", part, sep = ".")] <- "OK"
                              break
                         }
                    }
               }
               if(dir.exists(paste(visitFolder, "/csv", sep = ""))) {
                    visitFolderCSV <- paste(visitFolder, "/csv", sep = "")
                    csv.d <- dir(visitFolderCSV, "/*.csv")
                    for(fileName in csv.d) {
                         bodyPlacement <- unlist(strsplit(fileName, split = "-"))[2]
                         for(part in c("ankle", "hip", "thigh", "upper_arm", "wrist")) {
                              if(tolower(substr(bodyPlacement, start = 1, stop = nchar(part))) == part) {
                                   ppt.report[paste(visit, "CSV", part, sep = ".")] <- "OK"
                                   break
                              }
                         }
                    }
               }
          }
     }
     for(j in 2:(ncol(ppt.report)-1)) {
          if(ppt.report[j] != "OK") {
               ppt.report$status <- "INCOMPLETE"
               break
          }
     }
     
     ppt.report
}

# Script ---------------------------
chores.directory <- "~/../../Volumes/SHARE/ARRC/Active_Studies/CHORES-XL_087-2013/Participant Data/"
report <- data.frame(matrix(nrow = 0, ncol = 53))
l <- dir(chores.directory)
for(i in 3:length(l)) {
     print(paste("Checking ", l[i], " - (", i, " out of ", (length(l)-2), ")", sep = ""))
     if(dir.exists(paste(chores.directory, l[i], sep = ""))) {
          ppt.report <- check.one.participant.folder(chores.directory = chores.directory, participant = l[i])
          report <- rbind(report, ppt.report)
          colnames(report) <- colnames(ppt.report)
     }
}

rm(chores.directory, l, i, ppt.report)



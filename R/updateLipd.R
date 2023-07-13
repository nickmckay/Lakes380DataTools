#' Update a LiPD file from a formatted Lakes 380 spreadsheet
#'
#' @param L a LiPD object
#' @param path path to excel fil
#' @import readxl purrr glue
#'
#' @export
updateLipdFromSpreadsheet <- function(L,path){
  updated <- FALSE
  Lo <- L
  sn <- readxl::excel_sheets(path)
  sh <- purrr::map(sn,\(x) readxl::read_excel(path = path,sheet = x))
  names(sh) <- sn

  #go through paleoTables first
  for(p in 1:length(L$paleoData)){
    for(m in 1:length(L$paleoData[[p]]$measurementTable)){
      thisTable <- L$paleoData[[p]]$measurementTable[[m]]
      #match to the tableId
      print(glue::glue("Checking table {thisTable$tableId}..."))

      ws <- which(startsWith(thisTable$tableId,prefix = sn))
      if(length(ws) == 1){
        thisSheet <- sh[[ws]]

        #get names without units
        excelVarNames <- purrr::map_chr(stringr::str_split(names(thisSheet),pattern = " \\("),1)

        if(any(duplicated(excelVarNames))){
          stop("It looks like there are duplicated column names in the excel spreadsheet.")
        }

      }else{
        stop("couldn't match the paleo measurement table")
      }

      #now loop through the LiPD columns, and compare to the table, update as needed.
      toloop <- which(purrr::map_lgl(thisTable,is.list))

      for(v in toloop){
        thisColumn <- thisTable[[v]]

        #find corresponding spreadsheet column
        coli <- which(excelVarNames == thisColumn$variableName)
        if(length(coli) == 0){
          print("no match")
        }


        #compare values
        if(any(thisSheet[[coli]] != thisColumn$values)){
          updated <- TRUE
          print(glue::glue("Updating values for {thisColumn$variableName}..."))
          thisColumn$values <- as.numeric(thisSheet[[coli]])
          L$paleoData[[p]]$measurementTable[[m]][[v]] <- thisColumn
        }
      }

      #check values lengths
      allLengths <- purrr::map_dbl(thisTable[toloop],\(x) length(x$values))
      if(length(unique(allLengths)) > 1){
        stop("Uh oh, not all the columns are the same length after updating. This is bad.")
      }


    }
  }



  #next through chronSummary tables
  for(c in 1:length(L$chronData)){
    for(m in 1:length(L$chronData[[c]]$model)){
      for(s in 1:length(L$chronData[[c]]$model[[m]]$summaryTable)){

        thisTable <-L$chronData[[c]]$model[[m]]$summaryTable[[s]]
        #match to the tableId
        print(glue::glue("Checking table {thisTable$tableId}..."))
        ws <- which(startsWith(thisTable$tableId,prefix = sn))
        if(length(ws) == 1){
          thisSheet <- sh[[ws]]

          #get names without units
          excelVarNames <- purrr::map_chr(stringr::str_split(names(thisSheet),pattern = " \\("),1)

          if(any(duplicated(excelVarNames))){
            stop("It looks like there duplicated column names in the excel spreadsheet.")
          }

        }else{
          stop("couldn't match the chron summary table")
        }

        #now loop through the LiPD columns, and compare to the table, update as needed.
        toloop <- which(purrr::map_lgl(thisTable,is.list))

        for(v in toloop){
          thisColumn <- thisTable[[v]]

          #find corresponding spreadsheet column
          coli <- which(excelVarNames == thisColumn$variableName)
          if(length(coli) == 0){
            print("no match")
          }


          #compare values
          if(any(thisSheet[[coli]] != thisColumn$values)){
            updated <- TRUE
            print(glue::glue("Updating values for {thisColumn$variableName}..."))
            thisColumn$values <- as.numeric(thisSheet[[coli]])
            L$chronData[[c]]$model[[m]]$summaryTable[[s]][[v]] <- thisColumn

          }
        }

        #check values lengths
        allLengths <- purrr::map_dbl(thisTable[toloop],\(x) length(x$values))
        if(length(unique(allLengths)) > 1){
          stop("Uh oh, not all the columns are the same length after updating. This is bad.")
        }


      }
    }
  }

  if(updated){
    cl <- createChangelog(Lo,L)
    L <- updateChangelog(L,cl)
  }

  return(L)

}






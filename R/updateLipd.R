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
  usedSheet <- c()
  #go through paleoTables first
  for(p in 1:length(L$paleoData)){
    for(m in 1:length(L$paleoData[[p]]$measurementTable)){
      thisTable <- L$paleoData[[p]]$measurementTable[[m]]
      #match to the tableId
      print(glue::glue("Checking table {thisTable$tableId}..."))

      ws <- which(startsWith(thisTable$tableId,prefix = sn))
      if(length(ws) == 1){
        thisSheet <- sh[[ws]]
        usedSheet <- c(usedSheet,thisTable$tableId)

        #get names without units
        excelVarNames <- purrr::map_chr(stringr::str_split(names(thisSheet),pattern = " \\("),1)

        if(any(duplicated(excelVarNames))){
          stop("It looks like there are duplicated column names in the excel spreadsheet.")
        }

      }else{
        stop("The paleo measurement table doesn't seem to be in the excel file")
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
          usedSheet <- c(usedSheet,thisTable$tableId)

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


  #see if any tables remain in the spreadsheet

  notUsed <- sn[!startsWith(usedSheet,prefix = sn)]
  paln <- length(L$paleoData)
  tabn <- length(L$paleoData[[paln]]$measurementTable)


  if(!is.na(notUsed)){#try to add any unused sheets
    for(nu in notUsed){
      palnum <- ifelse(paln > tabn,paln + 1,paln)
      tabnum <- ifelse(paln > tabn,1,tabn + 1)

      ws <- which(sn == nu)
      thisSheet <- sh[[ws]]

      #loop through the columns and map into a new, simple paleo measurement
      #first, look for depth
      tsn <- names(thisSheet)
      dc <- which(grepl("depth",x = tsn,ignore.case = TRUE))
      if(length(dc) == 0){
        stop(glue::glue("{nu}: can't find any depth columns when trying to add new table. Stopping"))
      }
      if(length(dc) > 1){
        dc <- min(dc)
        message(glue::glue("{nu}: multiple depth columns when trying to add new table. Using left-most column ({tsn[dc]})"))
      }

      if(grepl(tsn[dc],pattern = "mm")){
        message(glue::glue("{nu}: It looks like your depth column is in mm ({tsn[dc]}), converting to cm"))
        thisSheet[,dc] <- thisSheet[,dc]/10
      }

      names(thisSheet)[dc] <- "depth (cm)"
      message(glue::glue("{nu}: renamed depth column to 'depth (cm)'"))


      for(co in 1:ncol(thisSheet)){
        #parse for name and units
        thisName <- names(thisSheet)[co]

        psplit <- stringr::str_split(thisName,"\\(",simplify = TRUE) %>% str_remove_all("\\)") %>% str_trim()
        L <- createColumn(L,
                          paleo.or.chron = "paleo",
                          paleo.or.chron.number = palnum,
                          table.type = "measurement",
                          table.number = tabnum,
                          variableName = psplit[1],
                          units = ifelse(is.na(psplit[2]),"unknown",psplit[2]),
                          values = thisSheet[[co]])
      }

      #assign in the table id
      L$paleoData[[palnum]]$measurementTable[[tabnum]]$tableId <- nu

    }
  }

  if(updated){
    cl <- createChangelog(Lo,L)
    L <- updateChangelog(L,cl)
  }

  return(L)

}



createColumn <- function(L,
                         paleo.or.chron = "paleo",
                         paleo.or.chron.number = 1,
                         table.type = "measurement",
                         table.number = 1,
                         variableName = NA,
                         units = NA,
                         values = NA,
                         additional.metadata = NA){


  #see if the relevant table exists
  ex <- try(L[[paste0(paleo.or.chron,"Data")]][[paleo.or.chron.number]][[paste0(table.type,"Table")]][[table.number]],silent = TRUE)

  if(is(ex,"try-error")){
    newTable <- TRUE
  }else{
    newTable <- FALSE
  }

  if(!newTable){
    #get the relevant table
    toi <- L[[paste0(paleo.or.chron,"Data")]][[paleo.or.chron.number]][[paste0(table.type,"Table")]][[table.number]]

    #get existing variablenames
    vn <- purrr::map(toi,purrr::pluck,"variableName")

    #which are columns?
    isCol <- which(!purrr::map_lgl(vn,is.null))

    if(length(isCol) == 0){
      stop("No valid columns")
    }


    #check variableNames
    vn <- purrr::map_chr(toi[isCol],purrr::pluck,"variableName")
    if(is.na(variableName)){
      stop("You must enter a variableName for this column")
    }

    if(variableName %in% vn){
      stop(glue::glue("The variableName {variableName} is already present in the table. Please enter a new one."))
    }

    #check units
    if(is.na(units)){
      stop("You must enter units for this column. 'unitless' is an acceptable entry.")
    }

    #check column length
    colLength <- unique(purrr::map_dbl(toi[isCol],~length(.x$values)))


    if(length(colLength) != 1){
      stop("The columns aren't all the same length!")
    }

    if(length(values) == 1 & colLength > 1){
      print(glue::glue("Replicating the input value ({values}) {colLength} times to match table length"))
      values = rep(values, colLength)
    }


    if(length(values) != colLength){
      stop(glue::glue("The new values vector has {length(values)} entries, but the rest of table has {colLength} observations. These must be the same."))
    }

  }else{#it's a new table
    print(glue::glue("Created new table {paleo.or.chron}-{paleo.or.chron.number} {table.type}-{table.number}"))
    toi <- list()
  }

  #put everything together
  cleanVariableName <- stringr::str_remove_all(variableName,"[^A-Za-z0-9]")
  toi[[cleanVariableName]] <- list()

  toi[[cleanVariableName]]$variableName <- variableName
  toi[[cleanVariableName]]$values <- values
  #B$chronData[[1]]$measurementTable[[1]]$reservoir$values[c(1,3)] <- 0 #for non 14C dates
  toi[[cleanVariableName]]$units <- units
  toi[[cleanVariableName]]$TSid <- lipdR::createTSid()

  if(!is.na(additional.metadata)){
    #make sure this is a named list
    if(!is.list(additional.metadata)){
      stop("additional.metadata must be a named list of parameters")
    }

    if(length(names(additional.metadata)) != length(additional.metadata)){
      stop("additional.metadata must be a named list of parameters - the length of the names and the list don't match")
    }

    mdname <- names(additional.metadata)

    for(i in 1:length(additional.metadata)){
      cleanParameterName <- stringr::str_remove_all(mdname[i],"[^A-Za-z0-9]")
      toi[[cleanVariableName]][[mdname[i]]] <- additional.metadata[[i]]
    }
  }

  #add back in
  if(length(L[[paste0(paleo.or.chron,"Data")]]) < paleo.or.chron.number){
    L[[paste0(paleo.or.chron,"Data")]][[paleo.or.chron.number]] <- list()
  }

  L[[paste0(paleo.or.chron,"Data")]][[paleo.or.chron.number]][[paste0(table.type,"Table")]][[table.number]] <- toi

  # print update

  print(glue::glue("Added '{variableName}' column (TSid = {toi[[cleanVariableName]]$TSid}) with {length(toi[[cleanVariableName]]$values)} values, to {paleo.or.chron}Data {paleo.or.chron.number}, {table.type}Table {table.number}"))

  return(L)

}





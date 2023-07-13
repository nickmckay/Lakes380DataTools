getChronSummaryTables <- function(L){

    pc <- c("chron")


  at <- list()#initialize alltables
  for(tpc in pc){
    PC <- L[[paste0(tpc,"Data")]]

    for(ni in 1:length(PC)){
      for(mi in 1:length(PC[[ni]]$model)){
        for(si in 1:length(PC[[ni]]$model[[mi]]$summaryTable)){
        TT <- PC[[ni]]$model[[mi]]$summaryTable[[si]]
        loTT <- TT[purrr::map_lgl(TT,is.list)]
        tt <- loTT[[1]]$values
        if(!is.null(loTT[[1]]$units)){
          units <- loTT[[1]]$units
        }else{
          units <- "missing"
        }
        tnames <- paste0(loTT[[1]]$variableName," (",units,")")

        if(length(loTT) > 1){
          for(c in 2:length(loTT)){
            tt <- cbind(tt,loTT[[c]]$values)
            if(!is.null(loTT[[c]]$units)){
              units <- loTT[[c]]$units
            }else{
              units <- "missing"
            }
            tnames <- c(tnames,paste0(loTT[[c]]$variableName," (",units,")"))
          }
        }
        tt <- as.data.frame(tt)
        names(tt) <- tnames

        #add into a list
        if(!is.null(TT$tableId)){
          at[[TT$tableId]] <- tt
        }else{
          at[[paste0(tpc,ni,mi,"summary",si)]] <- tt
        }

        }
      }
    }
  }
  return(at)
}



prepTable <- function(tab){
  #guess class
  tab <- readr::type_convert(tab)

  #move age to the front
  an <- names(tab)

  ac <- which(startsWith(tolower(an),"age"))
  if(length(ac) > 1){
    if(any(startsWith(tolower(an),"age "))){
      ac <- which(startsWith(tolower(an),"age "))[1]
    }else{
      ac <- ac[1]
    }
    ac <- ac[1]
  }

  if(length(ac) == 1){
    tab <- dplyr::select(tab,dplyr::all_of(ac),dplyr::everything())
  }


  #now move depth to the front.
  an <- names(tab)

  dc <- which(startsWith(tolower(an),"depth"))
  if(length(dc) > 1){
    if(any(startsWith(tolower(an),"depth "))){
      dc <- which(startsWith(tolower(an),"depth "))[1]
    }else{
    dc <- dc[1]
    }
  }
  if(length(dc) == 1){
    tab <- dplyr::select(tab,dplyr::all_of(dc),dplyr::everything())
  }

  #make sure it's not too big.
  if(ncol(tab) > 16384){
    tab <- as.data.frame(cbind(names(tab),t(tab)))
    names(tab) <- c("variableName",paste0("sample",seq_len(ncol(tab) - 1)))

  }

  return(tab)

}

lipd2excel <- function(L,outname = paste0(L$dataSetName,"-v",getVersion(L),".xlsx")){

#write out a spreadsheet
paleo_tabs <- getMeasurementTables(L,pc = "paleo") %>% purrr::map(prepTable)

chron_tabs <- getChronSummaryTables(L)

towrite <- append(paleo_tabs,chron_tabs)

write_xlsx(towrite,path = outname)
}









getVersion <- function(L){
  version <- as.numeric_version(purrr::map_chr(L$changelog,"version")) %>%
    max() %>%
    as.character()

  if(length(version)==0){
    version <- "0.0.0"
  }

  return(version)
}

#' Update all the age colums in measurement tables with a summary table
#'
#' @param L a lipd object
#' @param ... parameters to pass to mapAgeSummaryToPaleoData
#'
#' @return an updated LiPD object
#' @export
mapAgeSummaryToAllPaleoData <- function(L,...){
  for(p in 1:length(L$paleoData)){
    for(m in 1:length(L$paleoData[[p]]$measurementTable)){
      if(any(names(L$paleoData[[p]]$measurementTable[[m]]) == "depth")){
        L <- mapAgeSummaryToPaleoData(L,paleo.num = p,paleo.meas.table.num = m,...)
      }
    }
  }
  return(L)
}



mapAgeSummaryToPaleoData <- function(L,
                                     age.var = "age",
                                     chron.depth.var = "depth",
                                     paleo.depth.var = "depth",
                                     paleo.age.var = "age",
                                     map.median = NA,
                                     paleo.num=NA,
                                     paleo.meas.table.num=NA,
                                     chron.num=NA,
                                     model.num=NA,
                                     sum.table.num = 1,
                                     strict.search=FALSE){
  print(L$dataSetName)
  #check on the model first
  if(is.null(L$chronData)){
    stop("There's no chronData in this file")
  }

  #initialize chron.num
  if(is.na(chron.num)){
    if(length(L$chronData)==1){
      chron.num=1
    }else{
      chron.num=as.integer(readline(prompt = "Which chronData do you want to pull this summary from? "))
    }
  }

  #initialize model number
  if(length(L$chronData[[chron.num]]$model)==0){
    stop("No model in this chronData")
  }
  if(is.na(model.num)){
    if(length(L$chronData[[chron.num]]$model)==1){
      #only one model
      model.num=1
    }else{
      print(paste("ChronData", chron.num, "has", length(L$chronData[[chron.num]]$model), "models"))
      model.num=as.integer(readline(prompt = "Which chron model do you want to get the summary from? Enter an integer "))
    }
  }


  #initialize paleo.num
  if(is.na(paleo.num)){
    if(length(L$paleoData)==1){
      paleo.num=1
    }else{
      paleo.num=as.integer(readline(prompt = "Which paleoData do you want to put this age summary in? "))
    }
  }

  #initialize measurement table number
  if(is.na(paleo.meas.table.num)){
    if(length(L$paleoData[[paleo.num]]$measurementTable)==1){
      #only one pmt
      paleo.meas.table.num=1
    }else{
      print(paste("PaleoData", paleo.num, "has", length(L$paleoData[[paleo.num]]$measurementTable), "measurement tables"))
      paleo.meas.table.num=as.integer(readline(prompt = "Which measurement table do you want to put the summary in? Enter an integer "))
    }
  }


  #make sure the summary is there, with data
  copyAE  = FALSE

  print("Looking for age summary....")
  ensDepth = selectData(L,
                        table.type = "summary",
                        var.name = chron.depth.var,
                        paleo.or.chron = "chronData",
                        paleo.or.chron.num = chron.num,
                        strict.search = strict.search,
                        sum.table.num = sum.table.num,
                        model.num = model.num)$values

  ensAll = selectData(L,
                      table.type = "summary",
                      var.name = age.var,
                      paleo.or.chron = "chronData",
                      model.num = model.num,
                      sum.table.num = sum.table.num,
                      paleo.or.chron.num = chron.num,
                      strict.search = strict.search)

  #do something smarter here?
  if(grepl(age.var,pattern = "age",ignore.case = TRUE)){
    ensAll$variableName <- "age"
  }else if(grepl(age.var,pattern = "year",ignore.case = TRUE)){
    ensAll$variableName <- "year"
  }else{
    ensAll$variableName <- "time"
  }

  if(is.null(ensAll$values)){
    stop("Error: did not find the age ensemble.")
  }

  ens = ensAll$values

  if(is.null(ensDepth)){#if there are no depth data in the ensemble, try to apply the ensemble straight in (no interpolation)
    #check for the same size
    #get year, age or depth from paleodata
    pdya = selectData(L,
                      paleo.or.chron.num = paleo.num,
                      var.name = paleo.age.var,
                      always.choose = FALSE,
                      strict.search = strict.search,
                      meas.table.num = paleo.meas.table.num)$values

    if(is.null(pdya)){
      stop(glue::glue("We couldnt find depth in the ensembleTable, so we checked for {paleo.age.var} in the paleoTable, and couldn't find it. If there as a time vector in the paleoData measurementTable, specify it in paleo.age.var"))
    }

    #check for length of that variable
    if(length(pdya)  == nrow(ens)){
      #that's a good start, now let's see if they're correlated
      ct <- cor(apply(ens,1,median,na.rm = TRUE),pdya)
      if(abs(ct) < .7){
        ans <- askUser(glue::glue("Hmm, your mapped age ensemble and paleoData age vectors aren't very similar (r = {ct}), are you sure you want to use {paleo.age.var} to map the ensemble?"))
        if(tolower(substr(ans,1,1)) != "y"){
          stop("Stopped. Probably a good choice")
        }
      }
      copyAE  = TRUE
    }else{
      stop("Couldnt find depth in the ensembleTable, and the paleoData measurementTable has a different number of rows thant the ensemble.")
    }
  }


  if(!copyAE){
    #get the depth from the paleo measurement table
    print("getting depth from the paleodata table...")
    depth = selectData(L,paleo.or.chron.num = paleo.num,var.name = paleo.depth.var,always.choose = FALSE,ens.table.num = ens.table.num,meas.table.num = paleo.meas.table.num)$values

    #check that depth is numeric
    if(!is.numeric(depth)){
      stop("Uh oh, paleo depth is not a numeric vector. That will cause problems - check paleoData[[p]]measurementTable[[m]]$depth$values (or similar if var.name is not depth)")
    }


    #interpolate
    na.depth.i = which(!is.na(depth))
    aei = matrix(nrow = length(depth),ncol = 1)
    aeig = Hmisc::approxExtrap(ensDepth,ens,xout=depth[na.depth.i],na.rm=TRUE)$y
    aei[na.depth.i,] = aeig


  }else{
    #check to see if the ensemble needs to be flipped
    #correlate pdya with ens[,1]

    test.cor <- cor(pdya,ens[,1])
    if(test.cor < 0){
      aei <- rev(ens)
    }else{
      aei = ens
    }
  }

  #guess
  if(is.na(sum.table.num)){sum.table.num=1}


  #assign into measurementTable
  L$paleoData[[paleo.num]]$measurementTable[[paleo.meas.table.num]][[ensAll$variableName]]$variableName =  ensAll$variableName
  L$paleoData[[paleo.num]]$measurementTable[[paleo.meas.table.num]][[ensAll$variableName]]$values = aei
  L$paleoData[[paleo.num]]$measurementTable[[paleo.meas.table.num]][[ensAll$variableName]]$units = ensAll$units
  L$paleoData[[paleo.num]]$measurementTable[[paleo.meas.table.num]][[ensAll$variableName]]$fromChronData = chron.num
  L$paleoData[[paleo.num]]$measurementTable[[paleo.meas.table.num]][[ensAll$variableName]]$frommodel = model.num
  L$paleoData[[paleo.num]]$measurementTable[[paleo.meas.table.num]][[ensAll$variableName]]$TSid = lipdR::createTSid("sum")

  L$paleoData[[paleo.num]]$measurementTable[[paleo.meas.table.num]][[ensAll$variableName]]$description = paste("age summary pulled from chronData", chron.num,"model",model.num,"- fit to paleoData depth with linear interpolation")

  print(glue::glue("mapAgeSummaryToPaleoData created new variable {ensAll$variableName} in paleo {paleo.num} measurement table {paleo.meas.table.num}"))


  return(L)


}

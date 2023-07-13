empty_rectangle <- function(text){

  foo <- data.frame(x1 = 1, x2 = 2, y1 = 1, y2 = 2,
                    text = text)

  er <- ggplot(foo) +
    geom_rect(aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2),
              color = "black", size = 2, fill = "lightblue") +
    geom_text(aes(x = x1 + (x2 - x1) / 2, y = y1 + (y2 - y1) / 2,
                  label = text),
              size = 10) +
    theme_classic() +
    theme(axis.line  = element_blank(),
          axis.ticks = element_blank(),
          axis.text  = element_blank(),
          axis.title = element_blank())

  return(er)
}

#' Get epoch ages for plotting
#'
#' @param lakeId Lakes380 numeric id
#' @param chronTable chronTable to update
#' @param epochFile epoch file to load
#'
#' @return list of epoch ages
#' @export
getEpochAges <- function(lakeId,chronTable,epochFile = "data/Lakes380 DNA and pollen dataset with era - 9 May 2023.xlsx"){

  #get epoch depths
  dat <- readxl::read_excel(epochFile) %>%
    filter(ID == lakeId)

  if(nrow(dat) != 1){
    euroAge <- NA
    maoriAge <- NA
  }else{
    euroDepth <- as.numeric(dat$`Start Euro`)
    maoriDepth <- as.numeric(dat$`Start Maori`)

    euroAge <- approx(chronTable$depth,chronTable$age,xout = euroDepth)$y
    maoriAge <- approx(chronTable$depth,chronTable$age,xout = maoriDepth)$y
  }

  return(list(euroAge = euroAge, maoriAge = maoriAge))

}

#' Plot a Lakes380 data dashboard
#'
#' @param L a lipd object
#'
#' @return a ggplot object
#' @import ggplot2 egg vegan ape
#' @export
plotDashboard <- function(L){
  #L <- readLipd("~/Dropbox/lipdverse/Lakes380National/AlpineLake_46954.Lakes380.lpd")

  # dashboards
  TS <- extractTs(L)
  ts <- ts2tibble(TS)

  #simple age/depth plot
  chronTable <- data.frame(age = L$chronData[[1]]$model[[1]]$summaryTable[[1]]$age$values,
                           depth = L$chronData[[1]]$model[[1]]$summaryTable[[1]]$depth$values)

  if(L$chronData[[1]]$model[[1]]$methods == "loaded from csv"){
    chronType = "OxCal"
  }else{
    chronType = "Biostrat (linear interp)"
  }

  ageDepth <- ggplot(chronTable) +
    geom_line(aes(x  = age, y = depth,color = chronType)) +
    scale_y_reverse("Depth (cm)") +
    scale_color_manual("Age Model",values = "black") +
    xlab("Age (yr BP)") +
    theme_bw()


  epochAges <- try(getEpochAges(L$geo$lakeId,chronTable),silent = TRUE)

  if(is(epochAges,"try-error")){
    warning("Epoch ages didn't work - check file reference")
    epochAges <- list(euroAge = NA, maoriAge = NA)
  }

  # HSI
  hsi <- ts %>%
    filter(paleoData_variableName == "RABD660670")

  if(nrow(hsi) == 0){
    rabdPlot <- empty_rectangle("HSI data are missing")
  }else{
    hsi <- as.lipdTsTibbleLong(hsi)

    hsi$top <- hsi$depth[[1]] < 0.5

    rabdPlot <- ggplot(hsi) +
      geom_line(aes(x = age, y = paleoData_values,color = top)) +
      scale_x_reverse("Age (BP)") +
      ylab("RABD 660-670 (unitless)") +
      geom_vline(aes(xintercept = epochAges$euroAge),color = "dark green",linetype = 2) +
      geom_vline(aes(xintercept = epochAges$maoriAge),color = "orange",linetype = 2) +
      scale_color_manual(values = c("black","red")) +
      theme_bw() +
      theme(legend.position = "none")


  }
  #eDNA
  edna <- ts %>%
    filter(paleoData_proxy == "eDNA")

  if(nrow(edna) == 0){
    biplot <- pcoaTimePlot <- empty_rectangle("eDNA data are missing")
  }else{

    countMatrix <- edna %>%
      select(paleoData_values,paleoData_variableName) %>%
      pivot_wider(names_from = paleoData_variableName,values_from = paleoData_values) %>%
      unchop(cols = everything())

    countMatrixLog <- log1p(countMatrix)

    ## Principal coordinate analysis and simple ordination plot
    dist.mat <- vegdist(countMatrixLog, "bray")
    res <- ape::pcoa(dist.mat)

    biplot <- ggplot() +
      geom_point(size = 4, aes(x = res$vectors[,1], y = res$vectors[,2], color = edna$age[[1]])) +
      scale_color_viridis_c("Age (yr BP)") +
      xlab("PCOA 1") +
      ylab("PCOA 2") +
      theme_bw()


    pcoaTimePlot <- ggplot() +
      geom_line(aes(x = edna$age[[1]], y = res$vectors[,1], color = "PC1")) +
      geom_line(aes(x = edna$age[[1]], y = res$vectors[,2], color = "PC2")) +
      geom_vline(aes(xintercept = epochAges$euroAge),color = "dark green",linetype = 2) +
      geom_vline(aes(xintercept = epochAges$maoriAge),color = "orange",linetype = 2) +
      scale_x_reverse("Age (BP)") +
      ylab("PCOA Axis") +
      scale_color_brewer("Axis",palette = "Set1") +
      theme_bw()

  }
  # pollen
  if(any(names(ts) == "paleoData_pollenGroup")){
    pollenT1 <- ts %>%
      filter(paleoData_pollenGroup == "T1")

    pollenC <- ts %>%
      filter(paleoData_pollenGroup == "C")

    pollenB <- ts %>%
      filter(paleoData_pollenGroup == "B")


    pollenMatT1 <- pollenT1 %>%
      select(paleoData_values,paleoData_variableName) %>%
      pivot_wider(names_from = paleoData_variableName,values_from = paleoData_values) %>%
      unchop(cols = everything()) %>%
      as.matrix()

    pollenMatB <- pollenB %>%
      select(paleoData_values,paleoData_variableName) %>%
      pivot_wider(names_from = paleoData_variableName,values_from = paleoData_values) %>%
      unchop(cols = everything()) %>%
      as.matrix()

    pollenMatC <- pollenC %>%
      select(paleoData_values,paleoData_variableName) %>%
      pivot_wider(names_from = paleoData_variableName,values_from = paleoData_values) %>%
      unchop(cols = everything()) %>%
      as.matrix()

    pollenSumT1 <- rowSums(pollenMatT1,na.rm = TRUE)

    if(length(pollenSumT1) == 0){
      pollenSumT1 <- matrix(0,nrow = length(pollenT1$age[[1]]))
    }

    psT1 <- data.frame(age = pollenT1$age[[1]],
                       percent = pollenSumT1,
                       type = "Exotics")

    pollenSumB <- rowSums(pollenMatB,na.rm = TRUE)
    if(length(pollenSumB) == 0){
      pollenSumB <- matrix(0,nrow = length(pollenT1$age[[1]]))
    }

    psB <- data.frame(age = pollenT1$age[[1]],
                      percent = pollenSumB,
                      type = "Bracken")

    pollenSumC <- rowSums(pollenMatC,na.rm = TRUE)
    if(length(pollenSumC) == 0){
      pollenSumC <- matrix(0,nrow = length(pollenT1$age[[1]]))
    }


    psC <- data.frame(age = pollenT1$age[[1]],
                      percent = pollenSumC,
                      type = "Charcoal")

    ps <- bind_rows(psT1,psB,psC)


    pollenTimePlot <- ggplot(ps) +
      geom_area(aes(x = age, y = percent)) +
      geom_vline(aes(xintercept = epochAges$euroAge),color = "dark green",linetype = 2) +
      geom_vline(aes(xintercept = epochAges$maoriAge),color = "orange",linetype = 2) +
      scale_x_reverse("Age (BP)") +
      ylab("Abundance (%)") +
      facet_grid(type ~ .,scales = "free_y") +
      theme_bw()

  }else{
    pollenTimePlot <- empty_rectangle("Pollen data are missing")

  }


  # map

  map <- geoChronR::mapLipd(L,size = 2,label.site = FALSE, f = 0,extend.range = 0.5,projection = "mercator")


  dashboard <- egg::ggarrange(plots = list(map,rabdPlot,biplot,pcoaTimePlot,ageDepth,pollenTimePlot),nrow = 3,widths = c(1,3),draw = FALSE) %>%
    ggpubr::annotate_figure(top = ggpubr::text_grob(L$dataSetName,
                                                    color = "black", face = "bold", size = 14))

  return(dashboard)

}


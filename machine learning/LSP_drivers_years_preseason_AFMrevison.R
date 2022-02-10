{
  library(plyr)
  library(dplyr)
  library(tidyr)
  library(readr)
  ## Plot spatial figures of SOS advance 
  library(corrplot)
  library(maps)
  library(maptools)
  library(raster)
  library(mapdata)
  require(rgdal)
  require(ggplot2)
  require(rgeos)
  require(ggmap)
  require(RColorBrewer)
  require(sp)
  library(colorRamps)
  library(scales)
  library("quantreg")
  library("MASS")
  library(sfsmisc)
  library(matrixStats)
  # library(SDMTools)
  library(stringr)
  library(randomForest)
  library(dismo)
  library(h2o)
  library(rasterVis)
  library(car)
  norm1 <- function(data, variable=NULL, group, by='year', mean=c('all', 'excluded'), sd=c('diff', 'sd', 'asmean'), postfix='', row_min=1){
    nm <- names(data)
    if(is.numeric(by)) by <- nm[by]
    if(sum(nm==by)!=1) warnings("Please check column 'by'")
    if(is.null(variable)) {
      variable <- nm[nm!=by]
      if(length(variable)<= 0 ) warnings('Please recheck the names in x')
    } else if(is.character(variable)){
      if(sum(nm %in% variable)!=length(variable)) warnings("Please check column 'variable'")
    } else if(is.numeric(variable)) {
      variable <- nm[variable]
      if(sum(nm %in% variable)!=length(variable)) warnings("Please check column 'variable'")
    } else{
      warnings('What happened!!!')
    }
    
    
    colvar <- which(names(data) %in% variable) #ids
    ncolvar <- length(colvar)
    resout <- data
    resout[colvar] <- NA
    colname <- names(data)
    colname[colvar] <- paste0(colname[colvar], postfix)
    names(resout) <- colname
    
    data <- data %>% unite('groupj',  group, sep='_', remove=F)
    idgroups <- unique(data$groupj)
    ngroups <- length(idgroups)
    k <- 1
    ir <- 0
    for(idgroup in idgroups){
      ir <- ir + 1
      # idgroup <- idgroups[ir]
      # print(paste0(ir, ' ', ngroups))
      # print(idgroups[ir])
      tmpdata <- data %>% 
        filter(groupj==idgroup) %>%
        dplyr::select(-groupj)
      # tmpdata <- subset(tmpdata, select = -groupj )
      tmpout <- tmpdata
      tmpout[colvar] <- NA
      nrtmp <- nrow(tmpdata)
      # print(nrtmp)
      
      if(nrtmp>=row_min&(nrtmp>2|(nrtmp>1&mean=='all'))){ #|(nrtmp>1&mean=='excluded'&sd=='sd')
        if(mean=='all'){
          rows <- seq(1, nrtmp)
          tmpmean <- colMeans2(as.matrix(tmpdata[colvar]), rows = rows, na.rm=T)
          tmpmean <- t(replicate(nrtmp, tmpmean))
          if(sd=='diff') {
            tmpout[colvar] <- tmpdata[colvar] - tmpmean
          } else if((sd=='sd')|(sd=='asmean')){
            tmpsd <- colSds(as.matrix(tmpdata[colvar]), rows = rows, na.rm=T)
            tmpsd <- t(replicate(nrtmp, tmpsd))
            tmpout[colvar] <- (tmpdata[colvar] - tmpmean)/tmpsd
          } else warnings('Please check "sd"')
        } else if(mean == 'excluded'){
          tmpmean <- matrix(NA, nrow=nrtmp, ncol=ncolvar)
          if(sd=='asmean') {tmpsd <- matrix(NA, nrow=nrtmp, ncol=ncolvar)}
          for(irtmp in seq_len(nrtmp)){
            rows <- seq_len(nrtmp)[-irtmp]
            tmpmean[irtmp,] <- colMeans2(as.matrix(tmpdata[colvar]), rows = rows, na.rm=T)
            if(sd=='asmean') {tmpsd[irtmp,] <- colSds(as.matrix(tmpdata[colvar]), rows = rows, na.rm=T)}
          }
          
          if(sd=='diff'){
            tmpout[colvar] <- tmpdata[colvar] - tmpmean
          } else if(sd=='sd'){
            tmpsd <- colSds(as.matrix(tmpdata[colvar]), rows = seq(1, nrtmp), na.rm=T)
            tmpsd <- t(replicate(nrtmp, tmpsd))
            tmpout[colvar] <- (tmpdata[colvar] - tmpmean)/tmpsd
          } else if(sd=='asmean'){
            tmpout[colvar] <- (tmpdata[colvar] - tmpmean)/tmpsd
          }
        }
      }
      
      resout[seq(k, k+nrtmp-1),] <- tmpout
      k <- k+nrtmp
      
      
      
    }
    
    
    return(resout)
  }
  # The difference bt norm1 and norm2 is that norm2 mean is calculated by excluding the target year
  
  {
    basenames <- c('long', 'lat', "SOS", "EOS",'GMin', 'GMax', 
                   'Soil', 'TFC', 'SFC', 'Water', 'Elevation', 
                   'Slope', 'Aspect', 'BS')
    soscli <- c("SRad", "Prcp", "GDD", "CD", "TMax", "TMin")
    eoscli <- c("SRad", "Prcp", "GDD", "TMax", "TMin")
    sosclinames <- paste0(c(paste0(soscli, 30), paste0(soscli, 60), paste0(soscli, 90),
                            paste0(soscli, 120), paste0(soscli, 150), paste0(soscli, 180)
    ), '_SOS')
    eosclinames <- paste0(c(paste0(eoscli, 30), paste0(eoscli, 60), paste0(eoscli, 90),
                            paste0(eoscli, 120), paste0(eoscli, 150), paste0(eoscli, 180)
    ), '_EOS')
    freeze <- c('FFD', 'LFD', 'DF')
    snow <- c('FSD', 'LSD', 'DS')
    allnames <- c(basenames, sosclinames, eosclinames, 
                  paste0(freeze, '_SOS'), paste0(freeze[1], '_EOS'), 
                  paste0(snow, '_SOS'), paste0(snow[1], '_EOS'))
    sosclinames <- c(sosclinames, paste0(freeze, '_SOS'), paste0(snow, '_SOS'))
    eosclinames <- c(eosclinames, paste0(freeze[1], '_EOS'), paste0(snow[1], '_EOS'))
  }
  
}
# for(tmp in allnames){
#   print(noquote(tmp) )
#   noquote(tmp) 
# }
# load("/gpfs/scratch/jianmin.wang/Ponil_Complex_LSP_driver_AFMrevision/data/brtmodel.preseason.RData")
# save.image("/gpfs/scratch/jianmin.wang/Ponil_Complex_LSP_driver_AFMrevision/data/brtmodel.preseason.RData")

{
  setwd('/gpfs/scratch/jianmin.wang/Ponil_Complex_LSP_driver_AFMrevision/data/')
  pathout <- '/gpfs/scratch/jianmin.wang/Ponil_Complex_LSP_driver_AFMrevision/figures/'
  sate <- 'HLS' #'VNP'  'HLS', 'MOD250', 'MOD500'
  if (sate=='MOD500'){
    years <- seq(2001, 2018)
    years <- years[-2]
    subf <- 'LSP_MOD500_lstwinterJ4_preseason_snowfreeze_meantpsp/'
  } else if(sate=='HLS'){
    years <- 2018
    subf <- ''
  } 
  
  # tmp <- as.data.frame(raster[[5]], xy=T, na.rm=F, centroids=T, optional=F)
  for(year in years){
    # year <- 2018
    print(year)
    raster <- stack(paste0(subf, 'LSP_preseasonmodel_', sate, '_', year, '.tif')) 
    data <- as.data.frame(raster, xy=T, na.rm=T, centroids=T, optional=F)
    names(data) <- allnames
    data$Eastness <- sin(data$Aspect*pi/180)
    data$Northness <- cos(data$Aspect*pi/180)
    data$GRatio <- data$GMin/data$GMax
    data$GMin <- data$GMin/10000
    data$GMax <- data$GMax/10000
    data$VFC <- data$TFC + data$SFC
    data$TPV <- data$TFC/data$VFC
    data$TPV[is.nan(data$TPV)] <- NA
    # data$TPV[data$VFC == 0] <- NA
    
    data$year <- year
    if(year==years[1]) {data.all <- data}  else {data.all <- rbind(data.all, data)}
    if(year==2018 ) {assign(paste0('data.', sate, '.s2018.CD'), data)}
    #CD means using chilling days but not chilling degree days
  }
  
  # data.all.MOD500 <- data.all.MOD500 %>% 
  #   mutate(TPV2 = (0.779+6.7*GMin)/(0.39+3.8*GMax))
  # data.all$year <- as.factor(data.all$year)
  if(sate=='MOD500') {assign(paste0('data.all.', sate), data.all)}
}
par(mar=c(2, 2, 1, 1))




#### All years data normalized
{
  syears <- seq(2003, 2018)
  syears <- c(2001, syears)
  BSs <- c(0, 1, 2, 3, 4, 6)
  # BSs <- c(1)   #10000
  # BSs <- c(2, 3) #48000
  # BSs <- c(4)
  data.tmp <- data.all.MOD500 %>% 
    filter(EOS<366, EOS>200, SOS > 70, SOS<220) %>% #VFC>0 
    filter(year %in% syears) #%>% 
  # left_join(dataref, by='year') %>% 
  # mutate(GMin2=GMin*GMin_ref, GMax2=GMax*GMax_ref) %>%
  # dplyr::select(-c(GMin_ref, GMax_ref)) %>% 
  # mutate(GRatio2=GMin2/GMax2) %>% 
  # filter(GMin2>0, GMin2<1, GMax2>0, GMax2<1, GRatio2>0, GRatio2<1)  %>% 
  # filter(VFC >= 0.2)
  
  
  
  # #Filter out the observations with abnormaly high GMin
  # summ <- data.tmp %>%
  #   filter(VFC >0.4, TPV>0.2, TPV<0.8) %>%
  #   group_by(long, lat) %>%
  #   mutate(GMin_mean = median(GMin[year!=2001], na.rm=T)) %>%
  #   mutate(GMin_2001 = min(GMin[year==2001], 10)) %>%
  #   mutate(GMin = replace(GMin, year!=2001 & (GMin>GMin_2001 | GMin>GMin_mean*1.2 | GMin<GMin_mean/1.2), NA)) %>%
  #   ungroup()
  # summ <- as.data.frame(summ) %>% filter(complete.cases(.)) 
  # # View(summ[c('year', 'GMin', 'GMin_mean', 'GMin_2001')])
  
  
  
  
  row_min <- length(syears)
  meanm <- 'all'
  data.all.norm <- norm1(data.tmp, variable=c('SOS', 'EOS', 
                                              'GMin', 'GMax', 'GRatio',
                                              # 'GMin2', 'GMax2', 'GRatio2', 
                                              sosclinames, eosclinames), 
                         group=c('long', 'lat'), by='year', 
                         mean=meanm, sd='sd', postfix='',
                         row_min=row_min)
  
  
  
  # vegeprop <- c('GRatio2', 'GMax2') 
  # vegeprop <- c('GRatio', 'GMax', 'GMin') 
  # vegeprop <- c('TPV_p', 'VFC_p', 'TFC_p') 
  # vegeprop <- c('TPV_p', 'VFC_p') 
  sosclinames1 <- sosclinames[sosclinames!='FFD_SOS' & sosclinames!='FSD_SOS']
  eosclinames1 <- eosclinames
  refined <- T
  vegeprop <- c('GRatio', 'GMax')
  for (phen in c('SOS', 'EOS')){ #'SOS',, 'EOS'
    # phen <- 'SOS'
    if(phen=='SOS') {
      gbm.y <- phen
      gbm.x.f.old <- c(sosclinames1, vegeprop)
      # gbm.x.f.old <- gbm.x.f.old[!grepl('CD30', gbm.x.f.old)]
      gbm.x.f.new <- str_replace(gbm.x.f.old, '_SOS', '')
      gbm.x <- gbm.x.f.new
      if(refined){
        gbm.x <- c(vegeprop,
                   'SRad60', 'Prcp60', 'GDD30', 'CD90', 'TMax60', #'TMin60',
                   'DF', 'LSD', 'DS') #'FFD', 'FSD', 'LFD',
        # gbm.x <- c(vegeprop,
        #            'SRad60', 'Prcp60', 'GDD30', 'CD90', 'TMax60', 
        #            'DF', 'LSD',  'DS') #'LFD', 'FFD', 'DF', 'FSD',  'DS', 'TMin60',
      }
    } else if(phen=='EOS'){
      gbm.y <- phen
      gbm.x.f.old <- c(eosclinames1, vegeprop, 'SOS')
      gbm.x.f.new <- str_replace(gbm.x.f.old, '_EOS', '')
      gbm.x <- gbm.x.f.new
      if(refined){
        gbm.x <- c(vegeprop,
                   'SRad60', 'Prcp30', 'TMax60', 'TMin30',
                   'FFD', 'FSD', 'SOS') # 'GDD30',
      }
    }
    BSs <- c(0, 1, 2, 3 ,4, 5, 6)
    # BSs <- c(1, 2, 3 ,4)
    data.train <- data.all.norm %>% 
      filter(BS %in% BSs) %>% 
      filter(Water==0, VFC>0) %>% 
      dplyr::select(-c(Soil, TFC, SFC, Water)) %>% 
      dplyr::select(c(gbm.y, gbm.x.f.old)) %>% #, -starts_with('DayL')
      filter(complete.cases(.)) 
    names(data.train) <- c(gbm.y, gbm.x.f.new)  
    
    nrow <- nrow(data.train)
    print(nrow)
    
    if(phen=='SOS'){
      tmp <- data.train[c(gbm.y, gbm.x)]
      M <- lm(SOS ~., data = tmp)
    } else{
      tmp <- data.train[c(gbm.y, gbm.x)]
      M <- lm(EOS ~., data = tmp)
    }
    print(vif(M))
    
    # tmpcor <- cor(data.train[gbm.x])
    # # tmpcor <- cor(data.train %>% dplyr::select(gbm.x[gbm.x!='BS' & gbm.x!='FFD']))
    # # filename <- paste0(pathout, 'SPcor_', sate, '_', phen, '.png')
    # # png(filename = filename, width=8.5, height=8.5, units="in", res=300)
    # corrplot(tmpcor, method='number', type = 'upper')
    # # dev.off()
    
    max.trees = 50000
    n.trees = 4000
    learning.rate = 0.008
    # if(length(gbm.x) <15) {learning.rate=0.006}
    samples <- 'alldata'
    treec <- 5
    if(nrow > 40000){
      ntrain <- 2000 #6000
      pcode <- c(rep(1, ntrain), rep(2, nrow-ntrain))
      set.seed(76482513)
      pcode <- sample(pcode)
      data.train <- data.train[pcode==1,]
      max.trees = 50000
      n.trees = 2000  #2000
      learning.rate = 0.002 #0.006
      samples <- 'subset'
      treec <- 5
    }
    # data.train <- data 
    print(nrow(data.train))
    
    set.seed(76482513)
    # gbm.x <- gbm.x.f.new
    print(gbm.y)
    print(gbm.x)
    print(learning.rate)
    brt.model <- gbm.step(data = data.train, gbm.x = gbm.x, gbm.y = gbm.y,
                          family = 'gaussian', tree.complexity =treec, max.trees = max.trees,  #100000
                          learning.rate = learning.rate, bag.fraction = 0.5, n.trees = n.trees, #1000, 4000  #150
                          n.folds = 10, verbose=T)
    # brt.model <- gbm.fixed(data = data.train, gbm.x = gbm.x, gbm.y = gbm.y,
    #                        family = 'gaussian', tree.complexity =treec,  #100000
    #                        learning.rate = learning.rate, bag.fraction = 0.5, n.trees = 120000, #1000, 4000  #150
    #                        verbose=T)
    
    par(mar=c(5, 6, 1, 1))
    par(mfrow=c(1,1))
    brtmodelname <- paste0('brt.MOD500.', samples, '.norm', meanm, '.', phen, '.tc',  treec, '.rowm', row_min, '.lr', 1/learning.rate, '.BS', paste(BSs, collapse = ''), '.snowfreeze')
    if(refined) {brtmodelname <- paste0(brtmodelname, '.refined')}
    assign(brtmodelname, brt.model)
    summary(brt.model, srt=90, las=1) #, xlab='Relative influence (%)'
    gbm.plot(brt.model, n.plots=12, plot.layout = c(3, 4), main=NA, write.title =F)
    # gbm.plot(brt.MOD500.SOS.subset.norm.GRatio100, n.plots=8, plot.layout = c(2, 4), main=NA, write.title =F)
  }
}

brt.model <- brt.MOD500.alldata.normall.SOS.tc5.rowm17.lr125.BS1234.snowfreeze.refined.corrselect
gbm.plot(brt.model, n.plots=12, plot.layout = c(3, 4), main=NA, write.title =F)
# response.matrix <- gbm::plot.gbm(brt.HLS.2018.subset.SOS.tc5.slope0, 'Elevation', return.grid = TRUE)
tmp <- brt.model$gbm.call$dataframe[c(brt.model$gbm.call$gbm.x, brt.model$gbm.call$gbm.y)]
M <- lm(SOS ~., data = tmp)
print(vif(M))







#### Spatial variance
# Not using unless you only wanna do spatial
# {
#   sate = 'MOD500' #'VNP'  'HLS', 'MOD250', 'MOD500'
#   if (sate=='MOD500'){
#     subf <- 'LSP_MOD500_lstwinterJ4_preseason_snowfreeze_meantpsp/'
#   } else if(sate=='HLS'){
#     subf <- ''
#   }
# 
#   year <- 2018
#   raster <- stack(paste0(subf, 'LSP_preseasonmodel_', sate, '_', year, '.tif'))
#   data <- as.data.frame(raster, xy=T, na.rm=T, centroids=T, optional=F)
#   names(data) <- allnames
#   data$Eastness <- sin(data$Aspect*pi/180)
#   data$Northness <- cos(data$Aspect*pi/180)
#   data$GRatio <- data$GMin/data$GMax
#   data$GMin <- data$GMin/10000
#   data$GMax <- data$GMax/10000
#   data$VFC <- data$TFC + data$SFC
#   data$TPV <- data$TFC/data$VFC
#   data$TPV[is.nan(data$TPV)] <- NA
# 
#   data$year <- year
# 
#   assign(paste0('data.', sate, '.s2018.CD'), data)
# }
{  
  sate = 'MOD500' # 'HLS', 'MOD250', 'MOD500'
  refined <- F
  data <- get(paste0('data.', sate, '.s2018.CD'))
  data <- data %>% filter(EOS<366, EOS>200, SOS > 70, SOS<220)
  
  # tmp <- data %>% filter(EOS < 240,VFC>0)
  # mean(tmp$VFC, na.rm=T)
  # mean(tmp$TPV, na.rm=T)
  # tmp <- data %>% filter(Elevation>2700) %>%
  #   dplyr::select(SOS, VFC, Elevation)
  slopemin <- 0
  if(sate=='MOD500') {slopemin <- 1}
  data <- data %>% filter(Slope >=slopemin)
  
  vegeprop <- c('TPV', 'VFC')
  topography <- c('Elevation', 'Slope', 'Eastness', 'Northness')
  for (phen in c('SOS', 'EOS')){ #c('SOS', 'EOS')
    # phen <- 'SOS'
    if(phen=='SOS') {
      gbm.y <- phen
      gbm.x <- c(vegeprop, topography)
      if(sate=='MOD500'){
        gbm.x <- c(vegeprop, topography, 'SRad30_SOS', 'Prcp150_SOS')
        gbm.x <- c(vegeprop, topography, 'SRad30_SOS', 'Prcp150_SOS', 'LFD_SOS', 'LSD_SOS')
      } else if(sate=='HLS'){
        gbm.x <- c(vegeprop, topography, 'SRad90_SOS', 'Prcp180_SOS')
        gbm.x <- c(vegeprop, topography, 'SRad90_SOS', 'Prcp180_SOS', 'LFD_SOS', 'LSD_SOS')
      }
      if(refined){
        if(sate=='MOD500'){
          gbm.x <- c(vegeprop, topography, 
                     'SRad30_SOS', 'Prcp150_SOS', 'GDD30_SOS', 'CD180_SOS', 'TMin180_SOS',
                     'LFD_SOS', 'FSD_SOS', 'LSD_SOS') 
          #'FFD_SOS', 'TMax60_SOS', 'DF_SOS', 
          #'DS_SOS'
        } else if(sate=='HLS'){
          gbm.x <- c(vegeprop, topography, 
                     'SRad90_SOS', 'Prcp180_SOS', 'GDD30_SOS', 'CD180_SOS', 'TMax30_SOS', 'TMin180_SOS',
                      'LFD_SOS', 'DF_SOS', 'FSD_SOS', 'LSD_SOS', 'DS_SOS') #'FFD_SOS', 
        }
      }
      
    } else if(phen=='EOS'){
      gbm.y <- phen
      gbm.x <- c(vegeprop, topography, 'SOS')
      if(sate=='MOD500'){
        gbm.x <- c(vegeprop, topography, 'SRad60_EOS', 'Prcp90_EOS', 'SOS')
      } else if(sate=='HLS'){
        gbm.x <- c(vegeprop, topography,  'SRad60_EOS', 'Prcp90_EOS', 'SOS')
      }
      if(sate=='MOD500'){
        gbm.x <- c(vegeprop, topography, 
                   'SRad60_EOS', 'Prcp90_EOS', 'GDD30_EOS', 'TMax30_EOS', 'TMin30_EOS',
                   'FSD_EOS', 'SOS') #'FFD_EOS', 
      } else if(sate=='HLS'){
        gbm.x <- c(vegeprop, topography, 
                   'SRad60_EOS', 'Prcp90_EOS', 'GDD30_EOS', 'TMax30_EOS', 'TMin30_EOS',
                   'FSD_EOS', 'SOS') #'FFD_EOS', 
      }
    }
    # gbm.x <- c(eosclinames, vegeprop, topography)
    #threshold to purify the data.train
    data.train <- data %>% 
      filter(year %in% 2018) %>% 
      dplyr::select(c(gbm.y, gbm.x))  %>% 
      filter(complete.cases(.)) %>%
      rename_at(gbm.x, list(~str_replace(., paste0('_', phen), '')))
    gbm.x <- str_replace(gbm.x, paste0('_', phen), '')
    M <- lm(SOS ~., data = data.train)
    vif(M)
    tmpcor <- cor(data.train[gbm.x])
    corrplot(tmpcor, method='number', type = 'upper')
    # summary(data.train %>% dplyr::select(starts_with('GDD')))
    
    nrow <- nrow(data.train)
    print(nrow)
    if(sate=='MOD500'){
      samples <- 'alldata'
      learning.rate <- 0.001
      n.trees <- 1000
      max.trees <- 50000
      treec <- 5
    } else if(sate=='HLS'){
      samples <- 'alldata'
      learning.rate <- 0.05
      n.trees <- 4000
      max.trees <- 50000
      treec <- 5
    }
  
    if(nrow > 50000){
      ntrain <- 5000
      pcode <- c(rep(1, ntrain), rep(2, nrow-ntrain))
      set.seed(76482513)
      pcode <- sample(pcode)
      data.train <- data.train[pcode==1,]
      samples <- 'subset'
      learning.rate <- 0.002
      if(phen=='EOS') learning.rate <- 0.001
      n.trees <- 2000
      max.trees <- 50000
      treec <- 5
    }
    
    set.seed(76482513)
    # gbm.x <- gbm.x[gbm.x!='CD30']
    print(gbm.y)
    print(gbm.x)
    print(nrow(data.train))
    
    
    soscli <- c("SRad", "Prcp", "GDD", "CD", "TMax", "TMin")
    eoscli <- c("SRad", "Prcp", "GDD", "TMax", "TMin")
    gbm.x <- c(topography, paste0('TMin', seq(30, 180, 30)), vegeprop)
    tmpcor <- cor(data.train[gbm.x])
    # tmpcor <- cor(data.train %>% dplyr::select(gbm.x[gbm.x!='BS' & gbm.x!='FFD']))
    # filename <- paste0(pathout, 'SPcor_', sate, '_', phen, '.png')
    # png(filename = filename, width=30, height=30, units="in", res=300, type="cairo")
    corrplot(tmpcor, method='number', type = 'upper')
    # dev.off()
    
    
    
    
    
    
    brt.model <- gbm.step(data = data.train, gbm.x = gbm.x, gbm.y = gbm.y,
                          family = 'gaussian', tree.complexity =treec, max.trees = max.trees,
                          learning.rate = learning.rate, bag.fraction = 0.5, n.trees=n.trees, #1000,   #150
                          n.folds = 10, verbose=T)
    par(mar=c(5, 6, 1, 1))
    brtmodelname <- paste0('brt.', sate, '.2018.', samples, '.', phen, '.tc',  treec, '.slope', slopemin*10)
    if(refined) {brtmodelname <- paste0(brtmodelname, '.refined')}
    assign(brtmodelname, brt.model) # '.refined'
    summary(brt.model, srt=90, las=1) #, xlab='Relative influence (%)'
    gbm.plot(brt.model, n.plots=7, plot.layout = c(3, 3), main=NA, write.title =F)

    response.matrix <- gbm::plot.gbm(brt.model, 'Slope', return.grid = TRUE)
  }
}



brt.model <- brt.MOD500.2018.alldata.SOS.tc5
gbm.plot(brt.MOD500.2018.alldata.SOS.tc5, n.plots=12, plot.layout = c(3, 4), main=NA, write.title =F)
response.matrix <- gbm::plot.gbm(brt.HLS.2018.subset.SOS.tc5.slope0, 'Elevation', return.grid = TRUE)

gbm.plot2(gbm.object, variables = c('TPV', 'VFC'), smooth=T, plot.layout = c(2,2), y.zero.center =F)
gbm.plot(gbm.object, n.plots=2, plot.layout = c(3, 4), main=NA, write.title =F)





brt.MOD500.2018
# ratVI vs treesp DENSITY PLOTS 
{
  data2018 <- get(paste0('data.', sate, '.s2018.CD')) %>% 
    filter(VFC!=0) %>% 
    filter(complete.cases(.)) 
  modTFC <- lm(TFC ~ GMin, data2018)
  modVFC <- lm(VFC ~ GMin + GMax, data2018)
  modTPV <- lm(TPV ~ GMin + GMax, data2018)
  data2018$TFC_p <- predict(modTFC, data2018)
  data2018$VFC_p <- predict(modVFC, data2018)
  data2018$TPV_p <- predict(modTPV, data2018)
  # data2018$TPV_p <- data2018$TFC_p/data2018$VFC_p
  # library(pracma)
  # data2018$TFC_p2 <- odregress(data.matrix(data2018[c('GMin', 'GMax')]), data2018$TFC)$fitted
  # data2018$VFC_p2 <- odregress(data.matrix(data2018[c('GMin', 'GMax')]), data2018$VFC)$fitted
  # data2018$TPV_p2 <- odregress(data.matrix(data2018[c('GMin', 'GMax')]), data2018$VFC)$fitted
  data.all.MOD500$TFC_p <- predict(modTFC, data.all.MOD500)
  data.all.MOD500$VFC_p <- predict(modVFC, data.all.MOD500)
  data.all.MOD500$TPV_p <- predict(modTPV, data.all.MOD500)
  xs <- c('TFC', 'VFC', 'TPV')
  ys <- c('TFC_p', 'VFC_p', 'TPV_p')
  # ys <- c('TFC_p2', 'VFC_p2', 'TPV_p2')
  for(i in seq(1, 3)){
    library(hexbin)
    i <- 3
    # names(data.all.MOD500)[c(1,2)] <- c('long', 'lat')
    x <- xs[i] #'Vege', 'Treesp', 'Trees', 'Shrubs'
    y <- ys[i]
    datap <- data2018 %>%
      dplyr::select(x, y) %>% #%>% #'minVI', 'maxVI'
      #filter(get(x)>0, get(x)<1, get(y)>0, get(y)<1)
      filter(complete.cases(.))
    
    names(datap) <- c('x', 'y')
    
    # summary(datap)
    rmse <- sqrt(mean((datap$y-datap$x)^2, na.rm=T))
    mae <- mean(abs(datap$y-datap$x), na.rm=T)
    # me <- mean(datap$y-datap$x, na.rm=T)
    
    
    
    if(i==1){
      xlim <- c(0, 1)
      ylim <- c(0, 1)
      limits <- c(1, 5)
      xp <- c(0.55, 0.55, 0.55)
      yp <- c(0.3, 0.2, 0.1)
    } else if(i==2){
      xlim <- c(0, 1)
      ylim <- c(0, 1)
      limits <- c(1, 8)
      xp <- c(0.45, 0.45, 0.45)
      yp <- c(0.6, 0.5, 0.4)
    } else {
      xlim <- c(0, 1)
      ylim <- c(0, 1)
      limits <- c(1, 5)
      xp <- c(0.45, 0.45, 0.45)
      yp <- c(0.7, 0.6, 0.5)
    }
    
    xbreaks <- c(0, 0.2, 0.4, 0.6, 0.8, 1.0)
    ybreaks <- c(0, 0.2, 0.4, 0.6, 0.8, 1.0)
    xrang <- xlim
    yrang <- ylim
    
    
    p <- ggplot(datap,aes(x=x,y=y)) +
      geom_bin2d(bins=50) +
      coord_fixed(ratio = 1, xlim=xlim, ylim=ylim) +
      scale_y_continuous(limits = ylim, breaks=ybreaks) + scale_x_continuous(limits = xlim, breaks=xbreaks) +
      xlab(x) + ylab(y) +
      scale_fill_gradientn(colours=c("#313695", "#4575B4", "#74ADD1", "#ABD9E9", "#E0F3F8", "#FFFFBF",
                                     "#FEE090", "#FDAE61", "#F46D43", "#D73027", "#A50026") ,
                           name = "Frequency", limits=limits, oob=squish,
                           na.value=NA) +  #limits=c(1, 1500)
      # geom_abline(intercept=0, slope=1, linetype=2, lwd=1)
      theme_bw() +
      theme(panel.grid = element_blank(), axis.text.x = element_text(colour="black"),
            axis.text.y = element_text(colour="black"),
            legend.position = 'top',
            legend.key = element_rect(fill='white'),
            legend.box.spacing = unit(0, 'cm'),
            # legend.box.margin=ggplot2::margin(0, 0, -0.25, 0), #margin(top, right, bottom, left) think trouble
            # legend.key.width = unit(0.8, 'cm'), legend.key.height = unit(0.2, 'cm'),
            legend.text = element_text(size=8),
            legend.title = element_text(size=8)) +
      guides(fill = guide_colorbar(title.position = 'left', label.position = 'top',
                                   barwidth = 5, barheight = 0.3, title.vjust = -0.008)) +
      # guides(fill)
      # geom_text(x = xp[1], y = yp[1], label = eq, parse = TRUE, size=3) +
      # geom_text(x = xp[2], y = yp[2], label = re, parse = TRUE, size=3) +
      # geom_smooth(method = "lm", color = 'black') # lm_eqn(datap, y, x)
      geom_abline(intercept = 0, slope=1)
    # png(filename = paste0(pathout, y, '_', x, '2.png'), width=2.5, height=2.5, units="in", res=300)
    # print(p)
    # dev.off()
    
    
    hexhist <- hexbin(datap$x, datap$y, xbins=30, xbnds = xrang, ybnds = yrang, IDs = T)
    count.cell <- data.frame(cell=hexhist@cell, count=hexhist@count)
    datap$cell <- hexhist@cID
    datap <- merge(datap, count.cell, by="cell")
    
    datap <- datap[order(datap$count),]
    p <- ggplot(datap,aes(x=x,y=y)) + 
      coord_fixed(ratio = 1, xlim=xlim, ylim=ylim) +
      scale_y_continuous(limits = ylim, breaks=ybreaks) + scale_x_continuous(limits = xlim, breaks=xbreaks) +
      xlab(x) + ylab(y) +
      geom_point(aes(colour=count), size=1, shape=19, na.rm=TRUE) +
      scale_color_gradientn(colours=c("#313695", "#4575B4", "#74ADD1", "#ABD9E9", "#E0F3F8", "#FFFFBF",
                                      "#FEE090", "#FDAE61", "#F46D43", "#D73027", "#A50026") ,
                            name = "Frequency", limits=limits, oob=squish,
                            na.value=NA) +  #limits=c(1, 1500)
      # geom_abline(intercept=0, slope=1, linetype=2, lwd=1)
      theme_bw() + 
      theme(panel.grid = element_blank(), axis.text.x = element_text(colour="black"), 
            axis.text.y = element_text(colour="black"), 
            legend.position = 'top',
            legend.key = element_rect(fill='white'), 
            legend.box.spacing = unit(0, 'cm'),
            # legend.box.margin=ggplot2::margin(0, 0, -0.25, 0), #margin(top, right, bottom, left) think trouble
            # legend.key.width = unit(0.8, 'cm'), legend.key.height = unit(0.2, 'cm'),
            legend.text = element_text(size=8), 
            legend.title = element_text(size=8)) +
      guides(color = guide_colorbar(title.position = 'left', label.position = 'top', 
                                    barwidth = 5, barheight = 0.3, title.vjust = -0.008)) +
      # guides(fill)
      # geom_text(x = xp[1], y = yp[1], label = eq, parse = TRUE, size=3) +
      # geom_text(x = xp[2], y = yp[2], label = re, parse = TRUE, size=3) +
      # geom_text(x = xp[3], y = yp[3], label = pe, parse = TRUE, size=3) +
      # geom_smooth(method = "lm", color = 'black') # lm_eqn(datap, y, x)
      geom_abline(y=x)
    png(filename = paste0(pathout, y, '_', x, 'test.png'), width=2.5, height=2.5, units="in", res=300)
    print(p)
    dev.off()
  }
}




#Calculate the trajectory of GMin and GMax dense Evergreen trees
{
  # tree1 <- readOGR('bufferarea/tree1.shp')
  # tree2 <- readOGR('bufferarea/tree2.shp')
  # tree3 <- readOGR('bufferarea/tree3.shp')
  # tree4 <- readOGR('bufferarea/tree4.shp')
  # tree5 <- readOGR('bufferarea/tree5.shp')
  # trees <- rbind(tree1, tree2, tree3, tree4, tree5)
  trees <- readOGR('bufferarea/tree6.shp')
  plot(trees)
  raster <- stack('bufferarea/MODIS_Ponil_Complex_minVI_sinu.tif') 
  GMin <- raster::extract(raster, trees) #as(trees, 'SpatialPolygons')
  GMin <- t(ldply(GMin, data.frame))
  names(GMin) <- as.character(seq(2001, 2018))
  raster <- stack('bufferarea/MODIS_Ponil_Complex_maxVI_sinu.tif') 
  GMax <- raster::extract(raster, trees) #as(trees, 'SpatialPolygons')
  GMax <- t(ldply(GMax, data.frame))
  names(GMax) <- as.character(seq(2001, 2018))
  
  databuffer <- data.frame(year = seq(2001, 2018))
  databuffer$GMin <- rowMeans(GMin, na.rm=T)
  databuffer$GMax <- rowMeans(GMax, na.rm=T)
  
  dataref <- databuffer %>% filter(year!=2002)
  dataref$GMin <- mean(dataref$GMin)/dataref$GMin
  dataref$GMax <- mean(dataref$GMax)/dataref$GMax
  names(dataref) <- c('year', 'GMin_ref', 'GMax_ref')
  # plot(GMax~year, databuffer)
  # plot(GMin~year, databuffer)
}


# ANOVA test of Burn severity and TPV 
{
  sate = 'MOD500' # 'HLS', 'MOD250', 'MOD500'
  data <- get(paste0('data.', sate, '.s2018.CD'))
  data <- data %>% 
    filter(VFC>0, EOS<366, EOS>200, SOS > 70, SOS<220) %>% 
    filter(BS %in% c(1, 2, 3, 4)) %>% 
    filter(complete.cases(.))
  data$BS <- as.factor(data$BS)
  ano1 <- aov(VFC ~ BS, data = data)
  summary(ano1)
  ano2 <- aov(TPV ~ BS, data = data)
  summary(ano1)
  p <- ggplot(data) +
    geom_boxplot(aes(x=BS, y=VFC), notch = T) +
    labs(x='Burn Severity', y='VFC)') +
    theme_bw() +  theme(panel.grid = element_blank(),
                        # axis.title.x = element_blank(),
                        # axis.ticks.x = element_blank(),
                        # axis.text.x = element_blank(),
                        axis.text.x = element_text(colour="black", size=10),
                        axis.text.y = element_text(colour="black", size=10),
                        axis.title = element_text(colour="black", size=10), legend.position = 'none')
  
  png(filename = paste0(pathout, 'Boxplot_MOD500_VFC_BS.png'), width = 2, height=3, units="in", res=300, type="cairo")
  print(p)
  dev.off()
  
  
  p <- ggplot(data) +
    geom_boxplot(aes(x=BS, y=TPV), notch = T) +
    labs(x='Burn Severity', y='TPV)') +
    theme_bw() +  theme(panel.grid = element_blank(),
                        # axis.title.x = element_blank(),
                        # axis.ticks.x = element_blank(),
                        # axis.text.x = element_blank(),
                        axis.text.x = element_text(colour="black", size=10),
                        axis.text.y = element_text(colour="black", size=10),
                        axis.title = element_text(colour="black", size=10), legend.position = 'none')
  
  png(filename = paste0(pathout, 'Boxplot_MOD500_TPV_BS.png'), width = 2, height=3, units="in", res=300, type="cairo")
  print(p)
  dev.off()
  
}


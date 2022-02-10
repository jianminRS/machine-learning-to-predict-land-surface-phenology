categories <- data.frame(var = c('TPV', 'VFC', 
                                 'GDD', 'CD', 'TMax', 'TMin', 'Prcp', 'SRad', 
                                 'FFD', 'LFD', 'DF', 'FSD', 'LSD', 'DS', 
                                 'Elevation', 'Slope', 'Eastness', 'Northness', 
                                 'BS', 'SOS'), 
                         category = c('LCC', 'LCC', 
                                      'preseason', 'preseason', 'preseason', 'preseason', 'preseason', 'preseason', 
                                      'events', 'events', 'events', 'events', 'events', 'events', 
                                      'topo', 'topo', 'topo', 'topo', 
                                      'BS', 'SOS'))

conts <- left_join(conts.MOD500.SOS, categories, by='var')
conts <- left_join(conts.MOD500.EOS, categories, by='var')
conts <- left_join(conts.HLS.SOS, categories, by='var')
conts <- left_join(conts.HLS.EOS, categories, by='var')
conts <- left_join(conts.MOD500.SOS.inter, categories, by='var')
conts <- left_join(conts.MOD500.EOS.inter, categories, by='var')
conts %>% group_by(category) %>% 
  summarise(total.inf = sum(rel.inf, na.rm=T)  )

### Plot the variable importance 
for(sate in c('HLS', 'MOD500')){ #c('HLS', 'MOD500')
  for(phen in c('SOS', 'EOS')){
    # sate <-'HLS' #'HLS' #'MOD500'
    # phen <- 'SOS'
    
    if(sate=='MOD500') {
      brtmodel <- get(paste0('brt.MOD500.2018.alldata.', phen, '.tc5.slope10.refined'))
    } else if(sate=='HLS') {
      brtmodel <- get(paste0('brt.HLS.2018.subset.', phen, '.tc5.slope0.refined'))
    }
    
    R2 <- 1-brtmodel$self.statistics$mean.resid/brtmodel$self.statistics$mean.null
    R2 <- 1-brtmodel$cv.statistics$deviance.mean/brtmodel$self.statistics$mean.null
    print(R2)
    
    conts <- brtmodel$contributions
    conts$var <- gsub('[[:digit:]]+', '', conts$var)
    conts$var <- gsub('GMax', 'VFC', conts$var)
    conts$var <- gsub('GRatio', 'TPV', conts$var)
    
    n <- nrow(conts)
    conts$rank <- seq(1, n)
    assign(paste0('conts.', sate, '.', phen), conts)
    # 
    # conts$var <- factor(conts$var, levels=rev(conts$var))
    # thre.imp <- 100/n
    # # if(thre.imp > 5) {thre.imp <- 5}
    # p <- ggplot(conts) + 
    #   geom_bar(mapping=aes(x=var, y=rel.inf, fill=rank), color='black', size=0.1, stat = 'identity') +
    #   xlab('') + ylab('Relative importance (%)') +
    #   scale_fill_gradientn(colours = rev(c('#edf8b1', '#7fcdbb', '#2c7fb8'))) + 
    #   geom_hline(yintercept=thre.imp, linetype = 'dashed', size=0.4) +
    #   theme_bw() +
    #   scale_y_continuous(expand=c(0,0)) +
    #   theme(panel.grid = element_blank(), 
    #         panel.border = element_blank(),
    #         # axis.text.x = element_text(colour="black"),
    #         # axis.text.y = element_text(colour="black"),
    #         axis.text = element_text(colour="black", size=8),
    #         axis.title.x = element_text(colour="black", size = 8),
    #         axis.line.x = element_line(colour='black'), 
    #         axis.ticks.length = unit(0, 'cm'),
    #         legend.position = "none") +
    #   coord_flip()
    # filename <- paste0('thre2.importance_', sate, '_', phen, '.png')
    # png(filename = filename, width=1.9, height=0.1178571*(n+2.5), units="in", res=300)
    # print(p)
    # dev.off()
  }
}

##Interannual model
for(phen in c('SOS', 'EOS')){
  # sate <-'HLS' #'HLS' #'MOD500'
  # phen <- 'SOS'
  brtmodel <- get(paste0('brt.MOD500.alldata.normall.', phen, '.tc5.rowm17.lr125.BS1234.snowfreeze.refined'))
  
  R2 <- 1-brtmodel$self.statistics$mean.resid/brtmodel$self.statistics$mean.null
  R2 <- 1-brtmodel$cv.statistics$deviance.mean/brtmodel$self.statistics$mean.null
  print(R2)
  
  conts <- brtmodel$contributions
  conts$var <- gsub('[[:digit:]]+', '', conts$var)
  conts$var <- gsub('GMax', 'VFC', conts$var)
  conts$var <- gsub('GRatio', 'TPV', conts$var)
  conts$var <- factor(conts$var, levels=rev(conts$var))
  n <- nrow(conts)
  conts$rank <- seq(1, n)
  thre.imp <- 100/n
  # if(thre.imp > 5) {thre.imp <- 5}
  p <- ggplot(conts) + 
    geom_bar(mapping=aes(x=var, y=rel.inf, fill=rank), color='black', size=0.1, stat = 'identity') +
    xlab('') + ylab('Relative importance (%)') +
    scale_fill_gradientn(colours = rev(c('#edf8b1', '#7fcdbb', '#2c7fb8'))) +
    geom_hline(yintercept=thre.imp, linetype = 'dashed', size=0.4) +
    theme_bw() +
    scale_y_continuous(expand=c(0,0)) +
    theme(panel.grid = element_blank(), 
          panel.border = element_blank(),
          # axis.text.x = element_text(colour="black"),
          # axis.text.y = element_text(colour="black"),
          axis.text = element_text(colour="black", size=8),
          axis.title.x = element_text(colour="black", size = 8),
          axis.line.x = element_line(colour='black'), 
          axis.ticks.length = unit(0, 'cm'),
          legend.position = "none") +
    coord_flip()
  filename <- paste0('thre2.importance_', sate, '_', phen, '_inter.png')
  png(filename = filename, width=1.9, height=0.1178571*(n+2.5), units="in", res=300)
  print(p)
  dev.off()
  assign(paste0('conts.', sate, '.', phen,'.inter'), conts)
}



###Partial Dependence Plots
for(model in c('spatial', 'interannual')){
  for(phen in c('SOS', 'EOS')){
    # model <- 'spatial'
    # phen <- 'SOS'
    filename <- paste0(pathout, plotm, 'PDP.', sate,'.', phen,'.',model, 'pred.png') 
    if(model=='spatial') {
      brt.model <- get(paste0('brt.MOD500.2018.alldata.', phen, '.tc5.slope10.refined'))
    } else if(model=='interannual'){
      brt.model <- get(paste0('brt.MOD500.alldata.normall.', phen, '.tc5.rowm17.lr125.BS1234.snowfreeze.refined'))
    }
    # brt.model <- brt.HLS.2018.subset.SOS.tc5.slope0.refined
    
    gbm.plot(brt.model, n.plots=16, plot.layout = c(4, 4), main=NA, write.title =F,
             smooth=T)
    conts <- brt.model$contributions
    conts$var <- as.character(conts$var)
    conts$var.new <- gsub('[[:digit:]]+', '', conts$var)
    conts$var.new <- gsub('GMax', 'VFC', conts$var.new)
    conts$var.new <- gsub('GRatio', 'TPV', conts$var.new)
    
    conts.filter <- conts %>% 
      filter(rel.inf > 100/nrow(conts) | var.new=='TPV' | var.new=='VFC' | var.new=='SOS') #| var.new=='SOS'
    variables <- conts.filter$var
    ncol <- 2
    nrow <- ceiling(nrow(conts.filter)/ncol)
    plot.layout <- c(nrow, ncol)
    y.label <- paste0('predicted ', phen) #fitted
    
    
    
    
    png(filename = filename, width=3.25, height=nrow*1.1, units="in", res=300)
    gbm.plot2(brt.model, variables = variables, plot.layout = plot.layout,
              smooth=T, rug=T, y.zero.center =F, 
              widths = c(122, 100),
              y.label=y.label, x.label = conts.filter$var.new,
              common.scale=T, common.y.axis=T, show.contrib = T)
    dev.off()
  }
}







##### Interactions in the models 
for(model in c('spatial', 'interannual')){
  for(phen in c('SOS', 'EOS')){
    # model <- 'interannual'
    # phen <- 'SOS'
    # filename <- paste0(pathout, plotm, 'PDP.', sate,'.', phen,'.',model, '.png') 
    if(model=='spatial') {
      brt.model <- get(paste0('brt.MOD500.2018.alldata.', phen, '.tc5.slope10.refined'))
    } else if(model=='interannual'){
      brt.model <- get(paste0('brt.MOD500.alldata.normall.', phen, '.tc5.rowm17.lr125.BS1234.snowfreeze.refined'))
    }
    
    find.int <- gbm.interactions(brt.model)
    find.int
    find.int$interactions
    find.int$rank.list
    gbm.perspec(brt.model, 2, 1, z.range=c(-2, 2))
    
    
    # png(filename = filename, width=3.25, height=nrow*1.1, units="in", res=300)
    # gbm.plot2(brt.model, variables = variables, plot.layout = plot.layout,
    #           smooth=T, rug=T, y.zero.center =F, 
    #           widths = c(122, 100),
    #           y.label=y.label, x.label = conts.filter$var.new,
    #           common.scale=T, common.y.axis=T, show.contrib = T)
    # dev.off()
  }
}





















#Comparisons between HLS and MODIS
{
  raster <- stack('LSP_HLS30_2_480_2018.tif') #LSP_HLS30_2_480_2018_VFC005.tif
  LSP.HLSscaled <- as.data.frame(raster, xy=T, na.rm=T, centroids=T, optional=F)
  names <- c(paste0('SOS_p', seq(10, 90, 5)), paste0('EOS_p', seq(10, 90, 5)))
  names(LSP.HLSscaled) <- c('long', 'lat',  names)
  
  LSP.combine <- data.MOD500.s2018.CD %>% 
    dplyr::select(long, lat, SOS, EOS, TPV, VFC, GMin, GMax) %>% 
    left_join(LSP.HLSscaled, by=c('long', 'lat')) #%>% 
    # mutate(GDiff = GMax - GMin) %>% 
    # mutate(SOSDiff = abs(SOS-SOS_p40)) %>%
    # filter(SOSDiff>30) %>%
    # dplyr::select(GMin, GMax, GDiff,TPV, VFC, SOS, SOS_p40, SOSDiff)
  View(LSP.combine)
  hist(LSP.combine$GDiff)
  hist(data.MOD500.s2018.CD$TPV)
  hist(data.MOD500.s2018.CD$VFC)
  mean(data.MOD500.s2018.CD$TPV, na.rm=T)
  mean(data.MOD500.s2018.CD$VFC, na.rm=T)
  
  data.MOD500.s2018.CD %>% filter(VFC==0) %>% dplyr::select(TPV, VFC)
  
  ###with this paragraph the optimal percentage of 40% and 60% for SOS and EOS
  for(per in seq(10, 90, 5)){
    x <- 'EOS'
    y <- paste0(x, '_p', per)
    filename <- paste0(pathout, 'HLS_MODIS_comp/test1_', y, '_', x, '.png')
    if(!((x=='SOS'&per==40)|(x=='EOS'&per==60))){next}
    if(x=='SOS') {
      xlim <- c(70, 210)
      ylim <- c(70, 210)
      xbreaks <- c(90, 120, 150, 180, 210)
      ybreaks <- c(90, 120, 150, 180, 210)
      xrang <- xlim
      yrang <- ylim
      limits = c(1, 40)
      xp <- c(170, 167)
      yp <- c(90, 75)
    } 
    if(x=='EOS') {
      xlim <- c(200, 366)
      ylim <- c(200, 366)
      xbreaks <- c(210, 240, 270, 300, 330, 360)
      ybreaks <- c(210, 240, 270, 300, 330, 360)
      xrang <- xlim
      yrang <- ylim
      limits = c(1, 40)
      xp <- c(247, 245)
      yp <- c(360, 340)
    } 
    datap <- LSP.combine %>%
      dplyr::select(x, y) %>% #%>% #'minVI', 'maxVI'
      #filter(get(x)>0, get(x)<1, get(y)>0, get(y)<1)
      filter(complete.cases(.))
    
    names(datap) <- c('x', 'y')
    y <- paste0('aggregated HLS ', x)
    x <- paste0('MODIS ', x)
    # summary(datap)
    rmse <- sqrt(mean((datap$y-datap$x)^2, na.rm=T))
    mad <- mean(abs(datap$y-datap$x), na.rm=T)
    md <- mean(datap$y-datap$x, na.rm=T)
    
    print(paste0('perc=', per, ' rmse=', rmse,  ' mad=', mad, ' md=', md ))
    
    p <- ggplot(datap,aes(x=x,y=y)) +
      geom_bin2d(bins=50) +
      coord_fixed(ratio = 1) +
      scale_y_continuous(limits = ylim, breaks=ybreaks) + scale_x_continuous(limits = xlim, breaks=xbreaks) +
      xlab(x) + ylab(y) +
      scale_fill_gradientn(colours=c("#313695", "#4575B4", "#74ADD1", "#ABD9E9", "#E0F3F8", "#FFFFBF",
                                     "#FEE090", "#FDAE61", "#F46D43", "#D73027", "#A50026") ,
                           name = "Frequency", na.value=NA, limits=limits, oob=squish) +  #limits=c(1, 1500)
      # geom_abline(intercept=0, slope=1, linetype=2, lwd=1)
      theme_bw() +
      theme(panel.grid = element_blank(), 
            # axis.text.x = element_text(colour="black"),
            # axis.text.y = element_text(colour="black"),
            axis.text = element_text(colour="black", size=8),
            axis.title = element_text(colour="black", size = 8),
            legend.position = 'top',
            legend.key = element_rect(fill='white'),
            legend.box.spacing = unit(0, 'cm'),
            # legend.box.margin=ggplot2::margin(0, 0, -0.25, 0), #margin(top, right, bottom, left) think trouble
            # legend.key.width = unit(0.8, 'cm'), legend.key.height = unit(0.2, 'cm'),
            legend.text = element_text(size=8),
            legend.title = element_text(size=8)) +
      geom_text(x = xp[1], y = yp[1], label = paste0('MAD = ', round(mad, 1), ' days'), parse = F, size=3) +
      geom_text(x = xp[2], y = yp[2], label = paste0('MD = ', round(md, 1), ' days'), parse = F, size=3)+
      guides(fill = guide_colorbar(title.position = 'left', label.position = 'top',
                                   barwidth = 5, barheight = 0.3, title.vjust = -0.008)) +
      geom_abline(intercept = 0, slope=1, linetype=2)
    png(filename = filename, width=2.5, height=2.5, units="in", res=300)
    print(p)
    dev.off()
    
  }
  
  
}





raster_map <- function(ras, maxpixels=500000, legname, limits, val, brk, lbl, filename, width, height){
  p <- gplot(ras, maxpixels=maxpixels) +
    geom_raster(aes(fill=value))  +
    # scale_fill_gradientn(colours = terrain.colors(10), na.value = 'white')+ 
    scale_fill_gradientn(name=legname, colours=clr, limits=limits, oob=squish, 
                         values=val, breaks=brk, labels=lbl, na.value = "white", space='rgb', guide = 'colourbar') + 
    theme_nothing(legend = TRUE) +
    theme(legend.key = element_rect(fill='white'), legend.box.margin=ggplot2::margin(-10, 0, -10, -10), #margin(-10, 0, -10, -22)
          legend.key.height = unit(1.2, "lines"), legend.key.width = unit(0.4, "lines"), 
          legend.text = element_text(size=10, margin=ggplot2::margin(0, 0, 0, 0))) +
    # guides(fill=guide_colorbar(label.hjust = 0))+
    coord_equal()
  
  
  # png(filename = paste0(pathout, 'SRTM_VNP.png'), width = width, height=height, units="in", res=300)
  png(filename = filename, width = width, height=height, units="in", res=300)
  print(p)
  dev.off()
}


####Plot out the VFC and TPV at 30 and 500 m
{
  ras3m <- raster('landcover_3m.tif') 
  tmp <- raster('landcover_3m_masked_ENVI')
  # length(values(ras3m))
  vals <- values(ras3m)
  tmp <- values(tmp)
  tmp[is.na(vals)] <- NA
  tmp <- tmp + 1
  tmpind <- !is.na(tmp)
  vals[tmpind] <- tmp[tmpind]
  values(ras3m) <- vals
  plot(ras3m)
}
{
  sate <- 'MOD500' #'HLS'  'MOD500'
  phen <- 'GMax' #'VFC', 'TPV', 'GRatio, 'GMax'
  if(sate =='MOD500') { file <- 'LSP_MOD500_lstwinterJ4/LSP_model_MOD500_2018.tif'
  } else if (sate=='HLS'){ file <- 'LSP_model_HLS_2018.tif'
  }
  if(sate =='MOD500') { nscale <- 160
  } else if (sate=='HLS'){ nscale <- 10
  }
  
  if(phen=='TPV'|phen=='VFC'){
    ras1 <- aggregate(ras3m, fact=nscale, 
                      fun=function(vals, na.rm) {
                        sum(vals==2, na.rm=na.rm)/length(vals)
                      })
    ras2 <- aggregate(ras3m, fact=nscale, 
                      fun=function(vals, na.rm) {
                        sum(vals==3, na.rm=na.rm)/length(vals)
                      })

    # ras1 <- raster(file, band=6)
    # ras2 <- raster(file, band=7)
    
    ras <- ras1
    values(ras) <- values(ras1) + values(ras2)
    # ras <- rasTFC + rasSFC
    # bound <- readOGR('Ponil_Complex_refined/Ponil_Complex_refinedshape.shp')
    # bound <- spTransform(x=bound, CRSobj = crs(ras))
    ras <- mask(x=ras, mask=bound)
    if(phen == 'TPV') {
      values(ras) <- values(ras1)/values(ras)
      # values[values(ras)==0] <- NA #it will automatically do this in R 0/0= NaN
      ras <- mask(x=ras, mask=bound)
    }
  } else if(phen=='GRatio'|phen=='GMax'){
    ras1 <- raster(file, band=3)
    ras2 <- raster(file, band=4)
    ras <- ras2
    values(ras) <- values(ras2)/10000
    if(phen=='GRatio') {
      values(ras) <- values(ras1)/values(ras2)
      # values[values(ras2)==0] <- NA #it will automatically do this in R 0/0= NaN
    }
  }
    
  
  envi33 <- read.table('/hunter/data/jianmin/codes/R_codes/rgb33_envi.txt', header=T)
  # envi33 <- read.table('C:/Users/Jianmin.Wang/Google Drive/2research/R codes/rgb33_envi.txt', header=T)
  clrs_envi33 <- rgb(envi33$r, envi33$g, envi33$b, maxColorValue=255)
  if(phen=='TPV'|phen=='VFC'){
    minscale <- 0
    maxscale <- 1
    brk <- c(0, 0.25, 0.5, 0.75, 1.0)
    lbl <- as.character(brk)
  } else if(phen=='GMax'){
    minscale <- 0.2
    maxscale <- 0.4
    brk <- c(0.2, 0.25, 0.3, 0.35, 0.4)
    lbl <- c('\u2264 0.2', '0.25',  '0.3', '0.35', '\u2265 0.4')
    minscale <- 0.15
    maxscale <- 0.3
    brk <- c(0.15, 0.2, 0.25, 0.3)
    lbl <- c('\u2264 0.15', '0.2', '0.25',  '\u2265 0.3')
    minscale <- 0.15
    maxscale <- 0.35
    brk <- c(0.15, 0.2, 0.25, 0.3, 0.35)
    lbl <- c('\u2264 0.15', '0.2', '0.25', '0.3', '\u2265 0.35')
    # minscale <- 0.2
    # maxscale <- 0.35
    # brk <- c(0.2, 0.25, 0.3, 0.35)
    # lbl <- c('\u2264 0.2', '0.25', '0.3', '\u2265 0.35')
  } else if(phen == 'GRatio'){
    minscale <- 0.4
    maxscale <- 0.8
    brk <- c(0.4, 0.5, 0.6, 0.7, 0.8)
    lbl <- c('\u2264 0.4', '0.5', '0.6',  '0.7', '\u2265 0.8')
    minscale <- 0.45
    maxscale <- 0.8
    brk <- c(0.45, 0.5, 0.6, 0.7, 0.8)
    lbl <- c('\u2264 0.45', '0.5',  '0.6', '0.7', '\u2265 0.8')
  }
  
  val <- seq(minscale, maxscale, length.out = 256)
  val <- scales::rescale(val)
  clr <- clrs_envi33[scales::rescale(val, to=c(1, 256))]
  
  filename <- paste0(pathout, 'new_', sate, '_', phen, '2018.png')
  legname <- phen
  width <- 3
  height <- 3
  raster_map(ras=ras, maxpixels=500000, legname=legname, 
             limits=c(minscale, maxscale), val=val, brk=brk, lbl=lbl, 
             filename=filename, width=width, height=height)
  
  
}





###Plot out the rasters GMin, GMax, SOS, EOS, Elevation
{
  {
    sate <- 'MOD500'
    if(sate =='MOD500') {
      file <- 'LSP_MOD500_lstwinterJ4_preseason_snowfreeze_meantpsp/LSP_preseasonmodel_MOD500_2018.tif'
      phens <- c('SOS', 'EOS', 'GMin', 'GMax')
      bands <- c(1, 2, 3, 4)
    } else if (sate=='HLS'){
      file <- 'LSP_preseasonmodel_HLS_2018.tif'
      phens <- c('SOS', 'EOS', 'GMin', 'GMax', 'Elevation')
      bands <- c(1, 2, 3, 4, 9)
    }
    
    SOS <- raster(file, band=1)
    EOS <- raster(file, band=2)
    valSOS <- values(SOS)
    valEOS <- values(EOS)
    if(sate=='MOD500') {
      TFC <- raster(file, band=6)  
      SFC <- raster(file, band=7)  
      VFC <- values(TFC) + values(SFC)
      tmpind <- VFC <=0 | valSOS <= 70 | valSOS >= 220 | valEOS <= 200 | valEOS >= 366
    } else if(sate=='HLS'){
      TFC <- raster(file, band=6)  
      SFC <- raster(file, band=7)  
      VFC <- values(TFC) + values(SFC)
      tmpind <- VFC <=0 | valSOS <= 70 | valSOS >= 220 | valEOS <= 200 | valEOS >= 366
    }
    valSOS[tmpind] <- NA
    valEOS[tmpind] <- NA
    values(SOS) <- valSOS
    values(EOS) <- valEOS

    raster <- stack(file)
  }
  
  envi33 <- read.table('/hunter/data/jianmin/codes/R_codes/rgb33_envi.txt', header=T)
  clrs_envi33 <- rgb(envi33$r, envi33$g, envi33$b, maxColorValue=255)
  for(phen in phens){
    # phen <- 'SOS'
    width <- 3
    height <- 3
    if(phen=='SOS') {
      minscale <- 70
      maxscale <- 220
      brk <- c(90, 120, 150, 180, 210)
      lbl <- c('90', '120', '150', '180', '210')
      val <- seq(minscale, maxscale, length.out = 256)
      val <- scales::rescale(val)
      clr <- clrs_envi33[scales::rescale(val, to=c(1, 256))]
      ras <- SOS
      legname <- 'SOS\n(DOY)'
    } else if(phen=='EOS'){
      minscale <- 200
      maxscale <- 366
      brk <- c(210, 240, 270, 300, 330, 360)
      lbl <- c('210', '240', '270', '300', '330', '360')
      val <- seq(minscale, maxscale, length.out = 256)
      val <- scales::rescale(val)
      clr <- clrs_envi33[scales::rescale(val, to=c(1, 256))]
      ras <- EOS
      legname <- 'EOS\n(DOY)'
    } else if(phen=='GMin'){
      minscale <- 0.1
      maxscale <- 0.3
      brk <- c(0.1, 0.15, 0.2, 0.25, 0.3)
      lbl <- c('\u2264 0.1', '0.15', '0.2', '0.25', '\u2265 0.3')
      val <- seq(minscale, maxscale, length.out = 256)
      val <- scales::rescale(val)
      clr <- clrs_envi33[scales::rescale(val, to=c(1, 256))]
      ras <- raster[[bands[which(phens==phen)]]]/10000
      vals <- values(ras)
      vals[tmpind] <- NA
      values(ras) <- vals
      legname <- 'GMin\n(EVI2)'
    } else if(phen=='GMax'){
      if(sate=='MOD500'){
        minscale <- 0.2
        maxscale <- 0.4
        brk <- c(0.2, 0.25, 0.3, 0.35, 0.4)
        lbl <- c('\u2264 0.2', '0.25',  '0.3', '0.35', '\u2265 0.4')
      } else if(sate=='HLS'){
        minscale <- 0.2
        maxscale <- 0.4
        brk <- c(0.2, 0.25, 0.3, 0.35, 0.4)
        lbl <- c('\u2264 0.2', '0.25',  '0.3', '0.35', '\u2265 0.4')
      }
      val <- seq(minscale, maxscale, length.out = 256)
      val <- scales::rescale(val)
      clr <- clrs_envi33[scales::rescale(val, to=c(1, 256))]
      ras <- raster[[bands[which(phens==phen)]]]/10000
      vals <- values(ras)
      vals[tmpind] <- NA
      values(ras) <- vals
      legname <- 'GMax\n(EVI2)'
    } else if(phen=='Elevation'){
      minscale <- 2000
      maxscale <- 2800
      brk <- c(2000, 2200, 2400, 2600, 2800)
      lbl <- c('2000', '2200',  '2400', '2600', '2800')
      val <- seq(minscale, maxscale, length.out = 256)
      val <- scales::rescale(val)
      clr <- clrs_envi33[scales::rescale(val, to=c(1, 256))]
      ras <- raster[[bands[which(phens==phen)]]]
      legname <- 'Elevation\n(m)'
      width <- 4.5
      height <- 4.5
    } 
    filename <- paste0(pathout, sate, '_', phen, '2018.png')
    raster_map(ras=ras, maxpixels=500000, legname=legname, 
               limits=c(minscale, maxscale), val=val, brk=brk, lbl=lbl, 
               filename=filename, width=width, height=height)
  }
}



### Plot the classification map 3 m
{
  maxpixels <- 500000
  ras <- raster('landcover_3m.tif')  #landcover_3m_masked_ENVI_refined
  filename <- paste0(pathout, 'landcover_3m_newcolor.png')
  colors <- c('#B5AFA2', 'forestgreen', 'lawngreen', '#486DA2')  #c('#B5AFA2', '#006633', '#B49C46', '#486DA2') 
  values <- values(ras)
  values[values==0] <- NA
  values(ras) <- values
  p <- gplot(ras, maxpixels=maxpixels) +
    geom_raster(aes(fill=factor(value)))  +
    scale_fill_manual(name='Class', breaks=factor(seq(1,3)), values =colors, labels=c('Soil', 'Tree', 'Shrub') )+  #, 'Water'
    theme_nothing(legend = TRUE) +
    theme(legend.key = element_rect(fill='white'), legend.box.margin=ggplot2::margin(-10, 0, -10, -15), #margin(-10, 0, -10, -22)
          legend.key.height = unit(1.2, "lines"), legend.key.width = unit(0.4, "lines"), 
          legend.text = element_text(size=10, margin=ggplot2::margin(0, 0, 0, 0))) +
    # guides(fill=guide_colorbar(label.hjust = 0))+
    coord_equal()
  
  width <- 3.2
  height <- 3
  png(filename = filename, width = width, height=height, units="in", res=300)
  print(p)
  dev.off()
}



### Plot the burn severity 
{
  maxpixels <- 50000
  file <- 'C:/Users/Jianmin.Wang/Google Drive/2research/fire_phen_driver/Thesis3/data/LSP_preseasonmodel_HLS_2018.tif'
  ras <- raster(file, band=12)  #landcover_3m_masked_ENVI_refined
  filename <- paste0(pathout, 'BS_30m.png')
  colors <- rgb(c(0, 127, 255, 255, 127, 255), c(100, 255, 255, 0, 255, 255), 
                c(0, 212, 0, 0, 0, 255), max=255)
  values <- values(ras)
  values[values==0 ] <- NA
  values(ras) <- values
  
  fullrange <- readOGR('C:/Users/Jianmin.Wang/Google Drive/2research/fire_phen_driver/Thesis3/data/ponil_complex_originalshape/nm3668210504720020608_20010520_20031017_burn_bndy.shp')
  refinedrange <- readOGR('C:/Users/Jianmin.Wang/Google Drive/2research/fire_phen_driver/Thesis3/data/Ponil_Complex_refined/Ponil_Complex_refinedshape.shp')
  fullrange <- spTransform(fullrange, crs(ras))
  refinedrange <- spTransform(refinedrange, crs(ras))
  
  p <- gplot(ras, maxpixels=maxpixels) +
    geom_polygon(data=fullrange,aes(x = long, y = lat, group = group), colour='black', fill='grey', size=0.5) + 
    geom_raster(aes(fill=factor(value)))  +
    geom_polygon(data=refinedrange,aes(x = long, y = lat, group = group), colour='black', fill=NA, size=0.5) +
    scale_fill_manual(name='Burn Severity', breaks=factor(c(1, 2, 3, 4, 5, 6)), values =colors, labels=c('Unburned/Low', 'Low', 'Moderate', 'High', 'Increased-Greenness', 'Non-Processing') )+  #, 'Water'
    theme_nothing(legend = TRUE) +
    theme(legend.key = element_rect(linetype='solid', size=1, color='black'),
          legend.box.margin=ggplot2::margin(-10, 0, -10, -15), #margin(-10, 0, -10, -22)
          legend.key.height = unit(0.8, "lines"), 
          legend.key.width = unit(0.3, "lines"), 
          legend.text = element_text(size=10, margin=ggplot2::margin(0, 0, 0, 0))) +
    # guides(fill=guide_colorbar(label.hjust = 0))+
    coord_equal()
  print(p)
  width <- 3.5
  height <- 2.3
  png(filename = filename, width = width, height=height, units="in", res=300)
  print(p)
  dev.off()
}



### Plot the elevation 
{
  maxpixels <- 50000
  file <- 'C:/Users/Jianmin.Wang/Google Drive/2research/fire_phen_driver/Thesis3/data/LSP_preseasonmodel_HLS_2018.tif'
  ras <- raster(file, band=9)  #landcover_3m_masked_ENVI_refined
  filename <- paste0(pathout, 'Elevation.png')
  colors <- matlab.like2(7)
  colors <- rev(c('#f0f9e8', '#ccebc5', '#a8ddb5', '#7bccc4', '#4eb3d3', '#2b8cbe', '#08589e'))
  colors <- rev(c('#f6eff7', '#d0d1e6', '#a6bddb', '#67a9cf', '#3690c0', '#02818a', '#016450'))
  # fullrange <- readOGR('C:/Users/Jianmin.Wang/Google Drive/2research/fire_phen_driver/Thesis3/data/ponil_complex_originalshape/nm3668210504720020608_20010520_20031017_burn_bndy.shp')
  refinedrange <- readOGR('C:/Users/Jianmin.Wang/Google Drive/2research/fire_phen_driver/Thesis3/data/Ponil_Complex_refined/Ponil_Complex_refinedshape.shp')
  # fullrange <- spTransform(fullrange, crs(ras))
  refinedrange <- spTransform(refinedrange, crs(ras))
  
  p <- gplot(ras, maxpixels=maxpixels) +
    # geom_polygon(data=fullrange,aes(x = long, y = lat, group = group), colour='black', fill='grey', size=0.5) + 
    geom_raster(aes(fill=value/1000))  +
    geom_polygon(data=refinedrange,aes(x = long, y = lat, group = group), colour='black', fill=NA, size=0.5) +
    scale_fill_distiller(name='Elevation\n (km)', palette = "Spectral", na.value =NA) + 
    # scale_fill_gradientn(name='Elevation\n (km)', colors = colors, na.value =NA)+
    theme_nothing(legend = TRUE) +
    theme(legend.key = element_rect(linetype='solid', size=1, color='black'),
          legend.box.margin=ggplot2::margin(-10, 0, -10, -15), #margin(-10, 0, -10, -22)
          legend.key.height = unit(0.8, "lines"), 
          legend.key.width = unit(0.3, "lines"), 
          legend.text = element_text(size=10, margin=ggplot2::margin(0, 0, 0, 0))) +
    guides(fill=guide_colorbar(frame.colour = 'black'))+
    coord_equal()
  print(p)
  width <- 2.1
  height <- 1.6
  png(filename = filename, width = width, height=height, units="in", res=300)
  print(p)
  dev.off()
}






### Plot the location of Ponil Complex fire
fullrange <- readOGR('C:/Users/Jianmin.Wang/Google Drive/2research/fire_phen_driver/Thesis3/data/ponil_complex_originalshape/nm3668210504720020608_20010520_20031017_burn_bndy.shp')
bbox(fullrange)
fullrange <- spTransform(fullrange, CRS("+proj=longlat +datum=WGS84"))
centroid = gCentroid(fullrange)
# get the boundary box of a spatail data
pc.bbox <- bbox(fullrange)
xlim <- c(hm.bbox[1,1]-0.5, hm.bbox[1,2]+0.5)
ylim <- c(hm.bbox[2,1]-0.5, hm.bbox[2,2]+0.5)

map('state', lwd=3)
map('state', region = 'New Mexico', fill=T, add=T, col = 'gray80', lwd=4)  #
points(centroid, pch=20, col='red', cex=3)


map('state', region = 'New Mexico')  #, col='gray90'
map('county', 'New Mexico', add = F, fill=T, col='gray90') # , xlim=xlim, ylim=ylim
points(centroid, pch=20, col='red', cex=3)
# lines(fullrange, col='red')
map.cities(us.cities, country="NM", label=F, capital=2, cex=2, pch=20)
map.cities(us.cities, country="NM", label=F, minpop=350000, capital=0, cex=3.5, pch=20)






##Prepare the summary data
{
  summ <- data.all.MOD500 %>% 
    # filter(paste(long, lat, sep='_') %in% paste(ids$long, ids$lat, sep='_')) %>%
    group_by(year) %>% 
    summarise(n = n(), 
              mean_SOS = mean(SOS, na.rm=T),
              mean_EOS = mean(EOS, na.rm=T),
              mean_GMin = mean(GMin, na.rm=T),
              mean_GMax = mean(GMax, na.rm=T),
              mean_GRatio = mean(GRatio, na.rm=T),
              sd_SOS = sd(SOS, na.rm=TRUE), 
              sd_EOS = sd(EOS, na.rm=TRUE), 
              sd_GMin = sd(GMin, na.rm=TRUE), 
              sd_GMax = sd(GMax, na.rm=TRUE), 
              sd_GRatio = sd(GRatio, na.rm=TRUE)
              
    )
  
  summ <- as.data.frame(summ)
  
  
  ### add a year 2002 to break the line when plotting
  # tmp <- summ %>% filter(year==2001)
  # tmp$year <- 2002
  # tmp[!names(tmp) %in% c('year', 'BS')] <- NA
  # summ <- rbind(summ, tmp) 
  
  #add low and high
  summ$low_SOS <- summ$mean_SOS - summ$sd_SOS
  summ$high_SOS <- summ$mean_SOS + summ$sd_SOS
  summ$low_EOS <- summ$mean_EOS - summ$sd_EOS
  summ$high_EOS <- summ$mean_EOS + summ$sd_EOS
  summ$low_GMin <- summ$mean_GMin - summ$sd_GMin
  summ$high_GMin <- summ$mean_GMin + summ$sd_GMin
  summ$low_GMax <- summ$mean_GMax - summ$sd_GMax
  summ$high_GMax <- summ$mean_GMax + summ$sd_GMax
  summ$low_GRatio <- summ$mean_GRatio - summ$sd_GRatio
  summ$high_GRatio <- summ$mean_GRatio + summ$sd_GRatio
  
  
  tmp <- summ %>% filter(year==2001)
  tmp$year <- 2002
  tmp[!names(tmp) %in% c('year')] <- NA
  summ <- rbind(summ, tmp) 
  
}

## plot SOS and EOS together, GMin, GMax, and GRatio together 
## In different axes
## SE is too small. If want to plot with SD point plot might not be good
# Because the SD overlap. 
# Can try barplots with SD and sitting side by side
{
  summ$Year <- as.factor(substr(as.character(summ$year), 3, 4))
  {
    phen <- 'timing'
    colors <- c('SOS'='deepskyblue4', 'EOS'='gold4')
    colors <- c('SOS'='#80cdc1', 'EOS'='#dfc27d')
    diffaxis <- 155
    p <- ggplot(summ) + 
      geom_bar(aes(x=Year, y=mean_SOS, fill='SOS'), color="black", size=0.1,
               stat="identity", width = 0.4, position = position_nudge(x = -0.2)) + #color='black', , position=position_dodge()
      geom_errorbar(aes(x=Year, ymin=low_SOS, ymax=high_SOS), color='black', 
                    width=.1, size=0.2, position = position_nudge(x = -0.2)) +#position=position_dodge(0.05)
      coord_cartesian(ylim = c(95, 180)) + 
      scale_y_continuous(name='SOS (DOY)', sec.axis = sec_axis(~.+diffaxis, name='EOS (DOY)')) +
      geom_bar(aes(x=Year, y=mean_EOS-diffaxis, fill='EOS'), color="black", size=0.1,
               stat="identity", width = 0.4, position = position_nudge(x = 0.19)) + #color='black', 
      geom_errorbar(aes(x=Year, ymin=low_EOS-diffaxis, ymax=high_EOS-diffaxis), color='black', 
                    width=.1,size=0.2, position = position_nudge(x = 0.2)) + #position=position_dodge(0.05)
      scale_x_discrete(name='Year') +
      scale_fill_manual(values = colors) + 
      theme_bw() +  
      theme(panel.grid = element_blank(), 
            axis.text.x = element_text(colour="black", size=8), 
            axis.title.x = element_text(colour="black", size=8),
            axis.title.y = element_text(colour="deepskyblue4", size=8),
            axis.title.y.right = element_text(colour="gold4", size=8),
            axis.text.y = element_text(colour="deepskyblue4", size=8),
            axis.text.y.right = element_text(colour="gold4", size=8),
            legend.position = 'top',
            legend.key = element_rect(fill=NULL), 
            legend.box.spacing = unit(0, 'cm'),
            legend.box.margin=ggplot2::margin(0, 0, 0, 0), #margin(top, right, bottom, left) think trouble
            legend.key.width = unit(0.5, 'cm'), legend.key.height = unit(0.1, 'cm'),
            legend.text = element_text(size=8), 
            legend.title = element_text(size=8)) +
      guides(color = guide_legend(title = NULL), 
             fill=guide_legend(title = NULL), 
             shape=guide_legend(title = NULL)) 
    print(p)
    assign(paste('p.trajectory.2axes.', phen, sep = '.'), p)
    
    # png(filename = paste0(pathout, 'trajectory_2axes_', phen, "_bar.png"), width = 4, height=2, units="in", res=300)
    png(filename = paste0(pathout, 'trajectory_2axes_', phen, "_bar2.png"), width = 4.2, height=2, units="in", res=300)
    print(p)
    dev.off()
  }
  
  {
    phen <- 'greenness'
    diffaxis <- 0.4
    colors <- c('GMax'='deepskyblue4', 'GMin'='gold4', 'GRatio' = 'grey70')
    colors <- c('GMax'='#80cdc1', 'GMin'='#dfc27d', 'GRatio'='grey')
    p <- ggplot(summ)+
      geom_bar(aes(x=Year, y=mean_GMax, fill='GMax'), color="black", size=0.1,
               stat="identity", width = 0.28, position = position_nudge(x = -0.28)) + 
      geom_errorbar(aes(x=Year, ymin=low_GMax, ymax=high_GMax), color='black', 
                    width=.1, size=0.2, position = position_nudge(x = -0.28)) +
      # coord_cartesian(ylim = c(95, 180)) +
      geom_bar(aes(x=Year, y=mean_GMin, fill='GMin'), color="black", size=0.1,
               stat="identity", width = 0.28, position = position_nudge(x = 0)) + #color='black', , position=position_dodge()
      geom_errorbar(aes(x=Year, ymin=low_GMin, ymax=high_GMin), color='black', 
                    width=.1, size=0.2, position = position_nudge(x = 0)) +#position=position_dodge(0.05)
      geom_bar(aes(x=Year, y=mean_GRatio-diffaxis, fill='GRatio'), color="black", size=0.1,
               stat="identity", width = 0.28, position = position_nudge(x = 0.28)) + 
      geom_errorbar(aes(x=Year, ymin=low_GRatio-diffaxis, ymax=high_GRatio-diffaxis), color='black', 
                    width=.1, size=0.2, position = position_nudge(x = 0.28)) +
      scale_x_discrete(name='Year') +
      scale_y_continuous(name='EVI2', sec.axis = sec_axis(~.+diffaxis, name='Ratio')) +
      scale_fill_manual(values = colors) +
      theme_bw() +  
      theme(panel.grid = element_blank(), 
            axis.text.x = element_text(colour="black", size=8), 
            axis.title.x = element_text(colour="black", size=8),
            axis.title.y = element_text(colour="black", size=8),
            axis.title.y.right = element_text(colour="black", size=8),
            axis.text.y = element_text(colour="black", size=8),
            axis.text.y.right = element_text(colour="black", size=8),
            legend.position = 'top',
            legend.key = element_rect(fill=NULL), 
            legend.box.spacing = unit(0, 'cm'),
            legend.box.margin=ggplot2::margin(0, 0, 0, 0), #margin(top, right, bottom, left) think trouble
            legend.key.width = unit(0.5, 'cm'), legend.key.height = unit(0.1, 'cm'),
            legend.text = element_text(size=8), 
            legend.title = element_text(size=8)) +
      guides(color = guide_legend(title = NULL), 
             fill=guide_legend(title = NULL), 
             shape=guide_legend(title = NULL)) 
    # print(p)
    assign(paste('p.trajectory.2axes.', phen, sep = '.'), p)
    
    # png(filename = paste0(pathout, 'trajectory_2axes_', phen, "_bar.png"), width = 5, height=2, units="in", res=300)
    png(filename = paste0(pathout, 'trajectory_2axes_', phen, "_bar2.png"), width = 5.3, height=2, units="in", res=300)
    print(p)
    dev.off()
  }
  {
    phen <- 'greenness'
    diffaxis <- 0.4
    colors <- c('GMax'='deepskyblue4', 'GRatio'='gold4')
    colors <- c('GMax'='#80cdc1', 'GRatio'='#dfc27d')
    p <- ggplot(summ)+
      geom_bar(aes(x=Year, y=mean_GMax, fill='GMax'), color="black", size=0.1,
               stat="identity", width = 0.4, position = position_nudge(x = -0.2)) + 
      geom_errorbar(aes(x=Year, ymin=low_GMax, ymax=high_GMax), color='black', 
                    width=.1, size=0.2, position = position_nudge(x = -0.2)) +
      # coord_cartesian(ylim = c(95, 180)) +
      geom_bar(aes(x=Year, y=mean_GRatio-diffaxis, fill='GRatio'), color="black", size=0.1,
               stat="identity", width = 0.4, position = position_nudge(x = 0.19)) + 
      geom_errorbar(aes(x=Year, ymin=low_GRatio-diffaxis, ymax=high_GRatio-diffaxis), color='black', 
                    width=.1, size=0.2, position = position_nudge(x = 0.2)) +
      scale_x_discrete(name='Year') +
      scale_y_continuous(name='EVI2', sec.axis = sec_axis(~.+diffaxis, name='Ratio')) +
      scale_fill_manual(values = colors) +
      theme_bw() +  
      theme(panel.grid = element_blank(), 
            axis.text.x = element_text(colour="black", size=8), 
            axis.title.x = element_text(colour="black", size=8),
            axis.title.y = element_text(colour="black", size=8),
            axis.title.y.right = element_text(colour="black", size=8),
            axis.text.y = element_text(colour="black", size=8),
            axis.text.y.right = element_text(colour="black", size=8),
            legend.position = 'top',
            legend.key = element_rect(fill=NULL), 
            legend.box.spacing = unit(0, 'cm'),
            legend.box.margin=ggplot2::margin(0, 0, 0, 0), #margin(top, right, bottom, left) think trouble
            legend.key.width = unit(0.5, 'cm'), legend.key.height = unit(0.1, 'cm'),
            legend.text = element_text(size=8), 
            legend.title = element_text(size=8)) +
      guides(color = guide_legend(title = NULL), 
             fill=guide_legend(title = NULL), 
             shape=guide_legend(title = NULL)) 
    # print(p)
    assign(paste('p.trajectory.2axes.', phen, sep = '.'), p)
    
    # png(filename = paste0(pathout, 'trajectory_2axes_', phen, "_bar.png"), width = 5, height=2, units="in", res=300)
    png(filename = paste0(pathout, 'trajectory_2axes_', phen, "2_bar2.png"), width = 4.2, height=2, units="in", res=300)
    print(p)
    dev.off()
  }
}

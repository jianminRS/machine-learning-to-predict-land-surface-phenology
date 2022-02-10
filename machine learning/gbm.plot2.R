gbm.plot2 <- function(gbm.object,                # a gbm object - could be one from gbm.step
           variables = 0,               # the var to plot - if zero then plots all; either strings or numbers
           smooth = FALSE,                # should we add a smoothed version of the fitted function 
           rug = TRUE,                    # plot a rug of deciles
           n.plots = length(pred.names),  # plot the first n most important preds
           y.zero.center = TRUE,             # F:'origninal' or T: 'zero-center'
           common.scale = TRUE,           # use a common scale on the y axis 
           common.y.axis = FALSE, 
           y.label = "fitted function",   # the default y-axis label
           x.label = NULL,                # the default x-axis label
           title = NA,
           show.contrib = TRUE,           # show the contribution on the x axis
           plot.layout = c(3,4),          # define the default layout for graphs on the page
           ...
           # useful options include layout_matrix, widths, heights, etc
           # widths or heights = 3:1 c(1, 3, 2)
           # layout_matrix = cbind(c(1,1,1), c(2,3,4))). If you print out the matrix, the place of number n are taken by figure n
) {
    
    if (! requireNamespace('gbm') ) { stop ('you need to install the gbm package to run this function') }
    requireNamespace('splines')
    if (! requireNamespace('ggplot2') ) { stop ('you need to install the ggplot2 package to run this function') }
    if (! requireNamespace('gridExtra') ) { stop ('you need to install the gridExtra package to run this function') }
    if(common.y.axis) common.scale <- TRUE
    
    gbm.call <- gbm.object$gbm.call
    gbm.x <- gbm.call$gbm.x
    pred.names <- gbm.call$predictor.names ##The same to pred.names
    response.name <- gbm.call$response.name
    
    data <- gbm.call$dataframe
    
    max.plots <- plot.layout[1] * plot.layout[2]
    
    if(is.character(variables[1])) {variables <- match(variables, pred.names)}
    if (any(variables <= 0)|is.null(variables)) {   #we are plotting all vars in rank order of contribution
      if(length(variables)>1) warning('the used variables include <=0') 
      if (n.plots > max.vars) {
        n.plots <- max.vars
        warning("reducing no of plotted predictors to maximum available (",max.vars,")")
      }
      variables <- match(gbm.object$contributions$var[1:n.plots],pred.names)
    }
    n.plots <- length(variables)
    if(n.plots>max.plots) {stop('Please either reduce the number of variabls or increase the max.plots')}
    if(length(x.label)>0 & length(x.label)<n.plots) {x.label <- paste(x.label[1], variables, sep='_')}
    
      
    predictors <- list(rep(NA,n.plots)) # matrix(0,ncol=n.plots,nrow=100)
    responses <- list(rep(NA,n.plots)) # matrix(0,ncol=n.plots,nrow=100)
    var.names <- rep(NA, n.plots)
    for(j in seq(1, n.plots)){
      k <- variables[j]
      if (is.null(x.label)) {
        var.names[j] <- gbm.call$predictor.names[k]
      } else {
        var.names[j] <- x.label[j]
      }
      pred.data <- data[ , gbm.call$gbm.x[k]]
      
      response.matrix <- gbm::plot.gbm(gbm.object, k, return.grid = TRUE)
      
  
      predictors[[j]] <- response.matrix[,1]
      if (is.factor(data[,gbm.call$gbm.x[k]])) {
        predictors[[j]] <- factor(predictors[[j]],levels = levels(data[,gbm.call$gbm.x[k]]))
      }
      if(y.zero.center) {
        responses[[j]] <- response.matrix[,2] - mean(response.matrix[,2])
      } else {
        responses[[j]] <- response.matrix[,2]
      }
      
      
      if(j == 1) {
        ymin = min(responses[[j]])
        ymax = max(responses[[j]])
      } else {
        ymin = min(ymin,min(responses[[j]]))
        ymax = max(ymax,max(responses[[j]]))
      }
    }
    
    
    # now do the actual plots
    
    
    if(common.scale) {
      ylim <- c(ymin, ymax)
    } else {
        ylim <- NULL
    } 
    grobs <- list()
    widths <- list()
    ncol=plot.layout[2]
    nrow=plot.layout[1] 
    for (j in c(1:n.plots)) {
      k <- match(pred.names[variables[j]],gbm.object$contributions$var)
      if (show.contrib) {
        xname <- paste(var.names[j]," (",round(gbm.object$contributions[k,2],1),"%)",sep="")
      }else xname <- var.names[j]
      
      datap <- data.frame(predictors[[j]], responses[[j]])
      names(datap) <- c('x', 'y')
      p <- ggplot(datap) + 
        geom_line(aes(x=x, y=y), color='black') + 
        scale_y_continuous(limits = ylim, expand = c(0.05, 0)) +
        xlab(xname) + ylab(y.label) +
        # geom_abline(intercept=0, slope=1, linetype=2, lwd=1)
        theme_bw() +
        theme(panel.grid = element_blank(), 
              # axis.text.x = element_text(colour="black"),
              # axis.text.y = element_text(colour="black"),
              axis.text = element_text(colour="black", size=8),
              axis.title = element_text(colour="black", size = 8),
              plot.margin = unit(c(0.2,0.3,0.2,0.2), "lines"),
              legend.position = 'none')
      
      ##Because SRad is too large with a mean 10004 and it is difficult to make pretty figures
      ## mannually set the x breaks
      # if(length(grep('SRad30', pred.names[variables[j]]))>0){
      #   p <- p + scale_x_continuous(breaks = c(15500, 16000))
      # }

      if(smooth & is.vector(predictors[[j]])) {
        p <- p + geom_smooth(aes(x=x, y=y), method = "loess", se=F, color='red')
      }
      if (rug & is.vector(data[,gbm.x[variables[j]]])) {
        rugs <- quantile(data[,gbm.x[variables[j]]], probs = seq(0, 1, 0.1), na.rm = TRUE)
        rugs <- data.frame(x=rugs)
        p <- p + 
          geom_rug(data = rugs, 
                   mapping=aes(x=x), #, y=response.name
                   inherit.aes = F,
                   stat = 'identity',
                   color='black',
                   sides='b',
                   length = unit(0.04, "npc"))
      }
      
      if(common.y.axis & (j-1)%%ncol > 0){
        p <- p + 
          theme(axis.text.y = element_blank(),
                axis.ticks.y = element_blank(), 
                axis.title.y= element_blank())
      }
      # print(p)
      grobs[[j]] <- ggplotGrob(p)
      # if(common.y.axis){
      #   if(j>1) grobs[[j]]$widths <- grobs[[1]]$widths
      #   # widths[[j]] <- grobs[[j]]$widths[2:3]
      #   # if(j==1) {
      #   #   newWidth <- widths[[j]]
      #   # } else {
      #   #   newWidth <- unit.pmax(widths[[j]], newWidth)
      #   # }
      # }
      
    }  
    # if(common.y.axis){
    #   for(j in seq(1, n.plots)){
    #     grobs[[j]]$widths[2:3] <- newWidth
    #   }
    # }

    # arrangeGrob(grobs=grobs, nrow=plot.layout[1], ncol=plot.layout[2], top=title)
    grid.arrange(grobs=grobs, nrow=nrow, ncol=ncol, ...)
  }

























gbm.plot <-
  function(gbm.object,                # a gbm object - could be one from gbm.step
           variable.no = 0,               # the var to plot - if zero then plots all
           smooth = FALSE,                # should we add a smoothed version of the fitted function 
           rug = TRUE,                    # plot a rug of deciles
           n.plots = length(pred.names),  # plot the first n most important preds
           common.scale = TRUE,           # use a common scale on the y axis
           write.title = TRUE,            # plot a title above the plot
           y.label = "fitted function",   # the default y-axis label
           x.label = NULL,                # the default x-axis label
           show.contrib = TRUE,           # show the contribution on the x axis
           plot.layout = c(3,4),          # define the default layout for graphs on the page
           ...                            # other arguments to pass to the plotting 
           # useful options include cex.axis, cex.lab, etc.
  ) {
    
    if (! requireNamespace('gbm') ) { stop ('you need to install the gbm package to run this function') }
    requireNamespace('splines')
    
    gbm.call <- gbm.object$gbm.call
    gbm.x <- gbm.call$gbm.x
    pred.names <- gbm.call$predictor.names
    response.name <- gbm.call$response.name
    
    data <- gbm.call$dataframe
    
    max.plots <- plot.layout[1] * plot.layout[2]
    plot.count <- 0
    n.pages <- 1
    
    if (length(variable.no) > 1) { stop("only one response variable can be plotted at a time") }
    
    if (variable.no > 0) {   #we are plotting all vars in rank order of contribution
      n.plots <- 1
    }
    
    max.vars <- length(gbm.object$contributions$var)
    if (n.plots > max.vars) {
      n.plots <- max.vars
      warning("reducing no of plotted predictors to maximum available (",max.vars,")")
    }
    
    predictors <- list(rep(NA,n.plots)) # matrix(0,ncol=n.plots,nrow=100)
    responses <- list(rep(NA,n.plots)) # matrix(0,ncol=n.plots,nrow=100)
    
    for (j in c(1:n.plots)) {  #cycle through the first time and get the range of the functions
      if (n.plots == 1) {
        k <- variable.no
      } else {
        k <- match(gbm.object$contributions$var[j],pred.names)
      }
      if (is.null(x.label)) {
        var.name <- gbm.call$predictor.names[k]
      } else {
        var.name <- x.label
      }
      pred.data <- data[ , gbm.call$gbm.x[k]]
      
      response.matrix <- gbm::plot.gbm(gbm.object, k, return.grid = TRUE)
      
      predictors[[j]] <- response.matrix[,1]
      if (is.factor(data[,gbm.call$gbm.x[k]])) {
        predictors[[j]] <- factor(predictors[[j]],levels = levels(data[,gbm.call$gbm.x[k]]))
      }
      responses[[j]] <- response.matrix[,2] - mean(response.matrix[,2])
      
      if(j == 1) {
        ymin = min(responses[[j]])
        ymax = max(responses[[j]])
      } else {
        ymin = min(ymin,min(responses[[j]]))
        ymax = max(ymax,max(responses[[j]]))
      }
    }
    
    # now do the actual plots
    
    op <- graphics::par(no.readonly = TRUE) 
    graphics::par(mfrow = plot.layout)
    
    for (j in c(1:n.plots)) {
      
      if (plot.count == max.plots) {
        plot.count = 0
        n.pages <- n.pages + 1
      }
      
      plot.count <- plot.count + 1
      
      if (n.plots == 1) {
        k <- match(pred.names[variable.no],gbm.object$contributions$var)
        if (show.contrib) {
          x.label <- paste(var.name,"  (",round(gbm.object$contributions[k,2],1),"%)",sep="")
        }
      } else {
        k <- match(gbm.object$contributions$var[j],pred.names)
        var.name <- gbm.call$predictor.names[k]
        if (show.contrib) {
          x.label <- paste(var.name,"  (",round(gbm.object$contributions[j,2],1),"%)",sep="")
        } else x.label <- var.name
      }
      
      if (common.scale) {
        plot(predictors[[j]],responses[[j]],ylim=c(ymin,ymax), type='l',
             xlab = x.label, ylab = y.label, ...)
      } else {
        plot(predictors[[j]],responses[[j]], type='l', 
             xlab = x.label, ylab = y.label, ...)
      }
      if (smooth & is.vector(predictors[[j]])) {
        temp.lo <- loess(responses[[j]] ~ predictors[[j]], span = 0.3)
        lines(predictors[[j]],fitted(temp.lo), lty = 2, col = 2)
      }
      if (plot.count == 1) {
        if (write.title) {
          title(paste(response.name," - page ",n.pages,sep=""))
        }
        if (rug & is.vector(data[,gbm.call$gbm.x[variable.no]])) {
          rug(quantile(data[,gbm.call$gbm.x[variable.no]], probs = seq(0, 1, 0.1), na.rm = TRUE))
        }
      } else {
        if (write.title & j == 1) {
          title(response.name)
        }
        if (rug & is.vector(data[,gbm.call$gbm.x[k]])) {
          rug(quantile(data[,gbm.call$gbm.x[k]], probs = seq(0, 1, 0.1), na.rm = TRUE))
        }
      }
    }
    graphics::par(op)
    
  }

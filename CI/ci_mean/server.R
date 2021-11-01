library("shiny")
source("helper2.R")

mmstat$vartype <<- 'numvars'

mmstat.ui.elem('conflevel',  'confidenceLevel') 
mmstat.ui.elem('sigma',      'checkboxInput', 
               label=gettext("Population variance known"), value=TRUE)
mmstat.ui.elem("size",       "sampleSize")
mmstat.ui.elem('go',         'drawSample')
mmstat.ui.elem('reset',      'actionButton', label=gettext("Reset"), value=0)
mmstat.ui.elem('speed',      'speedSlider')
mmstat.ui.elem("dataset",    "dataSet",     
               choices=mmstat.getDataNames("BOSTONHOUSING", "USCRIME", "CARS"))
mmstat.ui.elem("variable",   "variable1",   
               vartype="numeric")
mmstat.ui.elem("cex",        "fontSize")

confint <- vector("list", 0)

CImean  <-function (x, oma, xlim=NULL, sigma=NA) {
    n <- length(x)
    if (is.na(sigma)) {
        dist <- qt(1-(1-oma)/2, n-1)*sd(x)/sqrt(n)
    } else {
        dist <- qnorm(1-(1-oma)/2)*sigma/sqrt(n)    
    }
    mean  <- mean(x)
    upper <- mean+dist
    lower <- mean-dist
    if (is.null(xlim)) {
        lim <- c(lower, upper)
    } else {
        lim   <- mmstat.merge(xlim, c(lower, upper))
    }
    list(upper=upper, lower=lower, mean=mean, oma=oma, n=n, xlim=lim)
}

drawIqrBoxWithPoints <- function (x, jitter, ylim, box.param=NULL, points.param=NULL) {
    if (is.list(points.param) || is.null(points.param) || points.param) {
        points.param$x <- x
        points.param$y <- ylim[1]+diff(ylim)*jitter  
        suppressWarnings(do.call('points', points.param))
    } 
    if (is.list(box.param) || is.null(box.param) || box.param) {
        q <- quantile(x, c(0.25, 0.5, 0.75))
        args         <- box.param
        args$xleft   <- q[1]
        args$xright  <- q[3]  
        args$ybottom <- ylim[1]  
        args$ytop    <- ylim[2]  
        suppressWarnings(do.call('rect', args))
        args <- box.param
        if(!is.null(args$border)) {
            args$col <- args$border
            args['border'] <- NULL
        }
        args$x <- c(q[2], q[2])
        args$y <- ylim
        suppressWarnings(do.call('lines', args))
    }
}

shinyServer(function(input, output, session) {  
    
    output$conflevelUI <- renderUI({ mmstat.ui.call('conflevel') })
    output$sigmaUI     <- renderUI({ mmstat.ui.call('sigma') })
    output$goUI        <- renderUI({ mmstat.ui.call('go') })
    output$resetUI     <- renderUI({ mmstat.ui.call('reset') })
    output$speedUI     <- renderUI({ mmstat.ui.call('speed') })
    output$datasetUI   <- renderUI({ mmstat.ui.call('dataset') })
    output$cexUI       <- renderUI({ mmstat.ui.call('cex')  })
    
    output$sizeUI <- renderUI({
        var <- getVar()
        mmstat.ui.call('size', ticks=var$ticks, max=length(var$ticks))
    })
    
    
    
    
    output$variableUI <- renderUI({
        inp  <- mmstat.getValues(NULL, dataset=input$dataset)
        mmstat.ui.call('variable',
                       choices = mmstat.getVarNames(inp$dataset, 'numeric'))
    })  
    
    
    
    getVar <- reactive({
        inp         <- mmstat.getValues(NULL, dataset=isolate(input$dataset), variable=input$variable)
        var         <- mmstat.getVar(inp$dataset, inp$variable)
        var$ticks   <- mmstat.ticks(var$n, nmin=30)   
        dec         <- mmstat.dec(0.1*c(0, var$sd/sqrt(max(var$ticks))))
        var$decimal <- dec$decimal
        var
    })
    
    
    
    #trigger for multible events
    triggerChanges <- reactive({
        paste(input$go , input$speed)
    })
    
    #for the Level
    getLevel <- eventReactive(triggerChanges(), {
        input$conflevel
    })
    
    getSigma <- eventReactive(triggerChanges(), {
        input$sigma
    })
    
    #for the size
    getSize <- eventReactive(triggerChanges(), {
        var <- getVar()
        inp <- mmstat.getValues(NULL, size=input$size)
        var$ticks[inp$size]
    })
    
    resetCI <- reactive ({
        input$reset
        confint <<- vector("list", 0)
    })
    
    drawSample <- reactive ({
        mmstat.log(sprintf('drawSample'))
        inp <- mmstat.getValues (NULL,
                                 go         = input$go,
                                 reset      = input$reset,
                                 sigma      = getSigma(),
                                 speed      = input$speed,
                                 conflevel  = getLevel())
        if (inp$speed>0) invalidateLater(500/inp$speed, session)   
        var      <- getVar()
        index    <- sample(length(var$values), size=getSize(), replace=T)
        sample   <- var$values[index]
        nci      <- length(confint)
        if(nci>0) xlim<-confint[[nci]]$xlim else xlim = NULL;
        clal     <- mmstat$UI$conflevel$ticks[inp$conflevel]/100
        confint[[nci+1]] <<- CImean(sample, clal, xlim, ifelse(inp$sigma, var$sd, NA))
        nci              <- length(confint)
        index
    })
    
    output$outputConfPlot <- renderPlot({
        mmstat.log(sprintf('outputConfPlot'))
        resetCI()
        var   <- getVar()
        index <- drawSample()
        inp   <- mmstat.getValues(NULL, cex = input$cex)
        nci   <- length(confint)
        if (nci) {
            par (mar=c(2,0,2,0))
            plot (0, 0, type="n", xlim=confint[[nci]]$xlim, ylim=c(1.5, 2.0+0.2*nci), axes=F, cex=0.5, col=mmstat$col[[1]], 
                  main=sprintf(gettext("%i confidence interval(s)"), nci),
                  cex.axis=inp$cex, cex.lab=inp$cex, cex.main=1.2*inp$cex, cex.sub=inp$cex)
            text(var$mean, 1.5, sprintf("%.*f", var$decimal, var$mean), col=mmstat$col[[1]], pos=4, cex=inp$cex)
            usr <- par("usr")
            mmstat.axis(1, usr[1:2], cex.axis=inp$cex) 
            #   points(sample, 1.5+0.5*jitter[index], pch=19, col=mmstat$col[[1]])
            seqi <- 1:nci
            lwd  <- 2-(nci>10)
            for (i in seqi) {
                yi  <- 1.95+0.2*i
                rng <- c(confint[[i]]$lower, confint[[i]]$upper)
                col <- ifelse((rng[1]-var$mean)>0 || (rng[2]-var$mean)<0, 'black', mmstat$col[[2]])
                lines(rng, c(yi, yi), lwd=lwd, lty='solid', col=col)
                lines(c(rng[1], rng[1]), c(yi-0.05, yi+0.05), lwd=lwd, lty='solid', col=col)
                lines(c(rng[2], rng[2]), c(yi-0.05, yi+0.05), lwd=lwd, lty='solid', col=col)
                lines(c(confint[[i]]$mean, confint[[i]]$mean), c(yi-0.05, yi+0.05), lwd=lwd, lty='solid', col=col)
            }
            size  <- sapply(confint, '[[', 'n')
            index <- 1+c(0, which(diff(size)!=0))
            posx  <- mmstat.pos(usr[1:2], c(0.05, 0.95)) 
            text(posx[1], 1.95+0.2*index, labels=sprintf('%.0f', size[index]), cex=inp$cex)
            oma  <- sapply(confint, '[[', 'oma')
            index <- 1+c(0, which(diff(oma)!=0))
            text(posx[2], 1.95+0.2*index, labels=sprintf('%.3f', oma[index]), cex=inp$cex)
            axis(3, at=posx, labels=c(expression('n'), expression(alpha)), cex.axis=inp$cex)
            abline(v=var$mean, col=mmstat$col[[1]], lwd=3, lty="dotted")
            box()
        }
    })    
    
    output$outputSamplePlot <- renderPlot({
        mmstat.log(sprintf('outputSamplePlot'))
        var   <- getVar()
        index <- drawSample()
        inp   <- mmstat.getValues(NULL, cex=input$cex, sigma=input$sigma)
        par (mar=c(5,0,2,0))
        plot(range(var$values), c(-0.05,1.0), type="n", axes=F, main=gettext("Population and sample"), xlab=var$xlab, sub=var$sub,
             cex.axis=inp$cex, cex.lab=inp$cex, cex.main=1.2*inp$cex, cex.sub=inp$cex)
        usr <- par("usr")
        mmstat.axis(1, usr[1:2], cex.axis=inp$cex) 
        drawIqrBoxWithPoints(var$values, var$jitter, ylim=c(0, 0.45),
                             box.param=list(border=mmstat$col[[1]], lwd=2),
                             points.param=list(col=mmstat$col[[9]], pch=19, cex=0.5*inp$cex))
        if (inp$sigma) {
            drawIqrBoxWithPoints(var$values[index], var$jitter[index], ylim=0.5+c(0, 0.45*sqrt(length(index)/var$n)),
                                 box.param=FALSE,
                                 points.param=list(col=mmstat$col[[10]], pch=19, cex=0.5*inp$cex))
        } else {
            drawIqrBoxWithPoints(var$values[index], var$jitter[index], ylim=0.5+c(0, 0.45*sqrt(length(index)/var$n)),
                                 box.param=list(border=mmstat$col[[2]], lwd=2),
                                 points.param=list(col=mmstat$col[[10]], pch=19, cex=0.5*inp$cex))      
        }
        box()
    })
    
    output$logText <- renderText({
        mmstat.getLog(session)
    })
})

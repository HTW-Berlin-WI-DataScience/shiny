library("shiny")

# Default parameters
mmstat.getValues <- function (local, ...) {
  ret <<- list(...)
  for (name in names(ret)) {
    if (is.null(ret[[name]]) || (length(ret[[name]])==0)) {
      if (is.null(local[[name]])) {
        stopif (is.null(mmstat$UI[[name]]$value) && is.null(mmstat$UI[[name]]$selected), 
                paste0('mmstat.getValues: no default value(s) for "', name, '"'))
        if (is.null(mmstat$UI[[name]]$value)) {
          ret[[name]] <- mmstat$UI[[name]]$selected
        } else {
          ret[[name]] <- mmstat$UI[[name]]$value
        }
      } else {
        ret[[name]] <- local[[name]]
      }
    }
    if (!is.null(mmstat$UI[[name]]$call) && (mmstat$UI[[name]]$call=='mmstat.sliderInput')) {
      if ((compareVersion(mmstat$shiny, '0.11')>=0) && !is.null(mmstat$UI[[name]]$ticks)) {
        ret[[name]] <- ret[[name]]+1
      }
    }
  }
  ret
}

# Datasets and variables
mmstat.getDataNames <- function (...) {
  is.binary <- function(v) {
    x <- unique(v)
    length(x) - sum(is.na(x)) == 2L
  }
  
  files <- list(...)
  if (length(files)==0) return(names(mmstat$dataset)[1])
  
  dataset        <- vector("list", length(files))
  names(dataset) <- files
  mmstat$dataset <<- dataset
  for (i in seq(files)) {
    file    <- files[[i]]    
    if (is.null(mmstat$dataset$file)) {
      data    <- readRDS(paste0(file, ".rds"))
      if (class(data)=="data.frame") {
        allvars <- names(data)
        mmstat$dataset[[file]] <<-list(data=data, allvars=allvars, 
                                       numvars=allvars[sapply(data, is.numeric)], 
                                       ordvars=allvars[sapply(data, is.ordered)], 
                                       facvars=allvars[sapply(data, is.factor)], 
                                       binvars=allvars[sapply(data, is.binary)])
      } 
      if (class(data)=="ts") {
        allvars <- colnames(data)
        mmstat$dataset[[file]] <<-list(data=data, allvars=allvars, numvars=allvars)
      }
    }
  }
  gettext(names(mmstat$dataset), "name")
}

mmstat.getVarNames <- function(dataname, vartype, which=NULL) {
  vars <- NULL
  if (vartype=='numeric') vars <- 'numvars'
  if (vartype=='ordered') vars <- 'ordvars'
  if (vartype=='factor')  vars <- 'facvars'
  if (vartype=='binary')  vars <- 'binvars'
  stopif(is.null(vars), 'mmstat.getVarNames: Variable type "%s" unknown')  
  if (is.null(which)) return(gettext(mmstat$dataset[[dataname]][[vars]], "name"))
  if (length(which==1)) return(gettext(mmstat$dataset[[dataname]][[vars]][which]))
  return(gettext(mmstat$dataset[[dataname]][[vars]][which], "name"))
}

mmstat.getDatasets <- function (...) {
  is.binary <- function(v) {
    x <- unique(v)
    length(x) - sum(is.na(x)) == 2L
  }
  
  files          <- list(...)
  dataset        <- vector("list", length(files))
  names(dataset) <- files
  mmstat$dataset <<- dataset
  for (i in seq(files)) {
    file    <- files[[i]]    
    data    <- readRDS(paste0(file, ".rds"))
    if (class(data)=="data.frame") {
      allvars <- names(data)
      mmstat$dataset[[file]] <<-list(data=data, allvars=allvars, 
                                     numvars=allvars[sapply(data, is.numeric)], 
                                     ordvars=allvars[sapply(data, is.ordered)], 
                                     facvars=allvars[sapply(data, is.factor)], 
                                     binvars=allvars[sapply(data, is.binary)])
    } 
    if (class(data)=="ts") {
      allvars <- colnames(data)
      mmstat$dataset[[file]] <<-list(data=data, allvars=allvars, numvars=allvars)
    }
  }
  names(dataset)[1]
}

mmstat.attrVar <- function(var, type, index=NULL) {
  ret <- var
  if (is.null(index)) index=1:length(var$values) else {
    ret$values <- var$values[index]
    ret$n      <- length(index)
  }
  if (type=='numvars') {
    if (is.null(var$jitter)) ret$jitter <- runif(length(index)) else  ret$jitter <- var$jitter[index] 
    ret$mean   <- mean(var$values[index], na.rm=T)
    ret$median <- median(var$values[index], na.rm=T)
    ret$sd     <- sd(var$values[index], na.rm=T)
    ret$var    <- var(var$values[index], na.rm=T)
    ret$range  <- range(var$values[index], na.rm=T)
    ret$iqr    <- IQR(var$values[index], na.rm=T)
    ret$quart  <- quantile(var$values[index], c(0.25, 0.75), na.rm=T)
    ret$min    <- min(var$values[index], na.rm=T)
    ret$max    <- max(var$values[index], na.rm=T)
  }
  if ((type=='binvars') || (type=='ordvars') || (type=='facvars')) {
    ret$tab    <- table(var$values[index], useNA="ifany")
    ret$prop   <- prop.table(ret$tab)
  }
  ret
}

mmstat.getDatasetNames <- function() {
  gettext(names(mmstat$dataset), "name")
}

mmstat.getVariableNames <- function(name) {
  gettext(mmstat$dataset[[name]]$allvars, "name")
}

mmstat.getVar <- function (dataname=NULL, varname=NULL, vartype=NULL, na.action=na.omit) {
  match.name <- function(needle, haystack) {
    if (is.null(needle)) return(NA);
    return(match(needle, haystack))
  }
  
  # which variable type do we need
  if (is.null(vartype)) vartype <- mmstat$vartype
  deftype <- c('numvars', 'binvars', 'ordvars', 'facvars', 'numeric', 'binary', 'ordered', 'factor')
  type    <- pmatch(vartype, deftype)
  stopif (is.na(type), paste0('mmstat.getVar: Unknown variable type: ', vartype))
  type    <- deftype[type]
  if (type=='numeric') type <- 'numvars'
  if (type=='ordered') type <- 'ordvars'
  if (type=='factor')  type <- 'facvars'
  if (type=='binary')  type <- 'binvars'
  # get dataset
  pos <- match.name(dataname, names(mmstat$dataset))
  if (is.na(pos)) pos <- 1  # assume that all datasets contain 
  dataname <- names(mmstat$dataset)[pos]
  if (is.null(mmstat$dataset[[dataname]])) stop(sprintf(gettext("Data file '%s' is missing"), dataname))
  # get variable
  pos <- match.name(varname, mmstat$dataset[[dataname]][[type]])
  if (is.na(pos)) pos <- 1  # assume that every datasets contains a variable of correct type 
  varname <- mmstat$dataset[[dataname]][[type]][pos]
  # get var values
  dataset <- mmstat$dataset[[dataname]]$data
  if (class(dataset)=="data.frame") values  <- na.action(dataset[[varname]])
  if (class(dataset)=="ts") values  <- na.action(dataset[,varname])
  var     <- list(values   = values, 
                  n        = length(values),
                  name     = varname,
                  sub      = paste(gettext("Dataset:"), gettext(dataname)),
                  xlab     = gettext(varname),
                  data     = dataname,
                  dataname = gettext(dataname)
                 )
  var <- mmstat.attrVar(var, type)
  var
}

mmstat.dec <- function(x, ord=NULL) {
  order <- order(x)
  df    <- diff(x[order])
  # take only positive differences
  df    <- min(df[df>0])
  if (!is.null(ord)) order <- order(order[ord])
  list(decimal = max(0, ceiling(-log10(df))),
       order   = order)
}

mmstat.axis <- function(side, range, at, labels, ...) {
  at  <- pretty(range)
  dec <- mmstat.dec(at)
  axis(side, at=at, labels=sprintf("%.*f", dec$decimal, at), ...)
}

mmstat.baraxis <- function(side, range, at, labels, ...) {
  pos <- 1+pretty(range)
  axis(side, at=at[pos], labels=labels[pos], ...)
}

mmstat.merge <- function (range1, range2) {
  return (c(min(range1, range2), max(range1, range2)))
}

mmstat.range <- function (...) {
  ranges <- list(...)
  isn    <- sapply(ranges, is.numeric)
  if (all(isn)) {
    mins   <- sapply(ranges, min)
    maxs   <- sapply(ranges, max)
  } else {
    mins <- maxs <- NA
  }
  return (c(min(mins), max(maxs)))
}

mmstat.round.up <- function (x, digits=0) {
  xr <- round(x, digits)
  tf <- (xr<x)
  xr[tf] <- xr[tf]+10^(-digits) 
  xr
}

mmstat.round.down <- function (x, digits=0) {
  xr <- round(x, digits)
  tf <- (xr>x)
  xr[tf] <- xr[tf]-10^(-digits)
  xr
}

mmstat.pos <- function (minmax, pos) {
  min(minmax)+diff(minmax)*pos
}

mmstat.ticks <- function (nin, nmin=3, tin=11) {
  nmax <- nin
  nt   <- tin-1
  repeat {
    n           <- nmin*exp((0:nt)/nt*log(nmax/nmin))
    pow         <- 10^trunc(log10(n))
    fsd         <- n%/%pow
    ssd         <- (n%%pow)%/%(pow/10)
    ssd[pow==1] <- 0
    ssd[ssd<3]  <- 0
    ssd[(ssd>2)&(ssd<8)] <- 5
    fsd[ssd>7]  <- fsd[ssd>7]+1
    ssd[ssd>7]  <- 0
    nret        <- fsd*pow+ssd*pow/10
    if(nret[nt+1]>nmax) nret[nt+1]<-nret[nt+1]-pow[nt+1]/2
    if (length(unique(nret))==nt+1) return(nret)
    nt <- nt-1
  }  
}

mmstat.math <- function (txt) {
  dollar <- strsplit(txt, '&', fixed=T)[[1]]
  if (length(dollar)<2) return(txt)
  res <- paste0('expression(paste("', dollar[1], '"')
  for (i in 2:length(dollar)) {
    percent <- strsplit(dollar[i], ';', fixed=T)[[1]]
    lp      <- length(percent)
    if (lp==1) res <- paste0(res, ',"', percent[1], '"')
    else {
      if (lp>2) percent[2] <- paste(percent[2:lp], sep=';')
      res <- paste0(res, ',', percent[1], ',"', percent[2], '"')
    }
  }
  res <- paste0(res, '))')
  eval(parse(text=res))
}

is.ASCII <- function (txt) { all(charToRaw(txt) <= as.raw(127)) }
stopif   <- function (cond, txt) { if (cond) stop(txt) }

mmstat.html <- function(file, ...) {
  stopif (!file.exists(file), sprintf("File '%s' does not exist", file))
  html     <- paste0(readLines(file), collapse="")
  stopif (!is.ASCII(html), sprintf("File '%s' contains non-ASCII symbols", file))
  args <- list(...)
  cond <- sapply(args, length)
  stopif(!all(cond), paste('mmstat.html - Zero length arguments:', paste(names(args)[!cond], collapse=', ')))
  if (length(args)) {
    stopif (any(sapply(args, is.null)), 'One or more arguments contain a NULL')
    args$fmt <- html
    html     <- do.call("sprintf", args)
  }
  return(html)
}

mmstat.plotTestRegions <- function (crit, xlim, ylim, cex, close=F, col="black", label=NULL, pos=1) {
  lines(xlim, c(ylim[1], ylim[1]), col=col)
  lines(xlim, c(ylim[2], ylim[2]), col=col)
  if (close) {
    lines(c(xlim[1],xlim[1]), ylim, col=col)
    lines(c(xlim[2],xlim[2]), ylim, col=col)
  }
  cu <- max(crit[1], xlim[1])
  if (crit[1]>=xlim[1]) {
    lines(c(cu,cu), ylim, col=col)
    text((cu+xlim[1])/2, mean(ylim), mmstat.math("\\\"&H[1];\\\""), cex=cex, col=col)
  } 
  co <- min(crit[2], xlim[2])
  if (crit[2]<=xlim[2]) {
    lines(c(co,co), ylim, col=col)
    text((co+xlim[2])/2, mean(ylim), mmstat.math("\\\"&H[1];\\\""), cex=cex, col=col)
  }
  text((co+cu)/2, mean(ylim), mmstat.math("\\\"&H[0];\\\""), cex=cex, col=col)
  if (!is.null(text)) {
    if (pos==2) text(xlim[1], mmstat.pos(ylim, -0.25), label, col=col, cex=cex, pos=4)
    if (pos==4) text(xlim[2], mmstat.pos(ylim, -0.25), label, col=col, cex=cex, pos=2)
  }
}

mmstat.htest <- function (...) {
  
  addnames <- function (txt1, txt2) {
    if (length(txt1)) {
      cont <- txt2 %in% txt1
      ret  <- c(txt1, txt2[!cont])
    } else {
      ret <- txt2
    }
    ret
  }
  
  htest <- list(method      = list(attr=NULL,         names='',                     fmt="%s",   lines=0),
                alternative = list(attr=NULL,         names='Alternative:',         fmt="%s",   lines=0),
                null.value  = list(attr='names',      names=vector("character", 0), fmt="%.4f", lines=1),
                data.name   = list(attr=NULL,         names='Data:',                fmt="%s",   lines=0),
                estimate    = list(attr='names',      names=vector("character", 0), fmt="%.4f", lines=0),
                conf.int    = list(attr='conf.level', names=vector("character", 0), fmt="%s",   lines=1),
                statistic   = list(attr='names',      names=vector("character", 0), fmt="%.4f", lines=0),
                parameter   = list(attr='names',      names=vector("character", 0), fmt="%.0f", lines=0),
                p.value     = list(attr=NULL,         names='p-value:',             fmt="%.4f", lines=0)
  )
  
  tests  <- list(...)
  nhtest <- names(htest)
  nrow   <- vector("numeric", length(htest))
  lines  <- 0
  for (j in seq(nhtest)) {
    name <- nhtest[j]
    attr <- htest[[nhtest[j]]]$attr
    if (!is.null(attr)) {
      # find all names
      for (i in seq(tests)) {
        htest[[name]]$names <- addnames(htest[[name]]$names, attr(tests[[i]][[name]], attr))
      }
    }
    # grab all values
    nrow[j] <- length(htest[[name]]$names)
    htest[[name]]$tab <- matrix('', nrow=nrow[j], ncol=length(tests))
    for (i in seq(tests)) {
      telem <- tests[[i]][[name]]
      if (!is.null(telem)) {
        if (is.null(attr)) {
          htest[[name]]$tab[1, i] <- sprintf(htest[[name]]$fmt, telem)
        } else if (attr=='conf.level') {
          htest[[name]]$tab[match(as.character(attr(telem, attr)), htest[[name]]$names), i] <- 
            paste0('[', round(telem[1],4), ', ', round(telem[2],4), ']')   
        } else {
          htest[[name]]$tab[match(as.character(attr(telem, attr)), htest[[name]]$names), i] <- 
            sprintf(htest[[name]]$fmt, telem)
        }
      }
    }
    if (!is.null(attr)) {
      if (attr=='conf.level') {
        htest[[name]]$names <- sprintf("%.1f%% CI", 100*as.numeric(htest[[name]]$names))
      }
    }
    lines <- lines+htest[[name]]$lines
  }
  tab <- matrix('', nrow=sum(nrow)+lines, ncol=1+length(tests))
  pos <- 1
  for (j in seq(nhtest)) {
    name <- nhtest[j]
    len  <- length(htest[[name]]$names)
    tab[pos:(pos+len-1), 1] <- htest[[name]]$names
    tab[pos:(pos+len-1), 2:(1+length(tests))] <- htest[[name]]$tab
    pos <- pos+len+htest[[name]]$lines
  }
  maxlen <- apply(nchar(tab), 2, max)
  for (j in seq(tab)) {
    if (j<=nrow(tab))
      tab[j] <- sprintf('%-*s', maxlen[1+((j-1)%/%nrow(tab))], tab[j])
    else 
      tab[j] <- sprintf('%*s', maxlen[1+((j-1)%/%nrow(tab))], tab[j])
  }
  paste(apply(tab, 1, paste, collapse="   "), collapse="\n")
}

mmstat.sliderInput <- function (...) {
  iapply <- function (l) {
    ret <- l
    if (is.list(l)) {
      if (!is.null(ret$name) && (ret$name=='input')) {
        ret$attribs[['data-values']] <- ticks
      } else {
        for (i in seq(ret)) ret[[i]] <- iapply(ret[[i]])
      }
    } 
    ret
  }
  
  args <- list(...)
  if ((compareVersion(mmstat$shiny, "0.11")>=0) && !is.null(args$ticks) && length(args$ticks)) {
    ticks      <- paste0(args$ticks, collapse=',')
    args$ticks <- T
    html       <- do.call('sliderInput', args)
    html       <- iapply(html)  
  } else {
    html      <- do.call('sliderInput', args)
  }
  html
}

#mmstat.header <- function (title, width=4, inputIds=NULL, labels=NULL, values=NULL) {
#  if (length(inputIds)) {
#    cols   <- list(column(width, div(class = "brand pull-left", gettext(title))))
#    cwidth <- (12-width) %/% length(inputIds)
#    for (i in seq(inputIds)) cols[[i]] <- column(2, checkboxInput(inputIds[i], gettext(lebels[i]), values[i])),
#  } else {
#    cols   <- list(column(12, div(class = "brand pull-left", gettext(title))))
#  }
#  div(class="navbar navbar-static-top",
#      div(class = "navbar-inner", 
#          do.call('fluidRow', cols)))
#}

mmstat.ui.elem <- function (inputId, type, ...) {
  found        <- F
  elem         <- list(...)
  elem$inputId <- inputId
  elem$type    <- type
  shinytypes   <- c('actionButton', 'checkboxInput', 'checkboxGroupInput', 'dateInput', 'dateRangeInput', 'fileInput', 
                    'helpText', 'numericInput', 'radioButtons', 'selectInput', 'sliderInput',
                    'submitButton', 'textInput')
  mmstattypes  <- c('sampleSize', 'drawSample', 'speedSlider',
                    'confidenceLevel', 
                    'significance', 'testHypotheses',
                    'dataSet', 'variable1', 'variableN',
                    'fontSize', 'actionButtonGroup')
  pos <- pmatch(type, shinytypes)
  if (!is.na(pos)) {
    found     <- T
    if (elem$type=='actionButton') {
      if (is.null(elem$value)) elem$value <- 0
    }
    elem$call <- shinytypes[pos]
  }
  pos <- pmatch(type, mmstattypes)
  if (!is.na(pos)) {
    found <- T
    if (elem$type=='sampleSize') { # sliderInput for significance level
      if (is.null(elem$label)) elem$label <- gettext("Sample size (n)") 
      elem$call   <- 'mmstat.sliderInput' 
      elem$update <- 'updateSliderInput' 
      elem$step   <- 1
      elem$min    <- 1
      elem$ticks  <- T
      if (is.null(elem$value)) elem$value <- as.numeric(compareVersion(mmstat$shiny, "0.11")<0) 
    }
    if (elem$type=='drawSample') { # button to draw a new sample
      elem$call  <- 'actionButton' 
      if (is.null(elem$label)) elem$label <- gettext("Draw sample")
      if (is.null(elem$value)) elem$value <- 0
    }
    if (elem$type=='testHypotheses') { # radiobuttons for two and one-sided tests
      elem$call  <- 'radioButtons' 
      if (is.null(elem$label)) elem$label <- gettext("Choose test type")
      elem$choices = gettext(c("two.sided", "less", "greater"), "name")
      if (is.null(elem$value)) elem$value <- 'two.sided'
    }
    if (elem$type=='significance') { # special slider for significance level
      elem$call   <- 'mmstat.sliderInput' 
      elem$update <- 'updateSliderInput' 
      if (is.null(elem$ticks)) elem$ticks <- c(0.1, 0.25, 0.5, 1, 2, 5, 10, 20)
      if (is.null(elem$label)) elem$label <- HTML(gettext("Select significance level (&alpha;)"))
      elem$step <- 1
      elem$min  <- 1
      elem$max  <- length(elem$ticks)
      if (is.null(elem$value)) elem$value <- 6-as.numeric(compareVersion(mmstat$shiny, "0.11")>=0) 
    }
    if (elem$type=='confidenceLevel')  { # special slider for confidence level
      elem$call   <- 'mmstat.sliderInput' 
      elem$update <- 'updateSliderInput' 
      if (is.null(elem$ticks)) elem$ticks <- c(80, 85, 90, 95, 98, 99, 99.5, 99.9)
      if (is.null(elem$label)) elem$label <- HTML(gettext("Select confidence level (1-&alpha;)"))
      elem$step <-1
      elem$min  <- 1
      elem$max  <- length(elem$ticks)
      if (is.null(elem$value)) elem$value <- 4-as.numeric(compareVersion(mmstat$shiny, "0.11")>=0) 
    }
    if (elem$type=='dataSet') { # selectInput to select between datasets
      elem$call  <- 'selectInput' 
      if (is.null(elem$label))   elem$label    <- gettext("Select a data set")
      if (is.null(elem$choices)) elem$choices  <- mmstat.getDataNames(gsub('.rds$', '', list.files(pattern='*.rds')))
      if (is.null(elem$value))   elem$value    <- mmstat.getDataNames()
    }
    if (elem$type=='variable1') { # selectInput to select one variable
      elem$call  <- 'selectInput' 
      if (is.null(elem$label))   elem$label    <- gettext("Select a variable")
      if (is.null(elem$choices)) elem$choices  <- mmstat.getVarNames(1, elem$vartype)
      if (is.null(elem$value))   elem$value    <- mmstat.getVarNames(1, elem$vartype, 1)
    }
    if (elem$type=='variableN') { # selectInput to select one variable
      elem$call     <- 'selectInput' 
      elem$multiple <- T
      if (is.null(elem$label))   elem$label   <- gettext("Select variable(s)")
      if (is.null(elem$choices)) elem$choices <- mmstat.getVarNames(1, elem$vartype)
    }
    if (elem$type=='fontSize') { # sliderInput for choosing font size
      elem$call   <- 'mmstat.sliderInput' 
      elem$update <- 'updateSliderInput' 
      if (is.null(elem$label)) elem$label <- gettext("Font size")
      if (is.null(elem$min))   elem$min   <- 1
      if (is.null(elem$max))   elem$max   <- 1.5      
      if (is.null(elem$step))  elem$step  <- 0.05
      if (is.null(elem$value)) elem$value <- elem$min      
    }
    if (elem$type=='speedSlider') { # sliderInput for choosing font size
      elem$call   <- 'mmstat.sliderInput' 
      elem$update <- 'updateSliderInput' 
      if (is.null(elem$label)) elem$label <- list(NULL)
      if (is.null(elem$min))   elem$min   <- 0
      if (is.null(elem$max))   elem$max   <- 5 
      if (is.null(elem$step))  elem$step  <- 1
      if (is.null(elem$value)) elem$value <- elem$min        
    }    
  }  
  stopif(!found, sprintf('mmstat.ui.elem: Type "%s" unknown', type))
  if (is.null(elem$update)) elem$update <- paste0('update', ucfirst(elem$call))
  mmstat$UI[[inputId]] <<- elem    
}

mmstat.ui.call <- function(inputId, ...) {
  elem <- mmstat$UI[[inputId]]
  what <- elem$call
  args <- list(...)
  for (name in names(elem)) { if (is.null(args[[name]])) args[[name]] <- elem[[name]] }
  args$call    <- NULL
  args$update  <- NULL
  args$type    <- NULL
  args$vartype <- NULL
  if ((what=='selectInput') || (what=='checkboxGroupInput')) args$value <- NULL
  do.call(what, args)
}

mmstat.ui.update <- function(inputId, ...) {
  elem <- mmstat$UI[[inputId]]
  what <- elem$update
  args <- list(...)
  for (name in names(elem)) { if (is.null(args[[name]])) args[[name]] <- elem[[name]] }
  args$call    <- NULL
  args$update  <- NULL
  args$type    <- NULL
  args$vartype <- NULL
  if ((what=='updateSelectInput') || (what=='updateCheckboxGroupInput')) args$value <- NULL
  do.call(what, args)
}

mmstat.lang <- function(deflang=NULL) {
  pof  <- list.files(pattern="*.po$")
  if (is.null(deflang)) {
    lang <- sapply(strsplit(pof, '.', fixed=T), function(elem) { elem[1] })
    pat  <- paste0('_', lang, '$')
    path <- getwd()
    path <- strsplit(path, '/', fixed=T)[[1]]
    pos  <- -1
    lind <- -1
    for (i in seq(pat)) {
      p <- grep(pat[i], path)
      if (length(p)) {
        p <- max(p)
        if (p>pos) { pos <- p; lind <- i; }
      }
    }
  } else {
    lind <- match (paste0(deflang, '.po'), pof)
    if (is.na(lind)) lind <- (-1)
  }
  msgfn <- ifelse(lind>0, pof[lind], "default.po")
  mmstat.warn (!file.exists(msgfn), sprintf("File '%s' does not exist", msgfn))
  msg    <- paste(readLines(msgfn), collapse=" ")
  msgid  <- regmatches(msg, gregexpr('msgid\\s*".*?"', msg))
  tmp    <- strsplit(msgid[[1]], '"')
  msgid  <- sapply(tmp, function (vec) { paste0(vec[2:length(vec)]) } )
  msgstr <- regmatches(msg, gregexpr('msgstr\\s*".*?"', msg))
  tmp    <- strsplit(msgstr[[1]], '"')
  msgstr <- sapply(tmp, function (vec) { paste0(vec[2:length(vec)]) } )
  mmstat$messages <<- list(id=msgid, str=msgstr)
}

htmlTable <- function (tab, vars=NULL, lines=NULL, cex=1, title='') {
  html  <- sprintf('<table style="text-align:right;width:100%%;font-size:%i%%;">', as.integer(100*cex))
  if (length(vars)==2) html <- paste0(html, sprintf('<tr><td rowspan="2" align="left">%s</td><td colspan="%i">', title, ncol(tab)), sprintf(gettext("Columns: %s"), gettext(vars[1])), '</td></tr>')    
  width  <- as.integer(100/(1+ncol(tab)))
#  html   <- paste0(html, sprintf('<TR><TD style="width:%i%%"></TD>', width))
  html   <- paste0(html, '<TR>')
#  align  <- ifelse (is.na(as.numeric(colnames(tab))), 'left', 'right')
#  html   <- paste0(html, paste0(sprintf('<TD style="text-align:%s;width:%i%%;background-color:#AAAAAA"><b>', align, width), sprintf("%s", colnames(tab)), '</b></TD>', collapse=""))
  html   <- paste0(html, paste0(sprintf('<TD style="width:%i%%;background-color:#AAAAAA"><b>', width), sprintf("%s", colnames(tab)), '</b></TD>', collapse=""))
  html   <- paste0(html, '</TR>')
  align  <- ifelse (is.na(suppressWarnings(as.numeric(rownames(tab)))), 'left', 'right')
  for (i in seq(nrow(tab))) {
    html <- paste0(html, sprintf('<TR><TD style="text-align:%s;background-color:#AAAAAA"><b>%s</b></TD>', align[i], rownames(tab)[i]))
    if (i%%2==0) {
      html <- paste0(html, paste0('<TD style="background-color:#CCCCCC">', sprintf("%i", tab[i,]), '</TD>', collapse=""))
    } else {
      html <- paste0(html, paste0('<TD>', sprintf("%i", tab[i,]), '</TD>', collapse=""))        
    }
    html <- paste0(html, '</TR>')
  }
  if (length(vars)==2) 
    html <- paste0(html, sprintf('<tr><td colspan="%i" style="text-align:left;">', 1+ncol(tab)), 
                         sprintf(gettext("Rows: %s"), vars[2]), '</td></tr>',
                         sprintf('<tr><td colspan="%i"><hr></td></tr>', 1+ncol(tab)))
  for (i in seq(lines)) 
    html  <- paste0(html, sprintf('<tr><td colspan="%i" style="text-align:left;">', 1+ncol(tab)), lines[i], '</td></tr>')
  html <- paste0(html, '</table>')
  html
}

ucfirst <- function (txt) { return (paste0(toupper(substr(txt, 1, 1)), substring(txt, 2))); }

gettext <- function (msg, utype="vector") {
  type <- pmatch(utype, c("name", "numeric"))
  if (is.na(type)) {
    #print(paste('Msg:', msg))
    ret <- paste0(ifelse(mmstat$debug>2, '?', ''), msg)
    #print(paste('Ret:', ret))
    pos <- match(msg, mmstat$messages$id)
    ind <- (1:length(pos))[!is.na(pos)]
    ret[ind] <- mmstat$messages$str[pos[ind]]
    #print(paste('Out:', ret))
    #for (i in 1:length(pos)) {
    #  if (!is.na(pos[i])) ret[i] <- mmstat$messages$str[pos[i]]
    #}
  } else if (type==1) {
    ret        <- as.list(msg)
    names(ret) <- gettext(msg)
  } else if (type==2) {
    ret        <- as.list(seq(msg))
    names(ret) <- gettext(msg)
  }
  return (ret)
}

# Logging

mmstat.getLog <- function (session) {
  if (!mmstat$debug) return ("")
  invalidateLater(100, session)
  paste0('<hr>', paste(mmstat$log, collapse="<br>"))
}

mmstat.log  <- function (txt) { if (mmstat$debug>1) mmstat$log <<- cbind(sprintf("%s (Info): %s", date(), txt), mmstat$log) } 
mmstat.warn <- function (cond, txt) { if ((mmstat$debug>0) && cond) mmstat$log <<- cbind(sprintf("%s (Warn): %s", date(), txt), mmstat$log) } 
mmstat.input <- function (input) {
  if (mmstat$debug>1) {
    ni  <-names(input)
    for (i in seq(ni)) {
      out <- capture.output(input[[ni[i]]])
      out[1] <- paste0(ni[i], ': ', out[1])
      for (j in seq(out)) mmstat$log <<- cbind(sprintf("%s (Input): %s", date(), out[j]), mmstat$log)  
    }
  }
}

# Main

mmstat <- list(debug = 1,
               shiny = sessionInfo()$otherPkgs$shiny$Version,
               col   = list(daquamarine="#1B9E77",  dorange="#D95F02",   dblue="#7570B3",   dpink="#E7298A", 	
                            dgreen="#66A61E",     	dyellow="#E6AB02", 	dgold="#A6761D", 	dgray="#666666",
                            laquamarine="#66C2A5",  lorange="#FC8D62",  lblue="#8DA0CB",  lpink="#E78AC3", 	
                            lgreen="#A6D854",     	lyellow="#FFD92F", 	lgold="#E5C494", 	lgray="#B3B3B3"),
               alpha = c(0.1, 0.25, 0.5, 1, 2.5, 5, 10, 20),
               UI    = vector("list", 0)
              )
mmstat.log("Starting app")
mmstat.lang()
Sys.setlocale("LC_ALL", gettext("LC_ALL"))







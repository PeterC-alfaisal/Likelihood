
# This file is a generated template, your changes will not be overwritten

contabClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "contabClass",
    inherit = contabBase,
    private = list(
      .cleanData = function() {
        
        rowVarName <- self$options$rows
        colVarName <- self$options$cols
        countsName <- self$options$counts
        
        data <- jmvcore::select(self$data, c(rowVarName, colVarName, countsName))
        data <- jmvcore::naOmit(data)
        
        if ( ! is.null(rowVarName))
          data[[rowVarName]] <- as.factor(data[[rowVarName]])
        if ( ! is.null(colVarName))
          data[[colVarName]] <- as.factor(data[[colVarName]])
        if ( ! is.null(countsName))
          data[[countsName]]  <- jmvcore::toNumeric(data[[countsName]])
        
        data
      },
      .run = function() {
        
        rowVarName <- self$options$rows
        colVarName <- self$options$cols
        countsName <- self$options$counts
        
        if (is.null(rowVarName) || is.null(colVarName))
          return()
        
        data <- private$.cleanData()
        
        if (nlevels(data[[rowVarName]]) < 2)
          jmvcore::reject(.("Row variable '{var}' contains fewer than 2 levels"), code='', var=rowVarName)
        if (nlevels(data[[colVarName]]) < 2)
          jmvcore::reject(.("Column variable '{var}' contains fewer than 2 levels"), code='', var=colVarName)
        
        if ( ! is.null(countsName)) {
          countCol <- jmvcore::toNumeric(data[[countsName]])
          
          if (any(countCol < 0, na.rm=TRUE))
            jmvcore::reject(.('Counts may not be negative'))
          if (any(is.infinite(countCol)))
            jmvcore::reject(.('Counts may not be infinite'))
        }
        
        rowVar <- data[[rowVarName]]
        colVar <- data[[colVarName]]
        
        freqs <- self$results$freqs
        
        if (! is.null(countsName))
          result <- stats::xtabs(countCol ~ rowVar + colVar)
        else
          result <- base::table(rowVar, colVar)
        
        colTotals <- apply(result, 2, base::sum)
        freqRowNo <- 1
        
        for (rowNo in seq_len(nrow(result))) {
          
          counts <- result[rowNo,]
          
          if (length(counts) > 0) {
            rowTotal <- sum(counts)
            pcRow <- counts / rowTotal
            pcCol <- counts / colTotals
            
            names(counts) <- paste0(seq_len(length(counts)), '[count]')
            names(pcRow)  <- paste0(seq_len(length(counts)), '[pcRow]')
            names(pcCol)  <- paste0(seq_len(length(counts)), '[pcCol]')
            names(rowTotal)  <- '.total[count]'
            
            freqs$setRow(rowNo=rowNo, values=c(counts, pcRow, pcCol, rowTotal))
            freqRowNo <- freqRowNo + 1
          }
        }
        
        nCols <- length(colTotals)
        
        N <- base::sum(colTotals)
        rowTotal <- N
        values <- as.list(colTotals)
        names(values) <- paste0(1:nCols, '[count]')
        values[['.total[count]']] <- rowTotal
        
        pcRow <- colTotals / rowTotal
        pcRow <- as.list(pcRow)
        names(pcRow) <- paste0(1:nCols, '[pcRow]')
        
        pcCol <- rep(1, nCols)
        pcCol <- as.list(pcCol)
        names(pcCol) <- paste0(1:nCols, '[pcCol]')
        
        names(rowTotal)  <- '.total[count]'
        
        values <- c(values, pcRow, pcCol, rowTotal)
        
        freqs$setRow(freqRowNo, values=values)
        
        rowVarName <- self$options$rows
        colVarName <- self$options$cols
        countsName <- self$options$counts
        
        if (is.null(rowVarName) || is.null(colVarName))
          return()
        
        freqs <- self$results$freqs
        
        rlevels <- levels(self$data[[rowVarName]])
        clevels <- levels(self$data[[colVarName]])
        nRows <- length(rlevels)
        
        subData <- jmvcore::select(data, c(rowVarName, colVarName))
        .COUNTS <- jmvcore::toNumeric(data[[countsName]])
        
        tabt <- xtabs(.COUNTS ~ ., data=subData)

        # calculating the interaction support
        S2way <- 0
        suppressWarnings(lt <- chisq.test(tabt, correct=FALSE)) # ignore warning message
        S2way <- sum( lt$observed * log(lt$observed/lt$expected) )
        dfi <- (lt$parameter)
        S2way_c <- S2way - (dfi-1)/2  # corrected for df

        # main marginal totals
        row_sum <- rowSums(tabt)
        col_sum <- colSums(tabt)
        grandtot <- sum(tabt)
        dfr <- length(row_sum)-1; dfc <- length(col_sum)-1; dft <- nRows*nCols-1
        RowMain <- sum(row_sum*log(row_sum))-grandtot*log(grandtot) + grandtot*log(length(row_sum))
        RowMain_c <- RowMain - (dfr-1)/2 # corrected for row df
        
        ColMain <- sum(col_sum*log(col_sum))-grandtot*log(grandtot) + grandtot*log(length(col_sum))
        ColMain_c <- ColMain - (dfc-1)/2 # corrected for column df
        
        exp_row <- grandtot/nRows # expected values
        exp_col <- grandtot/nCols
        exp_cell <- grandtot/(nRows*nCols)

        # Total S
        Tot_S <- sum(tabt*log(tabt))-sum(tabt)*log(sum(tabt)/(nRows*nCols))
        # same as components added together (without correction for df)
        Tot_S_c <- Tot_S - (dft-1)/2 # corrected for column df
        
        toogood <- dfi/2*(log(dfi/lt$statistic)) - (dfi - lt$statistic)/2
        suppressWarnings(ltr <- chisq.test(row_sum, correct=FALSE)) # ignore warning message
        toogoodr <- dfr/2*(log(dfr/ltr$statistic)) - (dfr - ltr$statistic)/2
        suppressWarnings(ltc <- chisq.test(col_sum, correct=FALSE)) # ignore warning message
        toogoodc <- dfc/2*(log(dfc/ltc$statistic)) - (dfc - ltc$statistic)/2
        
        # evidence for trend
#        trX <- NULL
#        tr <- NULL
#        trX$p.value <- NULL
#        if (length(col_sum)>=3) {
#          table.pos <- tabt[1:length(col_sum)]
#          trX <- prop.trend.test(tabt[1,], col_sum)
#          tr <- unname(trX$statistic)/2      # S for trend
#        }
        
        gt_p <- 1-pchisq(2*Tot_S,dft)
        gr_p <- 1-pchisq(2*RowMain_c,dfr)
        gc_p <- 1-pchisq(2*ColMain_c,dfc)
        gi_p <- 1-pchisq(2*S2way_c,dfi)
        
        int_text <- paste(rowVarName," x ", colVarName)
        
        table <- self$results$cttma
        table$setNote('Note', "Sc is S corrected for degrees of freedom using Edwards's Occam's bonus, see reference")
        table$setRow(rowNo=1, values=list(var= rowVarName, Value=exp_row, 
                                          S=RowMain, Sc=RowMain_c, G=2*RowMain_c, df=dfr, p=gr_p))
        table$setRow(rowNo=2, values=list(var=colVarName, Value=exp_col, 
                                          S=ColMain, Sc=ColMain_c, G=2*ColMain_c, df=dfc, p=gc_p))
        table$setRow(rowNo=3, values=list(var=int_text, Value=NULL, 
                                          S=S2way, Sc=S2way_c, G=2*S2way_c, df=dfi, p=gi_p))
        table$setRow(rowNo=4, values=list(var="Total", Value=exp_cell, 
                                          S=Tot_S, Sc=Tot_S_c, G=2*Tot_S_c, df=dft, p=gt_p))

        table <- self$results$ctt3
        table$setNote('Note', "A large S value indicates that variances are either larger or smaller than expected") 
        table$setRow(rowNo=1, values=list(var= int_text, Sv=toogood, X2=lt$statistic, dfv=dfi, 
                                  pv=lt$p.value, pv1=1-lt$p.value))
        table$setRow(rowNo=2, values=list(var= rowVarName, Sv=toogoodr, X2=ltr$statistic, dfv=dfr, 
                                          pv=ltr$p.value, pv1=1-ltr$p.value))
        table$setRow(rowNo=3, values=list(var= colVarName, Sv=toogoodc, X2=ltc$statistic, dfv=dfc, 
                                          pv=ltc$p.value, pv1=1-ltc$p.value))
        
        if(isTRUE(self$options$varA)) { 
          
          table <- self$results$ctt3
          table$setVisible(TRUE)
          
        }
      },
      
#### Plot functions ----
.initBarPlot = function() {
  image <- self$results$get('barplot')
  
  width <- 450
  height <- 400
  
    image$setSize(width * 2, height)
},
.barPlot = function(image, ggtheme, theme, ...) {
  
  if (! self$options$barplot)
    return()
  
  rowVarName <- self$options$rows
  colVarName <- self$options$cols
  countsName <- self$options$counts

  if (is.null(rowVarName) || is.null(colVarName))
    return()
  
  data <- private$.cleanData()
  data <- na.omit(data)
  
  if (! is.null(countsName)){
    untable <- function (df, counts) df[rep(1:nrow(df), counts), ]
    data <- untable(data[, c(rowVarName, colVarName)], counts=data[, countsName])
  }
  
  formula <- jmvcore::composeFormula(NULL, c(rowVarName, colVarName))
  counts <- xtabs(formula, data)
  d <- dim(counts)
  
  expand <- list()
  for (i in c(rowVarName, colVarName))
    expand[[i]] <- base::levels(data[[i]])
  tab <- expand.grid(expand)
  tab$Counts <- as.numeric(counts)
  
  if (self$options$yaxis == "ypc") { # percentages
    props <- counts
    
    if (self$options$yaxisPc == "column_pc") {
      pctVarName <- colVarName
    } else if (self$options$yaxisPc == "row_pc") {
      pctVarName <- rowVarName
    } else { # total
      pctVarName <- NULL
    }

    props <- proportions(counts, pctVarName)

    tab$Percentages <- as.numeric(props) * 100
  }
  
  if (self$options$xaxis == "xcols") {
    xVarName <- ensym(colVarName)
    zVarName <- ensym(rowVarName)
  } else {
    xVarName <- ensym(rowVarName)
    zVarName <- ensym(colVarName)
  }
  
  position <- self$options$bartype
  
  if (self$options$yaxis == "ycounts") {
    p <- ggplot(data=tab, aes(y=Counts, x=!!xVarName, fill=!!zVarName)) +
      geom_col(position=position, width = 0.7) +
      labs(y = .("Counts"))
  } else {
    p <- ggplot(data=tab, aes(y=Percentages, x=!!xVarName, fill=!!zVarName)) +
      geom_col(position=position, width = 0.7)
    
    if (self$options$yaxisPc == "total_pc") {
      p <- p + labs(y = .("Percentages of total"))
    } else {
      p <- p + labs(y = jmvcore::format(.("Percentages within {var}"), var=pctVarName))
    }
  }
  
  p <- p + ggtheme
  
  return(p)
},      .init = function() {
        
        freqs <- self$results$get('freqs')
        rowVarName <- self$options$rows
        colVarName <- self$options$cols
        
        data <- private$.cleanData()
        
        # add the row column, containing the row variable
        # fill in dots, if no row variable specified
        
        if ( ! is.null(rowVarName))
          title <- rowVarName
        else
          title <- '.'
        
        freqs$addColumn(
          name=title,
          title=title,
          type='text')
        
        # add the column columns (from the column variable)
        # fill in dots, if no column variable specified
        
        if ( ! is.null(colVarName)) {
          superTitle <- colVarName
          levels <- base::levels(data[[colVarName]])
        }
        else {
          superTitle <- '.'
          levels <- c('.', '.')
        }
        
        hasSubRows <- sum(self$options$pcRow,
                          self$options$pcCol) > 0
        
        subNames  <- c('[count]', '[pcRow]', '[pcCol]')
        subTitles <- c(.('Count'), .('% within row'), .('% within column'))
        visible   <- c('TRUE', '(pcRow)', '(pcCol)')
        types     <- c('integer', 'number', 'number')
        formats   <- c('', 'pc', 'pc')
        
        # iterate over the sub rows
        
        for (j in seq_along(subNames)) {
          subName <- subNames[[j]]
          if (j == 1)
            v <- '(pcRow || pcCol)'
          else
            v <- visible[j]
          
          freqs$addColumn(
            name=paste0('type', subName),
            title='',
            type='text',
            visible=v)
        }
        
        for (i in seq_along(levels)) {
          level <- levels[[i]]
          
          for (j in seq_along(subNames)) {
            subName <- subNames[[j]]
            freqs$addColumn(
              name=paste0(i, subName),
              title=level,
              superTitle=superTitle,
              type=types[j],
              format=formats[j],
              visible=visible[j])
          }
        }
        
        # add the Total column
        
        freqs$addColumn(
          name='.total[count]',
          title=.('Total'),
          type='integer')
        
        # populate the first column with levels of the row variable
        
        values <- list()
        for (i in seq_along(subNames))
          values[[paste0('type', subNames[i])]] <- subTitles[i]
        
        rows <- private$.grid(data, incRows=TRUE)
        
        for (i in seq_len(nrow(rows))) {
          for (name in dimnames(rows)[[2]]) {
            value <- as.character(rows[i, name])
            if (value == '.total')
              value <- .('Total')
            values[[name]] <- value
          }
          key <- paste0(rows[i,], collapse='`')
          freqs$addRow(rowKey=key, values=values)
          
          if (i == 1)
            freqs$addFormat(rowNo=i, 1, Cell.BEGIN_GROUP)
          else if (i == nrow(rows) - 1)
            freqs$addFormat(rowNo=i, 1, Cell.END_GROUP)
          else if (i == nrow(rows))
            freqs$addFormat(rowNo=i, 1, Cell.BEGIN_END_GROUP)
        }
      },
      .grid=function(data, incRows=FALSE) {
        
        rowVarName <- self$options$get('rows')
        
        expand <- list()
        
        if (incRows) {
          if (is.null(rowVarName))
            expand[['.']] <- c('.', '. ', .('Total'))
          else
            expand[[rowVarName]] <- c(base::levels(data[[rowVarName]]), '.total')
        }
        
        rows <- rev(expand.grid(expand))
        
        rows
      },
      .sourcifyOption = function(option) {
        if (option$name %in% c('rows', 'cols', 'counts'))
          return('')
        super$.sourcifyOption(option)
      },
      .formula=function() {
        if (is.null(self$options$rows) || is.null(self$options$cols))
          return('~')
        jmvcore:::composeFormula(self$options$counts, list(list(self$options$rows, self$options$cols)))
      }
    )
)

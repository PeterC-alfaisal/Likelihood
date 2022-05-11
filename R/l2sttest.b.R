
# This file is a generated template, your changes will not be overwritten

l2sttestClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "l2sttestClass",
    inherit = l2sttestBase,
    private = list(
      .run = function() {
        
        groupVarName <- self$options$group
        depVarNames <- self$options$dep
        varNames <- c(groupVarName, depVarNames)
        
        if (is.null(groupVarName) || length(depVarNames) == 0)
          return()
        
        formula <- jmvcore::constructFormula(depVarNames, groupVarName)
        formula <- as.formula(formula)
        if (isTRUE(self$options$welch)) {veq <- FALSE} else {veq <- TRUE}
        results <- t.test(formula, self$data, mu = self$options$nul, var.equal = veq)
        m.obs <- results$estimate[1]-results$estimate[2]
        df <- results$parameter
        nsam   <- aggregate(formula, self$data, function(x) length(x))[,2]
        N <- nsam[1] + nsam[2]
        sed <- results$stderr
        tval <- unname(results$statistic)
        like0 <- (1 + tval^2/df)^-(N/2) #L0
        # Maximum likelihood ratio and S
        S_m <- log(like0)
        results1 <- t.test(formula, self$data, mu = self$options$alt, var.equal = veq)
        like1 <- unname((1 + results1$statistic^2/df)^-(N/2)) #Alt H
        S_1 <- log(like1)
        
        # Add Likelihood interval
        lolim <- m.obs - sed*sqrt((exp(self$options$lint*2/N)-1)*df)
        hilim <- m.obs + sed*sqrt((exp(self$options$lint*2/N)-1)*df)
        
        means  <- aggregate(formula, self$data, mean)[,2]
        Median  <- aggregate(formula, self$data, median)[,2]
        sds  <- aggregate(formula, self$data, sd)[,2]
        ses    <- aggregate(formula, self$data, function(x) sd(x)/sqrt(length(x)))[,2]
        sel    <- means - ses*sqrt((exp(self$options$lint*2/nsam)-1)*(nsam-1))  # S-2 lower bound
        seu    <- means + ses*sqrt((exp(self$options$lint*2/nsam)-1)*(nsam-1))  # upper bound
        levels <- base::levels(self$data[[groupVarName]])
        
        table <- self$results$l2sttest
        if(isTRUE (self$options$welch))
          table$setNote('Note', "Uses Welch's procedure for unequal variances") 
        table$setRow(rowNo=1, values=list(var= "Null vs observed", Value=results$null.value, 
                                          mdiff= self$options$nul-m.obs, sed=sed, S=S_m, t=tval, df=df, p=results$p.value))
        table$setRow(rowNo=2, values=list(var="Alt. H vs observed", Value=results1$null.value, mdiff=self$options$alt-m.obs, 
                                          sed=sed, S=S_1, t=results1$statistic, df=df, p=results1$p.value))
        table$setRow(rowNo=3, values=list(var="Alt. H vs Null", Value=NULL, 
                                          mdiff= self$options$alt-self$options$nul, sed=sed, S=S_1-S_m, t=NULL, df=NULL, p=NULL))
        
        table <- self$results$l2sttest2
        siWidthTitle <- jmvcore::format(.('S-{lint} Likelihood Interval'), lint=self$options$lint)
        table$getColumn('Lower')$setSuperTitle(siWidthTitle)
        table$getColumn('Upper')$setSuperTitle(siWidthTitle)
        table$setRow(rowNo=1, values=list(mdiff=m.obs, Lower=lolim, Upper=hilim))
        
        table <- self$results$l2sttestd
        table$setRow(rowNo=1, values=list(gp=levels[1], N=nsam[1], Mean=means[1], 
                                          Median=Median[1], SD=sds[1], SE=ses[1]))
        table$setRow(rowNo=2, values=list(gp=levels[2], N=nsam[2], Mean=means[2], 
                                          Median=Median[2], SD=sds[2], SE=ses[2]))
        
        if(isTRUE(self$options$dtab)) { 
          
          table <- self$results$l2sttestd
          table$setVisible(TRUE)
          
        }
        
        plotData <- data.frame(level=levels, mean=means, sel=sel, seu=seu)
        
        image <- self$results$plot
        image$setState(plotData)
        
        if(isTRUE(self$options$plt)) {
          
          plot <- self$results$plot
          plot$setVisible(TRUE)
          
        }
        
        g <- data.frame(mobs=m.obs,sed=sed, df=df, N=N, 
                        null=results$null.value, alth=results1$null.value, lolim=lolim, hilim=hilim)
        imagec <- self$results$plotc
        imagec$setState(g)
        
        if(isTRUE(self$options$pll)) {
          
          plot <- self$results$plotc
          plot$setVisible(TRUE)
          
        }
      },
      
      .plot=function(image, ...) {
        plotData <- image$state
        plot <- ggplot(plotData, aes(x=level, y=mean)) +
          geom_errorbar(aes(ymin=sel, ymax=seu, width=.1)) +
          geom_point(shape=21, size=3, fill="white") +
          labs(title=self$options$dep, x=self$options$group, xlab=base::levels(self$data[[self$options$group]]))
        print(plot)
        TRUE
        
      },
      .plotc=function(imagec, ...) {
        
        g <- imagec$state
        
        plot <- curve((1 + ((g$mobs-x)/g$sed)^2/g$df)^-(g$N/2),
                      xlim = c(g$mobs-5*g$sed,g$mobs+5*g$sed), ylab = "Likelihood",
                      xlab = "Mean difference")
        segments(g$lolim, exp(-self$options$lint), g$hilim, exp(-self$options$lint), col = "red")
        lines(c(g$mobs,g$mobs),c(0,1),lty=2) # add mean as dashed line
        lines(c(g$null, g$null), c(0,(1 + ((g$mobs-g$null)/g$sed)^2/g$df)^-(g$N/2)),lty=1, col = "black") # for null
        lines(c(g$alth,g$alth),c(0,(1 + ((g$mobs-g$alth)/g$sed)^2/g$df)^-(g$N/2)),lty=1, col = "blue") # alt.h
        
        print(plot)
        TRUE
        
      }
    )
)

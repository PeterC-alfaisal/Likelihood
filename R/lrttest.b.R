
# This file is a generated template, your changes will not be overwritten

lrttestClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "lrttestClass",
    inherit = lrttestBase,
    private = list(
      .run = function() {
        
        data1 <- jmvcore::toNumeric(self$data[[self$options$depa]])
        data2 <- jmvcore::toNumeric(self$data[[self$options$depb]])
        adata <- data1 - data2
        
        results <- t.test(adata, mu = self$options$nul)
        m.obs <- results$estimate
        se.obs <- results$stderr
        df <- results$parameter
        N=df+1
        m1.obs <- mean(data1[1:N])
        m2.obs <- mean(data2[1:N])
        med1.obs <- median(data1[1:N])
        med2.obs <- median(data2[1:N])
        sd1 <- sd(data1[1:N])
        sd2 <- sd(data2[1:N])
        sed1 <- sd1/sqrt(N)
        sed2 <- sd2/sqrt(N)
        
        sed <- results$stderr
        sdd <- sed*sqrt(N)
        tval <- unname(results$statistic)
        like0 <- (1 + tval^2/df)^-(N/2) #L0
        # Maximum likelihood ratio and S
        S_m <- log(like0)
        
        results1 <- t.test(adata, mu = self$options$alt)
        like1 <- unname((1 + results1$statistic^2/df)^-(N/2)) #Alt H
        S_1 <- log(like1)
        
        # Add Likelihood interval
        lolim <- m.obs - sed*sqrt((exp(self$options$lint*2/N)-1)*df)
        hilim <- m.obs + sed*sqrt((exp(self$options$lint*2/N)-1)*df)
        
        sel    <- m.obs - sed*sqrt((exp(self$options$lint*2/N)-1)*(df))  # S-2 lower bound
        seu    <- m.obs + sed*sqrt((exp(self$options$lint*2/N)-1)*(df))  # upper bound
        
        table <- self$results$lrttest
        table$setRow(rowNo=1, values=list(var= "Null vs observed", Value=results$null.value, mdiff= self$options$nul-m.obs, 
                                          sed=sed, S=S_m, t=tval, df=df, p=results$p.value))
        table$setRow(rowNo=2, values=list(var="Alt. H vs observed", Value=results1$null.value, mdiff= self$options$alt-m.obs, 
                                          sed=sed, S=S_1, t=results1$statistic, df=df, p=results1$p.value))
        table$setRow(rowNo=3, values=list(var="Alt. H vs Null", Value=NULL, mdiff= self$options$alt-self$options$nul, 
                                          sed=sed, S=S_1-S_m, t=NULL, df=NULL, p=NULL))
        
        table <- self$results$lrttest2
        siWidthTitle <- jmvcore::format(.('S-{lint} Likelihood Interval'), lint=self$options$lint)
        table$getColumn('Lower')$setSuperTitle(siWidthTitle)
        table$getColumn('Upper')$setSuperTitle(siWidthTitle)
        table$setRow(rowNo=1, values=list(diff=m.obs, Lower=lolim, Upper=hilim))
        
        table <- self$results$lrttestd
        table$setRow(rowNo=1, values=list(gp=self$options$depa, N=N, Mean=m1.obs, Median=med1.obs, SD=sd1, SE=sed1))
        table$setRow(rowNo=2, values=list(gp=self$options$depb, N=N, Mean=m2.obs, Median=med2.obs, SD=sd2, SE=sed2))
        
        if(isTRUE(self$options$dtab)) { 
          
          table <- self$results$lrttestd
          table$setVisible(TRUE)
          
        }
        
        plotData <- data.frame(Mean=m.obs, sel=sel, seu=seu)
        
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
          
          plotc <- self$results$plotc
          plotc$setVisible(TRUE)
          
        }
        
        
      },
      
      .plot=function(image, ...) {
        plotData <- image$state
        
        xaxis_lab <- paste(self$options$depa, " - ", self$options$depb)
        plot <- ggplot(plotData, aes(x="", y=Mean)) +
          geom_errorbar(aes(ymin=sel, ymax=seu, width=.1)) +
          geom_point(shape=21, size=3, fill="white") +
          labs(x=xaxis_lab)
        
        print(plot)
        TRUE
        
      },
      .plotc=function(imagec, ...) {
        
        g <- imagec$state
        
        plot <- curve((1 + ((g$mobs-x)/g$sed)^2/g$df)^-(g$N/2),
                      xlim = c(g$mobs-5*g$sed,g$mobs+5*g$sed), ylab = "Likelihood",
                      xlab = "Observed mean")
        segments(g$lolim, exp(-self$options$lint), g$hilim, exp(-self$options$lint), col = "red")
        lines(c(g$mobs,g$mobs),c(0,1),lty=2) # add mean as dashed line
        lines(c(g$null, g$null), c(0,(1 + ((g$mobs-g$null)/g$sed)^2/g$df)^-(g$N/2)),lty=1, col = "black") # for null
        lines(c(g$alth,g$alth),c(0,(1 + ((g$mobs-g$alth)/g$sed)^2/g$df)^-(g$N/2)),lty=1, col = "blue") # alt.h
        
        print(plot)
        TRUE
        
      }
    )
)


# This file is a generated template, your changes will not be overwritten

lttestClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "lttestClass",
    inherit = lttestBase,
    private = list(
        .run = function() {
          
                             
          data <- jmvcore::toNumeric(self$data[[self$options$dep]])
          
          results <- t.test(data, mu = self$options$nul)
          m.obs <- results$estimate
          
          df <- results$parameter
          N=df+1
          med.obs <- median(data[1:N])
          
          sed <- results$stderr
          sdd <- sed*sqrt(N)
          tval <- unname(results$statistic)
          like0 <- (1 + tval^2/df)^-(N/2) #L0
          
          # Maximum likelihood ratio and S
          S_m <- log(like0)
          
          results1 <- t.test(data, mu = self$options$alt)
          like1 <- unname((1 + results1$statistic^2/df)^-(N/2)) #Alt H
          S_1 <- log(like1)
          
          t.AhvNul <- (self$options$alt-self$options$nul)/results$stderr
          p.AhvNul <- pt(t.AhvNul,df)
          
          # Add Likelihood interval
          lolim <- m.obs - sed*sqrt((exp(self$options$lint*2/N)-1)*df)
          hilim <- m.obs + sed*sqrt((exp(self$options$lint*2/N)-1)*df)
          
          table <- self$results$lttest
          table$setRow(rowNo=1, values=list(var= "Null vs observed", Value=results$null.value, 
                      mdiff= self$options$nul-m.obs, SE=sed, S=S_m, t=tval, df=df, p=results$p.value))
          table$setRow(rowNo=2, values=list(var="Alt. H vs observed", Value=results1$null.value, 
                      mdiff= self$options$alt-m.obs, SE=sed, S=S_1, t=results1$statistic, df=df, p=results1$p.value))
          table$setRow(rowNo=3, values=list(var="Alt. H vs Null", Value=NULL, 
                      mdiff= self$options$alt-self$options$nul, SE=sed, S=S_1-S_m, t=NULL, df=NULL, p=NULL))

          table <- self$results$lttest2
          siWidthTitle <- jmvcore::format(.('S-{lint} Likelihood Interval'), lint=self$options$lint)
          table$getColumn('Lower')$setSuperTitle(siWidthTitle)
          table$getColumn('Upper')$setSuperTitle(siWidthTitle)
          table$setRow(rowNo=1, values=list(Mean = m.obs, Lower=lolim, Upper=hilim))

          table <- self$results$lttestd
          table$setRow(rowNo=1, values=list(gp=self$options$dep, N=N, Mean=m.obs,
                                    Median=med.obs, SD=sdd, SE=sed))

          if(isTRUE(self$options$dtab)) { 
            
            table <- self$results$lttestd
            table$setVisible(TRUE)
            
          }
          
          plotData <- data.frame(mean=m.obs, lolim=lolim, hilim=hilim)
          
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
          plot <- ggplot(plotData, aes(x="", y=mean)) +
            geom_errorbar(aes(ymin=lolim, ymax=hilim, width=.1)) +
            geom_point(shape=21, size=3, fill="white") +
          labs(x=self$options$dep)
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

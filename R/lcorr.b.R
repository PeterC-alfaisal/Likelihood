
# This file is a generated template, your changes will not be overwritten

LcorrClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "LcorrClass",
    inherit = LcorrBase,
    private = list(
      .run = function() {
        
        data1 <- jmvcore::toNumeric(self$data[[self$options$depa]])
        data2 <- jmvcore::toNumeric(self$data[[self$options$depb]])
        adata <- data1 - data2
        
        exp.r <- self$options$alt
        L.int <- self$options$lint
        ciw <- self$options$ciWidth

        m <- cor.test(data1, data2, conf.level = ciw/100)
        null <- unname(m$null.value)
        r <- unname(m$estimate)
        t <- unname(m$statistic)
        p <- unname(m$p.value)
        df <- unname(m$parameter)
        N <- df + 2
        
        # using z
        rtrans <- function(r_value) (0.5 * log(abs((1 + r_value)/(1 - r_value))))
        se <- 1/(sqrt(N - 3))
        z0 <- (null - rtrans(r))/se
        
        # support for observed versus null
        S0 <- -z0^2/2

        z1 <- (rtrans(exp.r)-rtrans(r))/se
        S1 <- -z1^2/2             # support for exp.r versus observed r
        
        x_lim <- c(r-3.5*se, r+3.5*se)   # adjust to suit x limits on plot
        if (x_lim[1] < -1) x_lim[1]=-1
        if (x_lim[2] > 1) x_lim[2]=1
        
        r_dash_lower <- rtrans(r)-se*sqrt(L.int*2)
        r_dash_upper <- rtrans(r)+se*sqrt(L.int*2)
        
        lolim <- (exp(2*r_dash_lower)-1)/(exp(2*r_dash_lower)+1)
        hilim <- (exp(2*r_dash_upper)-1)/(exp(2*r_dash_upper)+1)

        # likelihood-based % confidence intervals
        toler <- 0.0001
        f <- function(x, r, goal) {
          (-((rtrans(x)-rtrans(r))/se)^2/2-goal)^2
        }

        goal = -qchisq(self$options$ciWidth/100,1)/2
        xmin1 <- optimize(f, c(-1, r), tol = toler, r, goal)
        xmin2 <- optimize(f, c(r, 1), tol = toler, r, goal)
        beg <- xmin1$minimum
        end <- xmin2$minimum
        
        table <- self$results$lcor
        table$setRow(rowNo=1, values=list(var= "Null vs observed", Value=0, rdiff= null-r, 
                                          S=S0, t=t, df=df, p=p))
        table$setRow(rowNo=2, values=list(var= "Alt. H versus observed", Value=exp.r, rdiff= exp.r-r, 
                                          S=S1, t=NULL, df=NULL, p=NULL))
        table$setRow(rowNo=3, values=list(var="Alt. H vs Null", Value=exp.r, rdiff= exp.r-null, 
                                          S=S1-S0, t=NULL, df=NULL, p=NULL))

        lintlev <- toString(self$options$lint); conflev <- paste0(self$options$ciWidth,"%")
        
        table <- self$results$lcor1
        table$setNote('Note', "*See reference") 
        siWidthTitle <- jmvcore::format(.('Interval'))
        table$getColumn('Lower')$setSuperTitle(siWidthTitle)
        table$getColumn('Upper')$setSuperTitle(siWidthTitle)
        table$setRow(rowNo=1, values=list(Interval="Support", Level=lintlev, r=r, 
                                          Lower=lolim, Upper=hilim))
        table$setRow(rowNo=2, values=list(Interval="Likelihood-based*", Level=conflev, r=r, 
                                          Lower=beg, Upper=end))
        

        g <- data.frame(r=r,se=se, L.int=L.int, null=m$null.value, 
                        exp.r=exp.r, lolim=lolim, hilim=hilim)
        
        imagec <- self$results$plotc
        imagec$setState(g)
        
        if(isTRUE(self$options$pll)) {
          
          plotc <- self$results$plotc
          plotc$setVisible(TRUE)
          
        }
        
        plotData <- data.frame(y=data1,x=data2)
        image <- self$results$plot
        image$setState(plotData)
        
        if(isTRUE(self$options$plt)) {
          
          plot <- self$results$plot
          plot$setVisible(TRUE)
          
        }
        
      },
      
      .plot=function(image, ggtheme, theme, ...) {
        plotData <- image$state
        line <- self$options$line
        method <- if (line == 'linear') 'lm' else 'auto'
        
        p <- ggplot2::ggplot(
          plotData, ggplot2::aes(x=x, y=y)
        )  + 
          ggplot2::geom_point(alpha=.8, size=2.5) + ggtheme +
          ggplot2::labs(
            x=self$options$depb, 
            y=self$options$depa
          )
        
        if (line != 'none') {
          p <- p + ggplot2::geom_smooth(
            method = method, se = self$options$se
          )
        }
          
        
        print(p)
        TRUE
        
      },
      .plotc=function(imagec, ...) {
        
        g <- imagec$state
        
        rtrans <- function(r_value) (0.5 * log(abs((1 + r_value)/(1 - r_value))))
        
        # to determine x axis space for plot
        x_lim <- c(g$r-3*g$se, g$r+3*g$se)   # adjust to suit x limits on plot
        if (x_lim[1] < -1) x_lim[1]=-1
        if (x_lim[2] > 1) x_lim[2]=1
        
        curve(exp(-((rtrans(g$r)-rtrans(x))/g$se)^2/2), xlim = x_lim, xlab = "r", ylab = "Likelihood")
        lines(c(g$r,g$r),c(0,1),lty=2) # add MLE as dashed line
        lines(c(g$null,g$null),c(0,exp(-((rtrans(g$r)-rtrans(g$null))/g$se)^2/2)), lty=1, col = "black") # add H prob as black line
        lines(c(g$exp.r,g$exp.r), c(0,exp(-((rtrans(g$r)-rtrans(g$exp.r))/g$se)^2/2)), lty=1, col = "blue") # add H prob as blue line
        segments(g$lolim, exp(-g$L.int), g$hilim, exp(-g$L.int), lwd = 1, col = "red")
        
        print(plot)
        TRUE
        
      }
    )
)
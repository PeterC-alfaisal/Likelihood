
propClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "propClass",
    inherit = propBase,
    private = list(
        .run = function() {

# note that .u.yaml file must use jus: '2.0' (not 3)
          
          if (is.null(self$options$var))
            return()
          
          var <- self$data[[self$options$var]]

          if ( ! is.null(self$options$counts)) {
            countsData <- self$data[[self$options$counts]]
            if (jmvcore::canBeNumeric(countsData))
              countsData <- jmvcore::toNumeric(countsData)
            else
              countsData <- suppressWarnings(as.numeric(as.character(countsData)))
            
            data <- data.frame(var=var, counts=countsData)
            counts <- xtabs(counts ~ var, data=data)
            
          } else {
            
            counts <- table(var)
          }
          
          ratio <- self$options$ratio
          if (is.null(ratio))
            expProps <- rep(1/length(counts), length(counts))
          else
            expProps <- ratio / sum(ratio)
          
          total <- sum(counts)
          
          table <- self$results$props
          
          keys <- table$rowKeys
          for (i in seq_along(keys)) {
            key <- keys[[i]]
            if (key %in% names(counts)) {
              count <- counts[[key]]
              expProp <- expProps[i]
              values <- list(
                `count[obs]`=count,
                `prop[obs]`=count / total,
                `count[exp]`=expProp * total,
                `prop[exp]`=expProp)
              table$setRow(rowKey=key, values=values)
            }
          }
          
#  likelihood code
          toler=0.0001
          len <- length(counts)
          n <- sum(counts)

          tests <- self$results$tests
          exp.p <- rep(1/len,each=len)   # null expected values
          result <- try(chisq.test(counts, p=exp.p))   # versus null
          if ( ! base::inherits(result, 'try-error')) {
            chi_n=result$statistic
            p_n=result$p.value
          } else {
              chi_n=NaN; df=''; p_n=''
          }
          result1 <- try(chisq.test(counts, p=expProps))   # versus specified expected
          if ( ! base::inherits(result1, 'try-error')) {
            chi_a=result1$statistic
            p_a=result1$p.value
          } else {
            chi_a=NaN; df=''; p_a=''
          }

          df <- (length(counts)-1)
          exp_n <- exp.p*n
          exp_ntext <- paste(round(exp_n,2),collapse=" | ")
          Sup_n <- -sum(counts*(log(counts)-log(exp_n)))
          Supc_n <- Sup_n+(df-1)/2 # corrected for df

          exp <- expProps*n
          exp_text <- paste(round(exp,2),collapse=" | ")
          Sup <- -sum(counts*(log(counts)-log(exp)))
          Supc <- Sup+(df-1)/2 # corrected for df
          
          Sup_an <- Sup - Sup_n
          Supc_an <- Sup_an-(df-1)/2
          
          lrt_n <- abs(2*Sup_n)  # likelihood ratio statistic
          LRt_p_n <- 1-pchisq(lrt_n,df)
          lrt <- abs(2*Sup)  # likelihood ratio statistic
          LRt_p <- 1-pchisq(lrt,df)
          lrt_an <- abs(2*Sup_an)  # likelihood ratio statistic
          LRt_p_an <- 1-pchisq(lrt_an,df)
          
          toogood_n <- df/2*(log(df/chi_n)) - (df - chi_n)/2
          toogood_a <- df/2*(log(df/chi_a)) - (df - chi_a)/2
          
          table <- self$results$tests
          table$setNote('Note', "Sc is S corrected for degrees of freedom using Edwards's Occam's bonus, see reference")
          table$setRow(rowNo=1, values=list(var= "Null vs observed", Values=exp_ntext, 
                                S=Sup_n, Sc=Supc_n, G=lrt_n, df=df, p=LRt_p_n))
          table$setRow(rowNo=2, values=list(var="Alt. H vs observed", Values=exp_text, 
                                S=Sup, Sc=Supc, G=lrt, df=df, p=LRt_p))
          table$setRow(rowNo=3, values=list(var="Alt. H vs Null", Values="", 
                                S=Sup_an, Sc=Supc_an, G=lrt_an, df=df, p=LRt_p_an))

          table <- self$results$ctt3
          table$setNote('Note', "A large S value indicates that variances are either larger or smaller than expected") 
          table$setRow(rowNo=1, values=list(var= "Null", Sv=toogood_n, X2=chi_n, dfv=df, 
                                            pv=p_n, pv1=1-p_n))
          table$setRow(rowNo=2, values=list(var= "Alt. H", Sv=toogood_a, X2=chi_a, dfv=df, 
                                            pv=p_a, pv1=1-p_a))
          
          if(isTRUE(self$options$varA)) { 
            
            table <- self$results$ctt3
            table$setVisible(TRUE)
            
          }
          
          if (isTRUE(self$options$bi) & isTRUE(len==2)) {
          # for binomial
          a <- counts[1]; r <- counts[2]
          p = a/n; goal = -qchisq(self$options$ciWidth/100,1)/2
          # likelihood-based % confidence interval
          x=0
          f <- function(x,a,r,p,goal) (a*log(x)+r*log(1-x)-(a*log(p)+r*log(1-p))-goal)^2
          xmin1 <- optimize(f, c(0, p), tol = toler, a, r, p, goal)
          xmin2 <- optimize(f, c(p, 1), tol = toler, a, r, p, goal)
          # likelihood interval
          goal <- -self$options$lint
          xmin1L <- optimize(f, c(0, p), tol = toler, a, r, p, goal)
          xmin2L <- optimize(f, c(p, 1), tol = toler, a, r, p, goal)
          if (p < .5) { lolim <- p - 4*sqrt(p*(1-p)/n); hilim <- p + 4*sqrt(p*(1-p)/n)
          } else {hilim <- p + 4*sqrt(p*(1-p)/n); lolim <- p - 4*sqrt(p*(1-p)/n)}
          if (lolim < 0) {lolim <- 0}
          if (hilim > 1) {hilim <- 1}
          
          lintlev <- toString(self$options$lint); conflev <- paste0(self$options$ciWidth,"%")
          
          table <- self$results$ctt2
          table$setNote('Note', "*See reference") 
          siWidthTitle <- jmvcore::format(.('Interval'))
          table$getColumn('Lower')$setSuperTitle(siWidthTitle)
          table$getColumn('Upper')$setSuperTitle(siWidthTitle)
          table$setRow(rowNo=1, values=list(Interval="Support", Level=lintlev, P = p, 
                                            Lower=xmin1L$minimum, Upper=xmin2L$minimum))
          table$setRow(rowNo=2, values=list(Interval="Likelihood-based*", Level=conflev, P = p, 
                                            Lower=xmin1$minimum, Upper=xmin2$minimum))
          table <- self$results$ctt2
            table$setVisible(TRUE)

          g <- data.frame(p=p, a=a, r=r, lolim=lolim, hilim=hilim, expprop=expProps[1],
                          xmin1L=xmin1L$minimum, xmin2L=xmin2L$minimum, goal=goal)
          imagec <- self$results$plotc
          imagec$setState(g)
          
          if(isTRUE(self$options$pll) & isTRUE(self$options$bi)) {
            
            plotc <- self$results$plotc
            plotc$setVisible(TRUE)
            
          }
            }
        },
.plotc=function(imagec, ...) {
  
  g <- imagec$state
  
  plot <- curve((x^g$a*(1-x)^g$r)/(g$p^g$a*(1-g$p)^g$r), from = 0, to = 1, xlim = c(g$lolim,g$hilim), 
                ylim = c(0,1), xlab = "Proportion", ylab = "Likelihood")
          lines(c(g$p,g$p),c(0,1),lty=2) # add MLE as dashed line
          lines(c(g$expprop,g$expprop),c(0,(g$expprop^g$a*(1-g$expprop)^g$r)/(g$p^g$a*(1-g$p)^g$r)),
                lty=1, col = "blue") # add H prob as blue line
          segments(g$xmin1L, exp(g$goal), g$xmin2L, exp(g$goal), lwd = 1, col = "red")
  
    lines(c(0.5,0.5), c(0,(0.5^g$a*(0.5)^g$r)/(g$p^g$a*(1-g$p)^g$r)), lty=1) # add Null as black line
    
    print(plot)
    TRUE
  },

        .sourcifyOption = function(option) {
          if (option$name %in% c('var', 'counts'))
            return('')
          super$.sourcifyOption(option)
        },
        .formula=function() {
          jmvcore:::composeFormula(self$options$counts, self$options$var)
        })
)

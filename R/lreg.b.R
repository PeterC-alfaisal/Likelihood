
# This file is a generated template, your changes will not be overwritten

LregClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "LregClass",
    inherit = LregBase,
    private = list(
      .run = function() {
        
        dep <- jmvcore::toNumeric(self$data[[self$options$dep]])
        pred <- jmvcore::toNumeric(self$data[[self$options$pred]])
        lint <- self$options$lint

        if (is.na(anova(lm(dep ~ pred + I(pred^2) + I(pred^3) + I(pred^4)))$Df[5]))
          jmvcore::reject(.("Too few levels or mismatch in variable lengths"), code='')
        
        m1 <- stats::anova(lm(dep ~ pred))

        tss <- sum(m1$`Sum Sq`)
        N <- (sum(unname(m1$Df))+1)
        lin_df <- unname(m1$Df[1])
        
        # support for null versus linear
        S_NL <- -0.5 * N * (log(tss) - log(unname(m1$`Sum Sq`[2])))
        
        # Akaike's correction
        k1 <- 2       # parameters for grand mean & variance
        k2 <- k1 + 1  # null parameters + parameter/df for linear 
        Ac <- k2 - k1
        S_NLc <- S_NL + Ac
        
        # examining non-linearity... alternative calculations could use $r.squared
        # comparing quadratic fit to linear
        m4 <- anova(lm(dep ~ pred + I(pred^2) + I(pred^3) + I(pred^4)))
        
        Q_ss_r <- tss - sum(m4$`Sum Sq`[1:2])

        S_Q <- -0.5 * N * (log(Q_ss_r) - log(m1$`Sum Sq`[2]))
        S_Qc <- S_Q - 1   # additional parameter for the quadratic
                                      
        
        # quadratic versus cubic
        C_ss_r <- tss - sum(m4$`Sum Sq`[1:3])
        S_C <- -0.5 * N * (log(C_ss_r) - log(Q_ss_r))
        S_Cc <- S_C - 1    # additional parameter for the cubic
        
        # cubic versus quartic
        Qt_ss_r <- tss - sum(m4$`Sum Sq`[1:4])
        S_Qt <- -0.5 * N * (log(Qt_ss_r) - log(C_ss_r))
        S_Qtc <- S_Qt - 1   # additional parameter for the quartic
        
        # for R squared & AIC
        m_l <- lm(dep ~ pred)
        m_q <- lm(dep ~ pred + I(pred^2))
        m_c <- lm(dep ~ pred + I(pred^2) + I(pred^3))
        m_qt <- lm(dep ~ pred + I(pred^2) + I(pred^3) + I(pred^4))

        # alternative code to produce Sc values:
#        m_0=lm(dep~1)
#        m_l <- lm(dep ~ pred)
#        S_NLc <- (AIC(m_l) - AIC(m_0))/2
#        m_q <- lm(dep ~ pred + I(pred^2))
#        S_Qc <- (AIC(m_l) - AIC(m_q))/2
#        m_c <- lm(dep ~ pred + I(pred^2) + I(pred^3))
#        S_Cc <- (AIC(m_q) - AIC(m_c))/2
#        m_qt <- lm(dep ~ pred + I(pred^2) + I(pred^3) + I(pred^4))
#        S_Qtc <- (AIC(m_c) - AIC(m_qt))/2
        
        table <- self$results$lreg
        table$setNote('Note', "S is calculated hierarchically from all remaining SSq, while p values are 
                      calculated using residual MSq from the highest polynomial model. 
                      Sc uses Akaike correction for parameters (Param)")
        table$setRow(rowNo=1, values=list(var= "Null vs Linear", S=S_NL, Sc=S_NLc, 
                                          Param=paste0(c(k1,k2), collapse = ', '), r2=summary(m_l)$r.squared, aic=AIC(m_l),
                                            df=paste0(c(m1$Df[1],m1$Df[2]),collapse=', '), p=m1$`Pr(>F)`[1]))
        table$setRow(rowNo=2, values=list(var= "Quadratic vs Linear", S=S_Q, Sc=S_Qc, 
                                          Param=paste0(c(k2,k2+1), collapse = ', '), r2=summary(m_q)$r.squared, aic=AIC(m_q),
                                          df=paste0(c(m4$Df[1],m4$Df[5]),collapse=', '), p=m4$`Pr(>F)`[2]))
        table$setRow(rowNo=3, values=list(var="Cubic vs Lower Orders", S=S_C, Sc=S_Cc, 
                                          Param=paste0(c(k2+1,k2+2), collapse = ', '), r2=summary(m_c)$r.squared, aic=AIC(m_c),
                                          df=paste0(c(m4$Df[2],m4$Df[5]),collapse=', '), p=m4$`Pr(>F)`[3]))
        table$setRow(rowNo=4, values=list(var="Quartic vs Lower Orders", S=S_Qt, Sc=S_Qtc, 
                                          Param=paste0(c(k2+2,k2+3), collapse = ', '), r2=summary(m_qt)$r.squared, aic=AIC(m_qt),
                                          df=paste0(c(m4$Df[3],m4$Df[5]),collapse=', '), p=m4$`Pr(>F)`[4]))

        # S and support intervals
        S_Int <- -N/2*log(1 + summary(m_qt)$coefficients[1,3]^2/summary(m_qt)$df[2])
        Int_lo <- summary(m_qt)$coefficients[1,1] - (summary(m_qt)$coefficients[1,2] * 
                                    sqrt((exp(lint*2/N) - 1) * summary(m_qt)$df[2]))
        Int_hi <- summary(m_qt)$coefficients[1,1] + (summary(m_qt)$coefficients[1,2] * 
                                    sqrt((exp(lint*2/N) - 1) * summary(m_qt)$df[2]))
        
        S_L <- -N/2*log(1 + summary(m_qt)$coefficients[2,3]^2/summary(m_qt)$df[2])
        L_lo <- summary(m_qt)$coefficients[2,1] - (summary(m_qt)$coefficients[2,2] * 
                                    sqrt((exp(lint*2/N) - 1) * summary(m_qt)$df[2]))
        L_hi <- summary(m_qt)$coefficients[2,1] + (summary(m_qt)$coefficients[2,2] * 
                                    sqrt((exp(lint*2/N) - 1) * summary(m_qt)$df[2]))
        
        Q_L <- -N/2*log(1 + summary(m_qt)$coefficients[3,3]^2/summary(m_qt)$df[2])
        Q_lo <- summary(m_qt)$coefficients[3,1] - (summary(m_qt)$coefficients[3,2] * 
                                    sqrt((exp(lint*2/N) - 1) * summary(m_qt)$df[2]))
        Q_hi <- summary(m_qt)$coefficients[3,1] + (summary(m_qt)$coefficients[3,2] * 
                                    sqrt((exp(lint*2/N) - 1) * summary(m_qt)$df[2]))
        
        C_L <- -N/2*log(1 + summary(m_qt)$coefficients[4,3]^2/summary(m_qt)$df[2])
        C_lo <- summary(m_qt)$coefficients[4,1] - (summary(m_qt)$coefficients[4,2] * 
                                    sqrt((exp(lint*2/N) - 1) * summary(m_qt)$df[2]))
        C_hi <- summary(m_qt)$coefficients[4,1] + (summary(m_qt)$coefficients[4,2] * 
                                    sqrt((exp(lint*2/N) - 1) * summary(m_qt)$df[2]))

        Qt_L <- -N/2*log(1 + summary(m_qt)$coefficients[5,3]^2/summary(m_qt)$df[2])
        Qt_lo <- summary(m_qt)$coefficients[5,1] - (summary(m_qt)$coefficients[5,2] * 
                                    sqrt((exp(lint*2/N) - 1) * summary(m_qt)$df[2]))
        Qt_hi <- summary(m_qt)$coefficients[5,1] + (summary(m_qt)$coefficients[5,2] * 
                                    sqrt((exp(lint*2/N) - 1) * summary(m_qt)$df[2]))
        
        table <- self$results$coef
        siWidthTitle <- jmvcore::format(.('S-{lint} Likelihood Interval'), lint=lint)
        table$getColumn('Lower')$setSuperTitle(siWidthTitle)
        table$getColumn('Upper')$setSuperTitle(siWidthTitle)
        table$setRow(rowNo=1, values=list(var="Intercept", Estimate=summary(m_qt)$coefficients[1,1], 
                                          SE=summary(m_qt)$coefficients[1,2], 
                                          S=S_Int, Lower=Int_lo, Upper=Int_hi, p=summary(m_qt)$coefficients[1,4]))
        table$setRow(rowNo=2, values=list(var="Linear", Estimate=summary(m_qt)$coefficients[2,1], 
                                          SE=summary(m_qt)$coefficients[2,2], 
                                          S=S_L, Lower=L_lo, Upper=L_hi, p=summary(m_qt)$coefficients[2,4]))
        table$setRow(rowNo=3, values=list(var= "Quadratic", Estimate=summary(m_qt)$coefficients[3,1], 
                                          SE=summary(m_qt)$coefficients[3,2], 
                                          S=Q_L, Lower=Q_lo, Upper=Q_hi, p=summary(m_qt)$coefficients[3,4]))
        table$setRow(rowNo=4, values=list(var="Cubic", Estimate=summary(m_qt)$coefficients[4,1], 
                                          SE=summary(m_qt)$coefficients[4,2], 
                                          S=C_L, Lower=C_lo, Upper=C_hi, p=summary(m_qt)$coefficients[4,4]))
        table$setRow(rowNo=5, values=list(var="Quartic", Estimate=summary(m_qt)$coefficients[5,1], 
                                          SE=summary(m_qt)$coefficients[5,2], 
                                          S=Qt_L, Lower=Qt_lo, Upper=Qt_hi, p=summary(m_qt)$coefficients[5,4]))
        
        plotData <- data.frame(y=dep,x=pred)
        image <- self$results$plot
        image$setState(plotData)
        
        if(isTRUE(self$options$plt)) {
          
          plot <- self$results$plot

          plot$setVisible(TRUE)
          
        }
        
      },
      
      .plot=function(image, ggtheme, theme, ...) {
        
        plotData <- image$state

        p <- ggplot2::ggplot(
          plotData, ggplot2::aes(x=x, y=y)
        )  + 
          ggplot2::geom_point(alpha=.8, size=2.5) + ggtheme +
          ggplot2::labs(
            x=self$options$pred, 
            y=self$options$dep)
        
        if(! isFALSE(self$options$lin)) {
          p <- p + ggplot2::stat_smooth(method="lm", se=FALSE, fill=NA,
                               formula=y ~ poly(x, 1, raw=TRUE),colour="black")
        }
        if(! isFALSE(self$options$quad)) {
          p <- p + ggplot2::stat_smooth(method="lm", se=FALSE, fill=NA,
                             formula=y ~ poly(x, 2, raw=TRUE),colour="red")
        }
        if(! isFALSE(self$options$cub)) {
          p <- p + ggplot2::stat_smooth(method="lm", se=FALSE, fill=NA,
                                        formula=y ~ poly(x, 3, raw=TRUE),colour="blue")
        }
        if(! isFALSE(self$options$quart)) {
          p <- p + ggplot2::stat_smooth(method="lm", se=FALSE, fill=NA,
                                        formula=y ~ poly(x, 4, raw=TRUE),colour="green")
        }

        print(p)
        TRUE
        
      }
    )
)
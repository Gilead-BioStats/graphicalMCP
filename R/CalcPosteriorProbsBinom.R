
#' @name CalcPosteriorProbsBinom
#' @title CalcPosteriorProbsBinom
#' @description { \ifelse{html}{ This function assumes \eqn{\pi}\out{<sub>E</sub>} ~ Beta( dParam1E, dParam2E ) and
#' \eqn{\pi}\out{<sub>S</sub>}~ Beta( dParam1S, dParam2S ) and computes
#' Pr( \eqn{\pi}\out{<sub>E</sub>} - \eqn{\pi}\out{<sub>S</sub> } > dDelta1 ) and
#' Pr( \eqn{\pi}\out{<sub>E</sub>}-\eqn{\pi}\out{<sub>S</sub>} > dDelta2 ) }{This function assumes \eqn{\pi_E} ~ Beta( dParam1E, dParam2E )
#' and  \eqn{\pi_S}~ Beta( dParam1S, dParam2S ) and computes Pr( \eqn{\pi_E - \piS} > dDelta1 ) and and Pr( \eqn{\pi_E-\pi_S} > dDelta2 )}
#' }
#' @export
CalcPosteriorProbsBinom <- function( dParam1S, dParam2S, dParam1E, dParam2E, dDelta1, dDelta2)
{
    # Calculate Pr( p_E - p_S > dDelta1| data) = Pr( p_E > p_S+ dDelta1 )
    dPostProb1 <- ProbX1GrX2PlusDelta( dParam1E, dParam2E, dParam1S, dParam2S, dDelta1)

    #Calculate Pr( p_E - p_S > dDelta2 | data ) = Pr( p_E > p_S + dDelta2 | data )
    dPostProb2 <- ProbX1GrX2PlusDelta( dParam1E, dParam2E, dParam1S, dParam2S, dDelta2)

    return( list( dPPGrtDelta1=dPostProb1, dPPGrtDelta2 = dPostProb2) )
}

# Note: the function above has @export which allows users to call it directly where the following are not easily called by users since
# they do not contian the @export.

# Function to calculate the posterior probabilities of interest, for simplicity this function uses the Beta distribution and
# is not general for any type of distribution
ProbX1GrX2PlusDelta <- function(dA1,dB1,dA2,dB2, dDelta)
{
    ## The function calculates Pr( X1 > X2 + dDelta)
    ## Note: Pr( X1 > X2 + dDelta) = Pr( X2 < X1 - dDelta ) which is what is calculated so we can use pbeta
    #Note: Pr( X1 - X2 < dDelta ) = Pr( X2 > X1 - dDelta)

    # compute the limits based on the parameters
    dMin <- min( qbeta( 0.00001, dA1, dB1 ), qbeta( 0.00001, dA2, dB2 ))
    dMax <- max( qbeta( 1.0 - 0.00001, dA1, dB1 ), qbeta( 1.0 - 0.00001, dA2, dB2 ))
    res <- integrate(fBetaIneqCalc, dMin, dMax, dA1 = dA1, dB1 = dB1, dA2 = dA2, dB2 = dB2, dDelta)
    res$value
}

#Helper functions
fBetaIneqCalc <- function(x, dA1, dB1, dA2, dB2, dDelta ){dbeta( x, dA1, dB1 )* (pbeta(x-dDelta,dA2,dB2)) }




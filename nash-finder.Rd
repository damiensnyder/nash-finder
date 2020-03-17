\name{Nash finder}

\alias{Nash finder}

\title{
  Find a Nash Equilibrium
}

\description{
  This function finds a mixed strategy Nash equilibrium for a numeric matrix representing the
  payouts of a symmetrical two-player game.
}

\usage{
find_equilibrium(payouts)
}

\arguments{
  \item{payouts}{
    a square numeric matrix containing the payouts of the game, such that payouts[i, j] represents
    the payout for a player who chooses strategy i against strategy j.
  }
}

\details{
  Returns only one Nash equilibrium solution, even if more equilibria exist.
}

\value{
  Returns a vector representing the proportion of the time each strategy is utilized in the
  equilibrium solution found. The indices of each strategy in the vector returned are the same as
  the indices of each strategy in the payout matrix.
}

\author{
  Damien Snyder
}

\examples{
payouts <- matrix(c(1, 0, 3, 4, 1, 0, 0, 3, 1), 3, 3)
equilibrium <- find_equilibrium(payouts)

## a prisoner's dilemma
payouts2 <- matrix(c(3, 6, 0, 1), 2, 2)
equilibrium2 <- find_equilibrium(payouts2)
}

\keyword{"array"}
\keyword{"algebra"}
\keyword{"optimize"}

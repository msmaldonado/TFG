\name{ChenTrapezoidal}
\alias{ChenTrapezoidal}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Funcion que calcula el indice del metodo de Chen para numeros difusos trapezoidales .}
\description{
Funcion que calcula el indice del metodo de Chen para numeros difusos trapezoidales .
}
\usage{
ChenTrapezoidal(a,b,c,d,xmin,xmax)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{a}{Extremo inferior del soporte }
\item{b}{Extremo inferior del nucleo }
\item{c}{Extremo superior del nucleo }
\item{d}{Extremo superior del soporte }
\item{xmin}{Minimo de los extremos inferiores del soporte del conjunto }
\item{xmax}{Maximo de los extremos inferiores del soporte del conjunto  }
}
\details{
Los valores a,b,c,d tienen que estar ordenados en forma creciente y estar en el intervalor del xmin y xmax
\value{
Devuelve el valor del indice
}
\references{}
\author{msmaldonado}
\note{}
\seealso{}
\examples{
ChenTrapezoidal(0.4,0.5,0.5,1,0.4,1);
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{NumerosDifusosTrapezoidales,Chen}



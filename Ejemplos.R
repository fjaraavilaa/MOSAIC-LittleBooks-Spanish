require(mosaic)
require(manipulate)
#####################Damas aleatorias de Fisher
require('doParallel')
cores <- detectCores()-1
cl <- makeCluster(cores)
registerDoParallel(cl)
options('mosaic:parallelMessage'=FALSE)
damas.aleatorias<- do(100000, parallel=TRUE) * rflip(10)
histogram(damas.aleatorias$heads, width=1, xlab='Cantidad de muestras de té bien adivinadas')
#####################Ejemplo generación de gráficos interactivos
mNorm <- function( mean=0, sd=1 ){
  lo <- mean - 5*sd
  hi <- mean + 5*sd
  manipulate(
    xpnorm( c(A,B), mean, sd, verbose=FALSE, invisible=TRUE ),
    A = slider(lo, hi, initial=mean-sd),
    B = slider(lo, hi, initial=mean+sd)
  )
}
mNorm( mean=100, sd=10)
#####################Ejemplo 
mplot(HELPrct)
#####################Ejemplo 
require(RMySQL)
myconnection <- dbConnect(MySQL(), user="genome", host='genome-mysql.soe.ucsc.edu')
dbnameselected <- dbConnect(MySQL(), user="genome", db="mm9", host="genome-mysql.soe.ucsc.edu.")
dbListTables(dbnameselected)
data <- dbSendQuery(dbnameselected, "select * from acembly")
data <- fetch(data, n=-1)
head(data, 3)
#####################Ejemplo sobre el problema de lazyeval
mod <- lm( length ~ width + sex, data = KidsFeet )
do(1) * mod
do(1) * lm( length ~ width + sex, data = KidsFeet )





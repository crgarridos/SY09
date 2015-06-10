ceuc.app <- function(Xapp, zapp){
  apply(Xapp, 2, by, zapp, mean)
}
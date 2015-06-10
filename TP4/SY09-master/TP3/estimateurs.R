estimateurAlpha <- function(data){
  sum(apply(data, 1,
  function(row) {
    if(row[3] == 1 && row[1] > -0.5) 
      { return(1) } 
    return(0) }
  )) / sum(data[,3] == 1)
}

estimateurBeta <- function(data){
  sum(apply(data, 1,
            function(row) {
              if(row[3] == 2 && row[1] < -0.5) 
              { return(1) } 
              return(0) }
  )) / sum(data[,3] == 1)
}
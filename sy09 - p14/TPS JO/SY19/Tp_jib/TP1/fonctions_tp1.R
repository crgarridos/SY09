prinerexp <- function(X,y) {
	
	sum = 0;
	result = seq(0,0, length=length(X));
	for(i in 1:length(X)){
		sum = sum + X[i];
		result[i] = sum/y*100;
		}
		
		return(result)
	}
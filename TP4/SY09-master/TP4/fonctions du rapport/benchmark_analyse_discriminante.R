
benchmark_analyse_discriminante = function() {

	benchmarks = array(0,dim=c(20,3,3))
	dimnames(benchmarks)[[3]] = c("adq.app","adl.app","nba.app")
	dimnames(benchmarks)[[2]] = c("Syhth1-1000","Syhth2-1000","Syhth2-1000")

	######################## benchmarks
	for(i in 1:3) {
		filename = paste("Synth",i,"-1000.txt", sep="")
		X = read.table(filename)[,1:2]
		z = read.table(filename)[,3]

		for(j in 1:20) {
			separation = separ1(X,z)
			Xapp = separation$Xapp
			Xtst = separation$Xtst
			zapp = separation$zapp
			ztst = separation$ztst
			
			for(f in c("adq.app","adl.app","nba.app")) {
				fct = get(f)
				parametres = fct(Xapp,zapp)
				classement = ad.val(parametres,Xtst)
				benchmarks[j,i,f] = sum(classement[,3]==ztst) / length(ztst)
			}
			
		}
	}
	
	######################## means
	resume = list()
	for(f in c("adq.app","adl.app","nba.app")) {
			resume[[f]]$mean_by_file = apply(benchmarks[1:20,,f],2,mean)
			resume[[f]]$mean = sum(resume[[f]]$mean_by_file) / 3
	}
	
	
	
	return(list(details=benchmarks,resume=resume))
	
}
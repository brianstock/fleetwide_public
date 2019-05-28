# Brian Stock
# forestFloor feature contribution plots

ff_plot_factor <- function(dat,mod.rf,ff,x,covar){
	labs <- levels(dat[,x])
	n.lev <- length(labs)
	the.col <- match(x, attributes(ff$FCmatrix)$dimnames[[2]])
	preds <- ff$FCmatrix[,the.col]

	pred.df <- data.frame(matrix(NA, nrow = dim(dat)[1], ncol = n.lev))
	names(pred.df) <- labs
	ind <- nlev <- list()
	for(l in 1:n.lev){
		ind[[l]] <- which(dat[,x] == labs[l])
		nlev[[l]] <- table(dat[,x])[l]
		pred.df[1:nlev[[l]],l] <- preds[ind[[l]]]
	}
	pred.df.plot <- reshape2::melt(pred.df)

	g <- ggplot(pred.df.plot, aes(x = variable, y = value)) +
	        geom_boxplot(fill="grey80", outlier.shape=NA, coef=0) +
	        theme_classic() +
	        ylab("") + xlab("") + theme(axis.text.x = element_blank(), axis.text.y = element_blank())
	getmed <- ggplot_build(g)$data[[1]]       	       
	miny <- getmed$lower - 1
	maxy <- getmed$upper + 1
	# g <- g + geom_segment(data=getmed, aes(x=xmin, xend=xmax, y=middle, yend=middle), colour="#3366FF", size=1.5) +
	# 	coord_cartesian(ylim = c(miny,maxy))
	g <- g + geom_segment(data=getmed, aes(x=xmin, xend=xmax, y=middle, yend=middle), colour="#3366FF", size=1.5) +
		coord_cartesian(ylim = c(miny,maxy))
	return(g)
}
# ff_plot_factor(dat,mod.rf,ff,x="depth_interval",covar)

ff_plot_cont <- function(dat,mod.rf,ff,thecov,covar){
	the.col <- match(thecov, attributes(ff$FCmatrix)$dimnames[[2]])
	fc <- ff$FCmatrix[,the.col]
	x <- dat[,thecov]
	if(class(x)=="numeric"){
		n.bin = 50
		xbin <- seq(from=min(x),to=max(x),length.out=n.bin+1)
		fc.low <- fc.high <- fc.med <- x.med <- bin.n <- rep(NA,n.bin)
		for(bin in 1:n.bin){
			b <- which(x > xbin[bin] & x < xbin[bin+1])
			fc.low[bin] <- quantile(fc[b],.025)
			fc.high[bin] <- quantile(fc[b],.975)
			fc.med[bin] <- median(fc[b])
			x.med[bin] <- median(x[b])
			bin.n[bin] <- length(x[b])
			if(bin.n[bin] < 5) fc.low[bin] <- fc.high[bin] <- fc.med[bin] <- x.med[bin] <- NA
		}		
	}
	if(class(x)=="integer"){
		xbin <- unique(x)
		n.bin <- length(xbin)
		fc.low <- fc.high <- fc.med <- x.med <- bin.n <- rep(NA,n.bin)
		for(bin in 1:n.bin){
			b <- which(x == xbin[bin])
			fc.low[bin] <- quantile(fc[b],.025)
			fc.high[bin] <- quantile(fc[b],.975)
			fc.med[bin] <- median(fc[b])
			x.med[bin] <- xbin[bin]
			bin.n[bin] <- length(x[b])
			if(bin.n[bin] < 5) fc.low[bin] <- fc.high[bin] <- fc.med[bin] <- x.med[bin] <- NA
		}			
	}
	minX <- min(x.med,na.rm=T)
	maxX <- max(x.med,na.rm=T)
	df <- data.frame(med=fc.med,low=fc.low,high=fc.high,x=xbin[1:n.bin])
	# minY <- max(min(df$low,na.rm=T), -6)
	# maxY <- min(max(df$high,na.rm=T), 6)

	# g <- ggplot(df, aes(x=x,y=med)) +
	# 		geom_ribbon(aes(ymin=low, ymax=high), fill="grey80",alpha=0.35) +
	# 		geom_line(aes(x=x, y=med),size=1.5, colour="#3366FF") +			
	#         coord_cartesian(xlim = c(minX,maxX)) +
	#         coord_cartesian(ylim = c(minY,maxY)) +	        
	#         theme_classic() +
	#         xlab(thecov) +
	#         ylab("Feature contribution")

	minY <- min(df$med, na.rm=T) - 0.1*diff(range(df$med,na.rm=T))
	maxY <- max(df$med, na.rm=T) + 0.1*diff(range(df$med,na.rm=T))
	g <- ggplot(df, aes(x=x,y=med)) +
			geom_ribbon(aes(ymin=low, ymax=high), fill="grey80",alpha=0.35) +
			geom_line(aes(x=x, y=med),size=1.5, colour="#3366FF") +			
	        coord_cartesian(xlim = c(minX,maxX)) +
	        coord_cartesian(ylim = c(minY,maxY)) +	        
	        theme_classic() +
	        ylab("") + xlab("") + theme(axis.text.x = element_blank(), axis.text.y = element_blank())
	return(g)
}
# ff_plot_cont(dat,mod.rf,ff,thecov="year",covar)


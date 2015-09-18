


col_hex<-read.table("hairColour/2015-09-18_possible_hair_colours.txt",sep="\t",stringsAsFactors=F,header=T,colClasses=c("character","numeric"))
col_rgb<-col2rgb(paste("#",toupper(col_hex[,"Color.Code"]),sep=""), alpha = FALSE)

col_rgb<-col_rgb[,order(apply(col_rgb,2,sum))]
col_hsv<-rgb2hsv(r=col_rgb["red",],g=col_rgb["green",],b=col_rgb["blue",])

plot()

colours<-rgb(col_rgb["red",],col_rgb["green",],col_rgb["blue",],maxColorValue=255)

plot(x=col_hsv["v",],y=col_hsv["s",],col=colours,pch=19,cex=2)
plot(x=col_hsv["h",],y=col_hsv["s",],col=colours,pch=19,cex=2)


#so human hair colours pretty much follows this pattern
v 1 - 0
s 0 - 1
h 0.1 - 0.1
#easy!


# let's generate our own scale

col<-data.frame(
	h = rep(0.1, 100),
	s=seq(0,1,length.out=100),
	v=seq(1,0,length.out=100)
)

col[,"col"]<-hsv(h=col[,"h"],s=col[,"s"],v=col[,"v"])

plot(x=col[,"v"],y=col[,"s"],col=colours,pch=19,cex=2)
#smooth!

#so this is the frame
brown<-data.frame(
	h = rep(0.1, 100),
	s=seq(0,1,length.out=100),
	v=seq(1,0,length.out=100)
)
brown[,"col"]<-hsv(h=brown[,"h"],s=brown[,"s"],v=brown[,"v"])


#so this is the frame
red<-data.frame(
	h = rep(0.0, 100),
	s=seq(0,1,length.out=100),
	v=seq(1,0,length.out=100)
)
red[,"col"]<-hsv(h=red[,"h"],s=red[,"s"],v=red[,"v"])


plot(NULL,xlim=c(0,1),ylim=c(0,1),xlab="blondeness",ylab="redheadness")

for(blondeness in seq(0,1,0.01)){
	for(redheadness in seq(0,1,0.01)){	
		col<-hsv(
			h=0.1 - (redheadness/10),
			s=min(c(1,1-blondeness + (redheadness/2))),
			# s=1-blondeness,

			v=blondeness 
		)
		points(x=blondeness,y=redheadness,col=col,pch=19,cex=2)
		
	}
}


points(x=seq(1,0,length.out=nrow(brown)),y=rep(0,nrow(brown)),col=brown[,"col"],pch=19,cex=2)

points(y=seq(1,0,length.out=nrow(red)),x=rep(0,nrow(red)),col=red[,"col"],pch=19,cex=2)






col_hex<-read.table("hairColour/2015-09-18_possible_hair_colours.txt",sep="\t",stringsAsFactors=F,header=T,colClasses=c("character","numeric"))
col_rgb<-col2rgb(paste("#",toupper(col_hex[,"Color.Code"]),sep=""), alpha = FALSE)

col_rgb<-col_rgb[,order(apply(col_rgb,2,sum))]
col_hsv<-rgb2hsv(r=col_rgb["red",],g=col_rgb["green",],b=col_rgb["blue",])

plot()














#This one actually works quite well on it's own


plot(NULL,xlim=c(0,1),ylim=c(0,1),xlab="blondeness",ylab="redheadness")

for(blondeness in seq(0,1,0.01)){
	for(redheadness in seq(0,1,0.01)){	
		col<-hsv(
			h=0.1 - (redheadness/10),
			s=min(c(1,1-blondeness + (redheadness/2))),
			# s=1-blondeness,
			
			v=blondeness 
		)
		points(x=blondeness,y=redheadness,col=col,pch=19,cex=2)
		
	}
}


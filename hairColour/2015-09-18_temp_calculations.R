


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
col<-data.frame(
	h = rep(0.1, 100),
	s=seq(0,1,length.out=100),
	v=seq(1,0,length.out=100)
)






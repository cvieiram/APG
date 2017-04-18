n     <- 100                    # length of vector                # desired correlation = cos(angle)
x1    <- rnorm(n, 5, 4)        # fixed given data
x2    <- rnorm(n, 0.5, 1)      # new random data
cor(x1,x1+x2+5)

plot(x1,x1+x2)

plot(x1,x3)
p3<- ggplot(data = data.frame((cbind(x1,x1+x2+5))), aes(x=x1, y=V2)) + geom_point()+
  scale_fill_gradient(low = "white",high = "springgreen4")+
  geom_abline(slope = 1, size = 1)+
  labs(x='Method One',y='Method Two')+
  scale_y_continuous(limits = c(0, 10), breaks=c(1,3,7,10))+
  scale_x_continuous(limits = c(0, 10), breaks=c(1,3,7,10))


p1<- ggplot(data = data.frame((cbind(x1,x1+x2))), aes(x=x1, y=V2)) + geom_point()+
  scale_fill_gradient(low = "white",high = "springgreen4")+
  geom_abline(slope = 1, size = 1)+
  labs(x='Method One',y='Method Two')+
  scale_y_continuous(limits = c(0, 10), breaks=c(1,3,7,10))+
  scale_x_continuous(limits = c(0, 10), breaks=c(1,3,7,10))

negative<-data.frame((cbind(x1,x1+x2)))
negative<- negative[order(x1),]
negative$V2N <-negative[order(-negative$V2),]$V2
negative$V2N <- negative$V2N -1
p2<-ggplot(data = negative, aes(x=x1, y=V2N)) + geom_point()+
  scale_fill_gradient(low = "white",high = "springgreen4")+
  geom_abline(intercept=10, slope = -1, size = 1)+
  labs(x='Method One',y='Method Two')+
  scale_y_continuous(limits = c(0, 10), breaks=c(1,3,7,10))+
  scale_x_continuous(limits = c(0, 10), breaks=c(1,3,7,10))

png(file="../images/model1.png",width=600,height=500)
p4
dev.off()

mtcars[order(mpg, -cyl),] 


df<- data.frame(x=c(2.5, 2.5, 7.5, 7.5), y=c(2.5,7.5,2.5,7.5))
df<- data.frame(x=c(0,10), y=c(0,10))
df$weights<-c(30,30)
df$weights <- as.factor(df$weights)

p4 <-ggplot(data = df, aes(x=x, y=y)) + 
  geom_area(aes(fill=weights)) + geom_hline(yintercept=0)+geom_abline(slope = 1, size = 1)+
  labs(x='Method One',y='Method Two')

df<- data.frame(x=c(0,10), y0=c(0,10),y1=c(0,0), y2=c(10,10))
ggplot(data = df,aes(x,y0))+
  geom_ribbon(aes(x=x, ymax=y1, ymin=y0), fill="gray", alpha=.8) +
  geom_ribbon(aes(x=x, ymax=y0, ymin=y2), fill="gray", alpha=.2) +
  theme_bw()


  geom_line(aes(y = y1), colour = 'red') +
  geom_line(aes(y = y2), colour = 'blue')+
  geom_line()
            
            + scale_fill_gradient(low = "white",high = "steelblue")+ #high = "springgreen4")+#
  labs(x='Method One',y='Method Two')+geom_abline(slope = 1, size = 1)+
  annotate("text", x=df$x, y=df$y, label= df$weights, size=10, color="gray28")


a<- c(6,7,9,7,8,6,7,8,6,8,9,7,10,9,8,9,7,9,7,9)
b<- c(6,7,9,7,8,6,7,8,6,8,9,7,10,9,8,9,7,9,7,9)
p1<- addWeights(a, b, "Method One", "Method Two")
b2<- c(b[1:13],b[14:20]-5)
p2<- addWeights(a, b2, "Method One", "Method Two")
a2<- c(a[1:8]-2, a[9:13],a[14:20]-5)
p3<- addWeights(a2, b2, "Method One", "Method Two")
p4<- addWeights(a2, b, "Method One", "Method Two")
png(file="../images/sampleQuadrants.png",width=1000,height=800)
multiplot(p1,p2,p4, p3,  cols=2)
dev.off()



multiplot(p1,p2,p3,  cols=3)


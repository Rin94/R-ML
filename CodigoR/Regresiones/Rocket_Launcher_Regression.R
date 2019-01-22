## read challenger.csv
launch<-read.csv(file.choose())

str(launch)
summary(launch)
lengths(launch)

### MADE THE COV, applying the formula 
b<-cov(launch$temperature, launch$distress_ct)/var(launch$temperature)
b
## made the intercept using the formula
a<-mean(launch$distress_ct)-b*mean(launch$temperature)
a

##correlation
c_orr<-cov(launch$temperature, launch$distress_ct)/(sd(launch$temperature)*sd(launch$distress_ct))
c_orr
cor(launch$temperature,launch$distress_ct)

## linear regression applying the formula

#solve() takes the inverse of a matrix
#t() is used to traspose a matrix
#%*% multiplies two matrices

reg<-function(y,x){
  x<-as.matrix(x)
  
  x<-cbind(Intercept=1,x)#agregamos columna a x y la llenamos con unos
  b<-solve(t(x)%*%x)%*%t(x)%*%y
  colnames(b)<-"estimate"
  print(b)
}
str(launch)
reg(y=launch$distress_ct, x=launch[3])

#x<-claunch[-2]
x1<-c(launch[1])
x2<-c(launch[3])
x3<-c(launch[4])
x4<-c(launch[5])
rocket_ln<-lm(launch$distress_ct~launch$temperature,data = launch)
rocket_ln

rocket_ln<-lm(launch$distress_ct~.,data = launch)
rocket_ln

summary(rocket_ln)

launch_1<-launch[-2]
launch_1<-launch_1[-1]
launch_1

reg(launch$distress_ct,launch_1[1:3])

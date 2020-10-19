resampling=function(g){
n=100
rs=rcauchy(n,location=0,scale=1)
theta_est=median(rs)
#jacknife
resamp=function(z){
rsamp=rs[-z]
rsamp
}
k=c(1:n)
newsamp_jack=lapply(k,resamp)
Tcurl=array(NA,n)
for(i in 1:n)Tcurl[i]=(n*theta_est)-((n-1)*median(newsamp_jack[[i]]))
Tjack=sum(Tcurl)/n
jack_bias=theta_est-Tjack
var_Tjack=(sum((Tcurl-mean(Tcurl))^2))/(n*(n-1))
#bootstrap
s=function(z)sample(rs,n,replace = TRUE,prob=NULL)
B=500
l=c(1:B)
newsamp_boot=lapply(l,s)
Tboot=array(NA,B)
for(i in 1:B)Tboot[i]=median(newsamp_boot[[i]])
boot_bias=(sum(Tboot)/B)-theta_est
var_Tboot=((sum(Tboot-mean(Tboot)))^2)/B
##
list(jack_bias,boot_bias,var_Tjack,var_Tboot)
}
BB=100;p=c(1:BB)
result=lapply(p,resampling)
vj=vb=array(NA,BB)
for(i in 1:BB){
vj[i]=result[[i]][3]
vb[i]=result[[i]][4]
}
######ploting
plot(vj,vb)




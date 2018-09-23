# Part I
V1<-rnorm(100, 0, 1)
V1.5<-rnorm(100, 0, 1.5)

counter=0
for (i in 1:100){
  if (abs(V1[i])<abs(V1.5[i])){
    counter=counter+1
  }
}


V1Squared<-V1^2
V1.5Squared<-V1.5^2
sum(V1Squared)
sum(V1.5Squared)

sqrt(sum((V1-mean(V1))^2)/99)
sqrt(sum((V1.5-mean(V1.5))^2)/99)

V1Log<-log(abs(V1))
V1.5Log<-log(abs(V1.5))
count=0
for (i in 1:100){
  if (V1Log[i]<V1.5Log[i]){
    count = count + 1
  }
}


# Part II
V1<-rcauchy(100, location=0, scale=1)
V1.5<-rcauchy(100, location=0, scale=sqrt(1.5))

counter=0
for (i in 1:100){
  if (abs(V1[i])<abs(V1.5[i])){
    counter=counter+1
  }
}


V1Squared<-V1^2
V1.5Squared<-V1.5^2
sum(V1Squared)
sum(V1.5Squared)

sqrt(sum((V1-mean(V1))^2)/99)
sqrt(sum((V1.5-mean(V1.5))^2)/99)

V1Log<-log(abs(V1))
V1.5Log<-log(abs(V1.5))
count=0
for (i in 1:100){
  if (V1Log[i]<V1.5Log[i]){
    count = count + 1
  }
}

V1<-c(9.4, 10.3, 7.8, 8.9, 5.6, 4.1, 12.1, 14.7, 6.9, 8.7, 4.2, 7.1, 8.8, 11.3, 7.7, 5.2, 6.4, 7.8)
sd(V1)

mean(9.4, 7.8, 5.6, 12.1, 6.9, 4.2, 8.8, 7.7, 6.4)


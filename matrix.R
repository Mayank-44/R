m1<- matrix(c(2:16),nrow=3,byrow="TRUE")
print(m1)

m1<- matrix(c(2:16),nrow=3,byrow="FALSE")
print(m1)

m1<- matrix(c(2:16),ncol=3,byrow="TRUE")
print(m1)

m1<- matrix(c(2:16),ncol=5,byrow="FALSE")
print(m1)

rowNames<-c("row1","row2","row3")
colNames<-c("col","col2","col3")

m2<-matrix(c(3:11),3,byrow="TRUE",dimnames=list(rowNames,colNames))
print(m2)

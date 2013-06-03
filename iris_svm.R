library(kernlab)
library(ggplot2)

# Check iris dataset
head(iris)
nrow(iris)
summary(iris)

# default parameter
classifier.default <- ksvm(
    Species ~., # Define Label
    data=iris,
    type="C-svc",
    kernel="rbfdot",
    cross=nrow(iris) # LOOCV
    )
acc.default <- 1 - cross(classifier.default)
print(acc.default)

# Get SVM accuracy by args
# (Cost param, Sigma param)
svm.getacc <- function(c,sig){
    classifier <- ksvm(
        Species ~., # Define Label
        data=iris,
        type="C-svc",
        kernel="rbfdot",
        C = c,
        kpar=list(sigma=sig), 
        cross=nrow(iris) # LOOCV
        )
    acc <- 1 - cross(classifier)
    ret <- c(c,sig,acc)
    cat(ret,"\n")
    return(ret)
}

# Grid search
svm.gridsearch <- function(seq.c = -5:15,seq.sigma = -15:3){
    vec <- numeric(0)
    # Try All C and sigma combination
    for(c in 2^seq.c){
        for(sigma in 2^seq.sigma){
            vec <- c(vec, svm.getacc(c,sigma))
        }
    }
    m <- t(matrix(vec,nrow=3))
    colnames(m) <- c("c","sigma","acc")
    # Remove error in acc diff
    m[m[,3] < 10^-5,3] <- 0
    return(data.frame(m))
}

grid <- svm.gridsearch()
summary(grid)
write.table(grid,file="girdsearch.csv",row.names=F)

# acc = 0.5 ~ 1
g = ggplot(grid,aes_string(x="c",y="sigma",z="acc")) + geom_tile(aes(fill=acc)) + scale_x_continuous(trans="log2") + scale_y_continuous(trans="log2") + scale_fill_continuous(limits=c(0.5, 1), breaks=seq(0,1,by=0.1))
print(g)
browser()

# acc = 0.9 ~ 1
g = ggplot(grid,aes_string(x="c",y="sigma",z="acc")) + geom_tile(aes(fill=acc)) + scale_x_continuous(trans="log2") + scale_y_continuous(trans="log2") + scale_fill_continuous(limits=c(0.9, 1), breaks=seq(0,1,by=0.01))
print(g)
browser()

# acc = 0.95 ~ 1
g = ggplot(grid,aes_string(x="c",y="sigma",z="acc")) + geom_tile(aes(fill=acc)) + scale_x_continuous(trans="log2") + scale_y_continuous(trans="log2") + scale_fill_continuous(limits=c(0.95, 1), breaks=seq(0,1,by=0.01))
print(g)

grid.subset = subset(grid,grid$acc == max(grid$acc))
print(grid.subset)
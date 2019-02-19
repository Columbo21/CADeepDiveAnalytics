# IF THEN ELSE
#********************

dat <- 3

if(is.numeric(dat)){
    dat <- dat + 5
    cat('Die Zahl lautet', dat)
}else{
    cat(dat, 'ist keine Zahl!')
}


# FOR LOOPS
#********************

names <- c('Harry', 'Bert', 'Murk')

for(n in names){
    cat(n, 'ist ein wunderschöner Name.\n')
}

for(n in 1:length(names)){
    cat('Der', n, '. Name lautet', names[[n]], '\n')
}

for(n in seq_along(names)){
    
}

# FUNKTIONEN
#********************

myfunc <- function(){
    cat('Diese Funktion ist sinnlos.')
}

cylinder_volume <- function(r, h){
    vol <- pi * r^2 * h
    return(vol)
}

plot(cylinder_volume(1:10, 10))

# APPLY
#********************

df <- data.frame(a=rnorm(10), b = rnorm(10), c = rnorm(10))

mean(df[,1])
mean(df[,2])
mean(df[,3])

for(k in 1:3){
    print(mean(df[,k]))
}

apply(df, 1, mean)


# Aufgabe: Schreibt eine Funktion die die Fibonacci Reihe erzeugt (15 min)

fibonacci <- function(x){
    
    ans <- rep(1,x)
    if(x > 2){
        for(k in 3:x){
            ans[k] <- ans[k-2] + ans[k-1]
        }
    }
    return(ans)
}

fibonacci(5)

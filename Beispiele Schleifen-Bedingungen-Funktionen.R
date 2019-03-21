dat<-3
if(dat<3){
  cat('JA')
}else{
  cat('nein')
}
ifelse(dat<4,'ja','nein')


# Loops

name <- c("Dominik", "Hans", "Jürgen")
for (n in name){
  cat(n, "ist ein Name")
}

for(i in seq(name)){
  cat("der", i, "-te name lautet", name[i], "! ")
}

myfunc <- function(){
  cat("Diese Funktion ist sinnlos")
}

myfunc()

cylinder_volume <- function (r, h = 10){ #=10 bedeutet, dass wenn nichts übergeben wird, er den Wert 10 verwendet
  vol <- pi * r^2 * h
  return(vol)
}
cylinder_volume(5,)
cylinder_volume(5,12)
cylinder_volume(h=3,5)

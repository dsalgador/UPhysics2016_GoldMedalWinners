#setwd("C:/Users/Daniel/Documents")
dades <- read.csv("tabuladata.csv",header=TRUE, na.strings=c("", "NA"))
names(dades) <- c("Species", "Common name", "m ", "FMR", "Taxon", "Habitat", "Diet", "Reference")

dades <- na.omit(dades)


files <- length(row.names(dades))
row.names(dades) <- as.character(seq(1,files,1))
#print(head(dades))

mammals <- dades[1:79 ,3:4];
birds <- dades[80:145 , 3:4];
reptiles <- dades[146: 200,];
#row.names(mammals) <- as.character(seq(1,files,1))
#row.names(birds) <- as.character(seq(1,files,1))
#row.names(reptiles) <- as.character(seq(1,files,1))
#hist(mammals$"m")

mammals_m <- as.matrix(mammals$'m')
mammals_m <- as.numeric(gsub(",", "", mammals_m))

mammals_FMR <- as.matrix(mammals$'FMR')
mammals_FMR <- as.numeric(gsub(",", "", mammals_FMR))

plot(log10(mammals_m),log10(mammals_FMR))


Xx <- log10(mammals_m)
Yx <- log10(mammals_FMR)
regx <- lm(Yx ~ Xx)
#plot(regx)
abline(coef(regx)[1:2], lwd = 2, col = "cyan")

a <- 10^(coef(regx)[1])

names(summary(regx))
x <- summary(regx)

f <- x$fstatistic
pf(f[1], f[2], f[3], lower=FALSE) 



species <- function(data, rows, color){
      #layout(matrix(1:1))
      specie <- data[rows , 3:4];
      specie_m <- as.matrix(specie $'m')
      specie_m <- as.numeric(gsub(",", "", specie_m))
      
      specie_FMR <- as.matrix(specie $'FMR')
      specie_FMR <- as.numeric(gsub(",", "", specie_FMR))
      
      plot(log10(specie_m),log10(specie_FMR),
           #xaxt = 'n', yaxt='n',
           xlab = expression(log[10] ~ "(Mass (g)) "), ylab = expression(log[10] ~"(FMR (kJ/day))" ))
      

      Xx <- log10(specie_m)
      Yx <- log10(specie_FMR)
      regx <- lm(Yx ~ Xx)
      adjr <- summary(regx)$adj.r.squared
      
      #plot(regx)
      abline(coef(regx)[1:2], lwd = 2, col = color)
      coeffs <- c(10^coef(regx)[1], coef(regx)[2], adjr)
      
      #mtext(r^2 ~ " = " ~  round(adjr, 3))
      #mtext(round(adjr, 3)) 
     # mtext(paste("r = "[2], as(p, "character")))
      
      #layout(matrix(1:4,2,2))
      #plot(regx)
      legend("topleft", bty="n", legend=paste("R2 =", 
                                                        format(summary(regx)$adj.r.squared, digits=3)))
      
      #print(summary(regx))
      return(coeffs)
      
    
}


#Els dos plots de mammals i reptils amb els fits
layout(matrix(1:2,1))
coeff_mammals <- species(dades, c(1:79), "red" )
title("FMR v.s mass for Mammals")

a_m <- coeff_mammals[1]
b_m <- coeff_mammals[2]
#mtext(round(a_m,3) ~ m^2 )

coeff_reptiles <- species(dades, c(146:200), "darkgreen" )
title("FMR v.s mass for Reptiles")
a_r <- coeff_reptiles[1]
b_r <- coeff_reptiles[2]
#mtext("FMR   ~  + b log M")



#coeff_birds <- species(dades, c(80:145) )



########


#layout(matrix(1:1))
komodos <- dades[c(162,191:194,197:200),3:4]
coeff_komodos <- species(dades,c(192:193,197:200), color = "blue")
a_k <- coeff_komodos[1]
b_k <- coeff_komodos[2]


M_d = 10^8
M_m = 11*10^6

K_m = a_m*M_m^(b_m)
#K_dr = a_r*M_d^(b_r)
#K_dk = a_k*M_d^(b_k)

M_d2 <- (K_m*(1/a_r))^(1/b_r)
M_d3 <- (K_m*(1/a_k))^(1/b_k)


#M_d3 <- (K_m*(1/1.07))^(1/0.735)
print(M_d2/M_m)
print(c(a_r,b_r))

print(M_d3/M_m)
print(c(a_k,b_k))


### proves  FACTOR 10
p1 <- function(){
    
    
    komodos <- dades[c(162,191:194,197:200),3:4]
    coeff_komodos <- species(dades,c(193:194, 197:200), color = "blue")
    a_k <- coeff_komodos[1]
    b_k <- coeff_komodos[2]
    
    
    M_d = 10^8
    M_m = 11*10^6
    
    
    K_m = a_m*M_m^(b_m)
    #K_d = a_r*M_d^(b_r)
    
    #M_d2 <- (K_m*(1/a_r))^(1/b_r)
    
    
    M_d3 <- (K_m*(1/a_k))^(1/b_k)
    #M_d3 <- (K_m*(1/1.07))^(1/0.735)
    
    
    print(M_d3/M_m)
    
    print(c(a_k,b_k))
}

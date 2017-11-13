setwd("C:/Users/Daniel/Dropbox/Abogrados Uphyisics")
dades <- read.csv("tabuladata.csv",header=TRUE, na.strings=c("", "NA"))
names(dades) <- c("Species", "Common name", "m ", "FMR", "Taxon", "Habitat", "Diet", "Reference")

dades <- na.omit(dades)


files <- length(row.names(dades))
row.names(dades) <- as.character(seq(1,files,1))

mammals <- dades[1:79 ,3:4];
reptiles <- dades[146: 200,];

plotmr <- function(){
  setEPS()
  postscript("mamiferreptil.eps")
  layout(matrix(1:1))
  
  plot(seq(-1,5,1), seq(-1,5,1),  type = 'n',#xaxt = 'n', yaxt='n',
       xlab = expression(log[10] ~ "(Mass (g)) "), ylab = expression(log[10] ~"(FMR (kJ/day))", col = "orange" ))
  ### MAMONES
  mammals_m <- as.matrix(mammals$'m')
  mammals_m <- as.numeric(gsub(",", "", mammals_m))
  
  mammals_FMR <- as.matrix(mammals$'FMR')
  mammals_FMR <- as.numeric(gsub(",", "", mammals_FMR))
  
  
  points(log10(mammals_m),log10(mammals_FMR), col = "orange")
  
  Xx <- log10(mammals_m)
  Yx <- log10(mammals_FMR)
  regx_m <- lm(Yx ~ Xx)
  adjr_m <- summary(regx_m)$adj.r.squared
  abline(coef(regx_m)[1:2], lwd = 2, col = 'red')
  coeffs_m <- c(10^coef(regx_m)[1], coef(regx_m)[2], adjr_m)
  
  legend("topleft", bty="n", legend=paste("R2 =", 
                                          format(summary(regx_m)$adj.r.squared, digits=3)))
  
  ##REPTILES
  
  reptiles_m <- as.matrix(reptiles$'m')
  reptiles_m <- as.numeric(gsub(",", "", reptiles_m))
  
  reptiles_FMR <- as.matrix(reptiles$'FMR')
  reptiles_FMR <- as.numeric(gsub(",", "", reptiles_FMR))
  
  points(log10(reptiles_m),log10(reptiles_FMR), col = "green")
  
  
  
  Xx <- log10(reptiles_m)
  Yx <- log10(reptiles_FMR)
  regx_r <- lm(Yx ~ Xx)
  adjr_r <- summary(regx_r)$adj.r.squared
  abline(coef(regx_r)[1:2], lwd = 2, col = 'darkgreen')
  coeffs_r <- c(10^coef(regx_r)[1], coef(regx_r)[2], adjr_r)

  legend("bottomright", bty="n", legend=paste("R2 =", 
                                              format(summary(regx_r)$adj.r.squared, digits=3)))
  
  minor.tick(nx=2, ny=2, tick.ratio=0.5)
  ######################
  #####################
  title("FMR v.s mass")
  
  legend(-1,4, c("Mammals", "Reptiles"),
         lty=1, lwd=2, col=c("red", "darkgreen"));
  dev.off(); 
  plot(regx_r)
}

#####
#Komodos
#komodos <- dades[c(162,191:194,197:200),3:4]
komodos <- dades[c(193:194, 197:200),3:4] #els que donen 10

n = 8
plotmk <- function(){
  
  
  setEPS()
  postscript("mamiferkomodo.eps")
  
  layout(matrix(1:1))
  
  plot(seq(1,n,1), seq(1,n,1),  type = 'n',#xaxt = 'n', yaxt='n',
       xlab = expression(log[10] ~ "(Mass (g)) "), ylab = expression(log[10] ~"(FMR (kJ/day))", col = "orange" ))
  ### MAMONES
  mammals_m <- as.matrix(mammals$'m')
  mammals_m <- as.numeric(gsub(",", "", mammals_m))
  
  mammals_FMR <- as.matrix(mammals$'FMR')
  mammals_FMR <- as.numeric(gsub(",", "", mammals_FMR))
  
  
  points(log10(mammals_m),log10(mammals_FMR), col = "orange")
  
  Xx <- log10(mammals_m)
  Yx <- log10(mammals_FMR)
  regx_m <- lm(Yx ~ Xx)
  adjr_m <- summary(regx_m)$adj.r.squared
  abline(coef(regx_m)[1:2], lwd = 2, col = 'red')
  coeffs <- c(10^coef(regx_m)[1], coef(regx_m)[2], adjr_m)
  
  #legend("left", bty="n", legend=paste("R2 =", 
   #                                       format(summary(regx_m)$adj.r.squared, digits=3)))
  
  ##komodos
  
  komodos_m <- as.matrix(komodos$'m')
  komodos_m <- as.numeric(gsub(",", "", komodos_m))
  
  komodos_FMR <- as.matrix(komodos$'FMR')
  komodos_FMR <- as.numeric(gsub(",", "", komodos_FMR))
  
  points(log10(komodos_m),log10(komodos_FMR), col = "blue")
  
  
  
  Xx <- log10(komodos_m)
  Yx <- log10(komodos_FMR)
  regx_r <- lm(Yx ~ Xx)
  adjr_r <- summary(regx_r)$adj.r.squared
  abline(coef(regx_r)[1:2], lwd = 2, col = 'darkblue')
  coeffs <- c(10^coef(regx_r)[1], coef(regx_r)[2], adjr_r)
  
  #legend("bottomright", bty="n", legend=paste("R2 =", 
   #                                           format(summary(regx_r)$adj.r.squared, digits=3)))
  minor.tick(nx=2, ny=2, tick.ratio=0.5)
  legend(1,8, c("Mammals", "Varanus >1.3 Kg"),
         lty=1, lwd=2, col=c("red", "darkblue"), border = "white");
 
  ######################
  #####################
  title("FEE v.s mass")
  dev.off();
}

plotmk();
plotmr();



setwd("C:/Users/Daniel/Documents")
dades <- read.csv("tabuladata.csv",header=TRUE, na.strings=c("", "NA"))
names(dades) <- c("Species", "Common name", "m ", "FMR", "Taxon", "Habitat", "Diet", "Reference")

dades <- na.omit(dades)


files <- length(row.names(dades))
row.names(dades) <- as.character(seq(1,files,1))

mammals <- dades[1:79 ,3:4];
reptiles <- dades[146: 200, 3:4];


##MAMONES

mammals_m <- as.matrix(mammals$'m')
mammals_m <- as.numeric(gsub(",", "", mammals_m))

mammals_FMR <- as.matrix(mammals$'FMR')
mammals_FMR <- as.numeric(gsub(",", "", mammals_FMR))


Xx <- log10(mammals_m)
Yx <- log10(mammals_FMR)
regx_m <- lm(Yx ~ Xx)
adjr_m <- summary(regx_m)$adj.r.squared
coeffs_m <- c(10^coef(regx_m)[1], coef(regx_m)[2], adjr_m)


#reptos

reptiles_m <- as.matrix(reptiles$'m')
reptiles_m <- as.numeric(gsub(",", "", reptiles_m))

reptiles_FMR <- as.matrix(reptiles$'FMR')
reptiles_FMR <- as.numeric(gsub(",", "", reptiles_FMR))

Xx <- log10(reptiles_m)
Yx <- log10(reptiles_FMR)
regx_r <- lm(Yx ~ Xx)
adjr_r <- summary(regx_r)$adj.r.squared
coeffs_r <- c(10^coef(regx_r)[1], coef(regx_r)[2], adjr_r)

#KOMO

komodos <- dades[c(193:194, 197:200),3:4] #els que donen 10

komodos_m <- as.matrix(komodos$'m')
komodos_m <- as.numeric(gsub(",", "", komodos_m))

komodos_FMR <- as.matrix(komodos$'FMR')
komodos_FMR <- as.numeric(gsub(",", "", komodos_FMR))


Xx <- log10(komodos_m)
Yx <- log10(komodos_FMR)
regx_k <- lm(Yx ~ Xx)
adjr_k <- summary(regx_k)$adj.r.squared
coeffs_k <- c(10^coef(regx_k)[1], coef(regx_k)[2], adjr_k)



################
###############
a_m <- coeffs_m[1]
b_m <- coeffs_m[2]

a_r <- coeffs_r[1]
b_r <- coeffs_r[2]

a_k <- coeffs_k[1]
b_k <- coeffs_k[2]



M_d = 10^8
M_m1 = 1*10^6
M_m2 = 2*10^6

K_m1 = a_m*M_m1^(b_m)
K_m2 = a_m*M_m2^(b_m)

M_dr1 <- (K_m1*(1/a_r))^(1/b_r)
M_dk1 <- (K_m1*(1/a_k))^(1/b_k)

M_dr2 <- (K_m2*(1/a_r))^(1/b_r)
M_dk2 <- (K_m2*(1/a_k))^(1/b_k)


print(c(M_dr1/10^6, M_dr2/10^6))
print(c(M_dr1/M_m1,  M_dr2/M_m2))


print(c(M_dk1/10^6, M_dk2/10^6))
print(c(M_dk1/M_m1,  M_dk2/M_m2))

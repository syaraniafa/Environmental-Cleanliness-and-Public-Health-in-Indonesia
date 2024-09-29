library(readxl)
data <- read_excel("C:/Users/syarani/OneDrive - Bina Nusantara/tugas kuliah/Semester 6/Multivariate Statistics/AOL/dataset.xlsx")
head(data)
summary(data)
data$x2

#>> matriks korelasi keseluruhan
corr = cor(data[2:9])
corr

#>> partisi matriks korelasi berdasarkan kelompok variabel
#1. yy
rho11 = cor(data[2:9])[1:3,1:3]
rho11

#2. yx
rho12 = cor(data[2:9])[1:3,4:8]
rho12

#3. xy
rho21 = cor(data[2:9])[4:8,1:3]
rho21

#4. xx
rho22 = cor(data[2:9])[4:8,4:8]
rho22

#>> mencari nilai A
library(expm)
inverse_sqrtrho11 = solve(sqrtm(rho11))
inverse_sqrtrho11

inverse_rho22 = solve(rho22)
inverse_rho22

A = inverse_sqrtrho11 %*% rho12 %*% inverse_rho22 %*% rho21 %*% inverse_sqrtrho11
A

#>> Hitung nilai eigen dari matrix A
eigen(A)

#Akar dari nilai eigen terbesar adalah nilai kefisien korelasi kanonik antara kebersihan dan kesehatan
sqrt(eigen(A)$values)

#>> Kesimpulan
#Terbentuk 3 fungsi kanonik dimana fungsi kanonik pertama (U1, V1) memiliki koefisien korelasi sebesar 0.6843578, 
#kemudian fungsi kanonik kedua (U2, V2) memiliki koefisien korelasi sebesar 0.5755327,
#fungsi kanonik ketiga (U3, V3) memiliki koefisien korelasi sebesar 0.4327731
#Note: fungsi kanoniknya hanya 3 karena mengikuti kelompok variabel yang memiliki jumlah variabel yang paling kecil (di kasus ini = 3)


#mencari bobot kanonik
#eigenvector
eigen(A)$vectors
e1 = eigen(A)$vectors[1:3, 1]
e2 = eigen(A)$vectors[1:3, 2]
e3 = eigen(A)$vectors[1:3, 3]
e1
e2
e3

#data kelompok 1 (kebersihan lingkungan)
X_1 = as.matrix(data[2:4])
X_1

#data kelompok 2(kesehatan masyarakat)
X_2 = as.matrix(data[5:9])
X_2

# eigenvektor
eigenvectors_A = eigen(A)$vectors

#>> Mencari U dan V
#1. mencari a
a = inverse_sqrtrho11 %*% eigenvectors_A
print(a)
a1 = e1%*%inverse_sqrtrho11
a2 = e2%*%inverse_sqrtrho11
a3 = e3%*%inverse_sqrtrho11
a1
a2
a3

#2. mencari b
inverse_sqrtrho22 = solve(sqrtm(rho22))
inverse_sqrtrho22
inverse_rho11 = solve(rho11)
inverse_rho11

f1 = eigen(inverse_sqrtrho22%*%rho21%*%inverse_rho11%*%rho12%*%inverse_sqrtrho22)$vectors[, 1]
f1
f2 = eigen(inverse_sqrtrho22%*%rho21%*%inverse_rho11%*%rho12%*%inverse_sqrtrho22)$vectors[, 2]
f2
f3 = eigen(inverse_sqrtrho22%*%rho21%*%inverse_rho11%*%rho12%*%inverse_sqrtrho22)$vectors[, 3]
f3
  
b1 = f1%*%inverse_sqrtrho22
b2 = f2%*%inverse_sqrtrho22
b3 = f3%*%inverse_sqrtrho22
b1
b2
b3

#muatan kanonik
# Menghitung fungsi kanonik U untuk kelompok Kebersihan Lingkungan
U1 <- X_1 %*% t(a1)
U2 <- X_1 %*% t(a2)
U3 <- X_1 %*% t(a3)

# Menghitung fungsi kanonik V untuk kelompok Kesehatan Masyarakat
V1 <- X_2 %*% t(b1)
V2 <- X_2 %*% t(b2)
V3 <- X_2 %*% t(b3)

# Menghitung muatan kanonik untuk kelompok Kebersihan Lingkungan
canonical_loadings_Y <- cor(data[2:4], cbind(U1, U2, U3))
colnames(canonical_loadings_Y) <- c("U1", "U2", "U3")
print(canonical_loadings_Y)

# Menghitung muatan kanonik untuk kelompok Kesehatan Masyarakat
canonical_loadings_X <- cor(data[5:9], cbind(V1, V2, V3))
colnames(canonical_loadings_X) <- c("V1", "V2", "V3")
print(canonical_loadings_X)

#muatan kanonik silang
# Menghitung muatan kanonik untuk kelompok Kebersihan Lingkungan
canonical_loadings_Y2 <- cor(data[2:4], cbind(V1, V2, V3))
colnames(canonical_loadings_Y) <- c("U1", "U2", "U3")
print(canonical_loadings_Y)

# Menghitung muatan kanonik untuk kelompok Kesehatan Masyarakat
canonical_loadings_X2 <- cor(data[5:9], cbind(U1, U2, U3))
colnames(canonical_loadings_X) <- c("V1", "V2", "V3")
print(canonical_loadings_X)

#uji signifikansi parameter
#install.packages("CCA")
#install.packages("CCP")
library(CCA)
library(CCP)
cca_result <- cancor(X_1, X_2)
cca_result

print(cca_result$cor)

rho <- cca_result$cor
test_results <- p.asym(rho, nrow(data), ncol(X), ncol(Y))

# Print the test results
print(test_results)

#>> uji hipotesis independensi kanonik
lambda.test = det(corr)/(det(rho11)*det(rho22))
lambda.test


#mencari nilai lambda crit menggunakan tabel wilks
# tabel buku rencher halaman 629
# n = 34
# p = 3
# q = 5
# vh = q = 5
# ve = n-1-q = 34-1-3 = 30
# parameter lambda crit = lamda, vh, ve
nrow(data)
lambda.crit = 0.648
lambda.test < lambda.crit #true, artinya terdapat korelasi kanonik yang signifikan antar 2 set variabel tsb (bagus)


# uji serentak & parsial
#uji serentak 
cancor_result = cancor(X_1, X_2)

library(CCP)
canonical_correlations <- cancor_result$cor
canonical_correlations

#uji parsial
# Perform Wilks' Lambda test using CCP package
n <- nrow(data)
p <- 3
q <- 5
wilks_result <- p.asym(canonical_correlations, n, p, q, tstat = "Wilks")

# Print the results
wilks_result


#>> Uji asumsi
#a. uji normalitas
#install.packages("MVN")
# Uji normalitas untuk X_1 (kebersihan lingkungan)
shapiro.test(X_1[, 1])
shapiro.test(X_1[, 2])
shapiro.test(X_1[, 3])
X_1

# Uji normalitas untuk X_2 (kesehatan masyarakat)
shapiro.test(X_2[, 1])
shapiro.test(X_2[, 2])
shapiro.test(X_2[, 3])
shapiro.test(X_2[, 4])
shapiro.test(X_2[, 5])
X_2

#b. uji multicolinearitas
cor(X_1)
cor(X_2)

#d. uji homoskedastisitas
library(lmtest)
bptest(X_1~X_2)







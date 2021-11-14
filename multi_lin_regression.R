x <- cigarettes
x_used <- data.frame(1, x[2], x[3], x[4])

# 1/ size of the dataframe
dim(x_used)

# 2/ size of (x*t(x)) 
names(x_used) <- NULL
print(x_used)

a <- as.matrix(x_used) #transform a dataframe to a matrix
b <- as.matrix(t(x_used))
print(a)

result_mult <- b %*% a

print(result_mult)
dim(result_mult)

# 3/ t(x)*y
c <- as.matrix(x[5])
print(x[5])
result <- b %*% c 
result

# 4/ least square coefficients
coeff <- solve(result_mult) %*% result
print(coeff)

# 5/ -predicted values
predicted <- a %*% coeff
print(predicted)
# -residues
epsilon <- c - predicted
print(epsilon)

#6/ (a) degree of freedom
nb_obs <- dim(x_used)[1]
nb_fact <- dim(x_used)[2]
ddl <- nb_obs - nb_fact

ddl

#(b) estimate the variance
for (i in epsilon){
  s <- 0
  s <- s + epsilon[i]**2
}
estimated_var <- s/ddl
estimated_var

#(c) estimated matrix of covariance
estimated_matrix_cov <- estimated_var * solve(result_mult)
estimated_matrix_cov

#(d) standard errors of coefficients
std_0 <- estimated_matrix_cov[1,1]
std_1 <- estimated_matrix_cov[2,2]
std_2 <- estimated_matrix_cov[3,3]
std_3 <- estimated_matrix_cov[4,4]
std <- data.frame(std_0, std_1, std_2, std_3)
std

#7/ (a) mean of Y
mean_Y <- mean(c)
mean_Y

#(b) SC - ddl - CM
var_anal_tab <- data.frame((matrix(ncol = 3, nrow = 3)))
colnames(var_anal_tab) <- c("SC", "DDL", "CM")
rownames (var_anal_tab) <- c("Expliquée", "Résiduelle", "Totale")

  # SCE, SCR and SCT
SCE <- 0
SCR <- 0
for (i in 1:nb_obs){
  SCE <- SCE + (predicted[i] - mean_Y)**2
  SCR <- SCR + (c[i] - predicted[i])**2
}
var_anal_tab$SC[1] <- SCE
var_anal_tab$SC[2] <- SCR
var_anal_tab$SC[3] <- SCE + SCR

  #ddl
var_anal_tab$DDL[1] <- nb_fact - 1
var_anal_tab$DDL[2] <- ddl
var_anal_tab$DDL[3] <- nb_fact - 1 + ddl

  #CM
var_anal_tab$CM[1] <- (var_anal_tab$SC[1])/(var_anal_tab$DDL[1])
var_anal_tab$CM[2] <- (var_anal_tab$SC[2])/(var_anal_tab$DDL[2])
var_anal_tab

#(c) determination coeff
R_sqr <- var_anal_tab$SC[1]/var_anal_tab$SC[3]
R_sqr # equals to 0.93 --> the variables Goudron, Nicotine and Poids
      #explain the target class CO by a perecentage of 93%

















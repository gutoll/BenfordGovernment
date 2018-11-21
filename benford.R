
library('BenfordTests')
library('moments')

con = file(filepath, "r")
num = c()
digit1 = integer(9)
digit2 = integer(10)
digit12 = integer(99)
i = 1


# Leitura dos dados e identificação do primeiro e dos dois primeiros algarismos dos dados#
while (TRUE) {    
	line = readLines(con, n = 1)
    if(length(line) == 0)
    	break;
    num[i] = as.double(line)
    aux = num[i] * 1000
    while(aux >= 100) {
    	aux = floor(aux / 10)
    }

    digit12[aux] = digit12[aux] + 1
    aux = floor(aux / 10)
    digit1[aux] = digit1[aux] + 1   
    i = i + 1
}

#resultados de média, mediana e obliquidade#
cat("mean = ", mean(num), "\n")
cat("median = ", median(num), "\n")
cat("skewness = ", skewness(num), "\n")


#gerando as porcentagens#  
digit1 = (digit1 * 100)/(i-1)
digit12 = (digit12[1:99] * 100)/(i - 1)
print(digit1)
print(digit12)


#identificando o segundo algarismo dos dados isoladamente#
for(i in c(10:99)) {
	if(i %% 10 == 0)
		digit2[10] = digit2[10] + digit12[i]
	else
		digit2[i %% 10] = digit2[i %% 10] + digit12[i]
}



#Usado na impressão dos dados de forma a gerar facilmente os gráficos no LaTeX
#for(i in c(1:9)) {
# 	cat("(", i, ", ", round(digit1[i], digits=2), ") ")
#}
#cat("\n( 0 , ", round(digit2[10], digits=2), ") ")
#for(i in c(1:9)) {
# 	cat("(", i, ", ", round(digit2[i], digits=2), ") ")
#}
#cat("\n")
#for(i in c(10:99)) {
#	cat("(", i, ", ", round(digit12[i], digits=2), ") ")
#}
#cat("\n")

#gerando os resultados dos testes#
print(ks.benftest(num))
print(ks.benftest(num, digits=2))
print(chisq.benftest(num))
print(chisq.benftest(num, digits=2))

close(con)
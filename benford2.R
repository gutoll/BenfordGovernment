library('BenfordTests')
library('moments')

con = file(filepath, "r")
num = c()
digit1 = integer(9)
digit2 = integer(10)
digit12 = integer(99)
i = 1

# Leitura dos dados#
while (TRUE) {    
	line = readLines(con, n = 1)
    if(length(line) == 0)
    	break;
    num[i] = as.double(line)
    i = i + 1
}


# Torna os dados únicos, isto é, retira as repetições. Usado no teste final do trabalho, não usado no secundário#
#num = unique(num)

#identificando o primeiro e os dois primeiros algarismos dos dados#
for(i in 1:length(num)) {
  aux = num[i] * 1000
  while(aux >= 100) {
  	aux = floor(aux / 10)
  }
  digit12[aux] = digit12[aux] + 1
  aux = floor(aux / 10)
  digit1[aux] = digit1[aux] + 1   
}
i = i + 1

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
	if(i %% 10 == 0) {
    digit2[10] = digit2[10] + digit12[i]
  }else{
    digit2[i %% 10] = digit2[i %% 10] + digit12[i]
  }
}
print(digit2)


#Usado na impressão dos dados de forma a gerar facilmente os gráficos no LaTeX#
#for(i in c(1:9)) {
#   cat("(", i, ", ", round(digit1[i], digits=2), ") ")
#}
#cat("\n( 0 , ", round(digit2[10], digits=2), ") ")
#for(i in c(1:9)) {
#   cat("(", i, ", ", round(digit2[i], digits=2), ") ")
#}
#cat("\n")
#for(i in c(10:99)) {
# cat("(", i, ", ", round(digit12[i], digits=2), ") ")
#}
#cat("\n")

#mapeando as porcentagens em um novo conjunto de aproximadamente 10 mil dados#
limitated_num = c()
count = 1
for(i in c(10:99)) {
  for(j in 1:floor(digit12[i] * 100)) {
    limitated_num[count] = i
    count = count + 1
  }
}

#gerando os resultados dos testes#
print(ks.benftest(limitated_num))
print(ks.benftest(limitated_num, digits=2))
print(chisq.benftest(limitated_num))
print(chisq.benftest(limitated_num, digits=2))


close(con)
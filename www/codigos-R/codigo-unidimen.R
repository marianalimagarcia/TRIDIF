library(psych)

# se os dados da matriz forem dicotomicos
correlacao = tetrachoric(matriz)
# se os dados da matriz forem politomicos
correlacao = polychoric(matriz)    

propexp = eigen(correlacao$rho)$values/sum(eigen(correlacao$rho)$values)
proptexto = propexp[1]*100

if (proptexto >= 20){
  print("Suposição de unidimensionalidade atendida")
}else{
  print("Suposição de unidimensionalidade NÃO atendida")
}

plot(x = 1:ncol(matriz), y = propexp, type = "b")

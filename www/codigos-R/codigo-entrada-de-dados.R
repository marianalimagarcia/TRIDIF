DF = read.csv(file   = "nomedoarquivo.csv")

#se os dados não tiverem a coluna grupo
matriz = DF

#se os dados tiverem a coluna grupo
matriz = DF[, 1:(ncol(DF) - 1)]
grupo = DF[, ncol(DF)]
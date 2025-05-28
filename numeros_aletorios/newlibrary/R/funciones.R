#numeros aleatorios de notas
rm(list = ls())
set.seed(as.numeric(Sys.time()))
#numero de estudiantes
n <- 20

# Generación de notas
notas <- round(runif(n, min = 0, max = 20), 1)
print(notas)

# Grafico
plot(notas,
     type = "o",
     col = "black",
     pch = 19,
     xlab = "Estudiante",
     ylab = "Nota",
     main = "Notas Aleatorias de Estudiantes",
     ylim = c(0, 20))

text(x = 1:n, y = notas + 1, labels = notas, col = "purple")

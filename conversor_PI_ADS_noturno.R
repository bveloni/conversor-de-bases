# Conversor de bases - atividade do 1o semestre de ADS
# Bases: decimal (10), binario (2), hexadecimal (16), octal (8) e quaternario (4).

cat("=== CONVERSOR DE BASES ===\n")
cat("1 - Decimal (base 10)\n")
cat("2 - Binario (base 2)\n")
cat("3 - Hexadecimal (base 16)\n")
cat("4 - Octal (base 8)\n")
cat("5 - Quaternario (base 4)\n\n")

origem <- readline("Escolha a base de origem (1 a 5): ")
destino <- readline("Escolha a base de destino (1 a 5): ")

opcoes <- c("1", "2", "3", "4", "5")
bases <- c(10, 2, 16, 8, 4)
algarismos <- c("0", "1", "2", "3", "4", "5", "6", "7",
                "8", "9", "A", "B", "C", "D", "E", "F")

if (!(origem %in% opcoes) || !(destino %in% opcoes)) {
  cat("Opcao invalida. Digite um numero de 1 a 5.\n")
} else if (origem == destino) {
  cat("Escolha duas bases diferentes.\n")
} else {
  base_origem <- bases[as.numeric(origem)]
  base_destino <- bases[as.numeric(destino)]
  entrada <- toupper(readline("Digite o numero (sem sinal): "))
  valido <- TRUE
  decimal <- 0

  if (nchar(entrada) == 0) {
    valido <- FALSE
  } else {
    # Converter para decimal: cada algarismo vezes a base elevada a sua posicao.
    for (i in 1:nchar(entrada)) {
      caractere <- substr(entrada, i, i)
      digito <- -1

      for (j in 1:16) {
        if (caractere == algarismos[j]) {
          digito <- j - 1
        }
      }

      if (digito < 0 || digito >= base_origem) {
        valido <- FALSE
        break
      }

      expoente <- nchar(entrada) - i
      decimal <- decimal + digito * base_origem^expoente
    }
  }

  if (valido && (!is.finite(decimal) || decimal > 2147483647)) {
    valido <- FALSE
    cat("Numero muito grande. Limite: 2147483647 em decimal.\n")
  }

  if (!valido) {
    cat("Entrada invalida para a base escolhida.\n")
  } else {
    resultado <- ""

    if (base_destino == 10) {
      resultado <- format(decimal, scientific = FALSE, trim = TRUE)
    } else {
      numero <- decimal

      if (numero == 0) {
        resultado <- "0"
      } else {
        # Dividir pela base de destino e guardar os restos.
        while (numero > 0) {
          resto <- numero %% base_destino
          resultado <- paste0(algarismos[resto + 1], resultado)
          numero <- numero %/% base_destino
        }
      }
    }

    cat("Resultado: ", resultado, "\n", sep = "")
  }
}

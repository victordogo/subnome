#' substitui_nome
#'
#' Pacote teste para a disciplina de Perspectiva em Ciência de Dados que substitui nomes dentro de uma string.
#'
#' @param string Uma string
#' @param aluno Um nome de aluno, padrao "Victor"
#' @param aluna Um nome de aluna, padrao "Samantha"
#' @param professor Um nome de professor, padrao "Rafael"
#'
#' @return
#' A string com palavras "professor","aluno" ou "aluna" substituidas
#' por nomes "Rafael", "Victor" ou "Samantha" respectivamente.
#'
#' @export
#'
#' @examples
#' substitui_nome("O professor deu uma prova. A aluna foi muito bem, ja o aluno, nem tanto.")
#'
substitui_nome <- function(string, aluno="Victor", aluna="Samantha", professor="Rafael"){
  # Checando se o argumento e uma string

  if(is.character(string)!=TRUE |
     is.character(aluno)!=TRUE |
     is.character(aluna)!=TRUE |
     is.character(professor)!=TRUE){
    stop("Argumentos de entrada nao sao uma string.")
  }

  # Substituindo professor por nome
  if(
    stringr::str_detect(string, "professor") |
    stringr::str_detect(string, "Professor")
  ){
    string <- stringr::str_replace(string,"professor",professor)
    string <- stringr::str_replace(string,"Professor",professor)
  }

  # Substituindo aluno por nome
  if(
    stringr::str_detect(string, "aluno") |
    stringr::str_detect(string, "Aluno")
  ){
    string <- stringr::str_replace(string,"aluno",aluno)
    string <- stringr::str_replace(string,"Aluno",aluno)
  }

  # Substituindo aluna por nome
  if(
    stringr::str_detect(string, "aluna") |
    stringr::str_detect(string, "Aluna")
  ){
    string <- stringr::str_replace(string,"aluna",aluna)
    string <- stringr::str_replace(string,"Aluna",aluna)
  }

  # Retornando resultado

  return(string)
}

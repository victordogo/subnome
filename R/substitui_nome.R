#' substitui_nome
#'
#' @param string Uma string
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
substitui_nome <- function(string){
  # Checando se o argumento e uma string

  if(is.character(string)!=TRUE){
    stop("O argumento de entrada nao e uma string.")
  }

  # Substituindo professor por nome
  if(
    stringr::str_detect(string, "professor") |
    stringr::str_detect(string, "Professor")
  ){
    string <- stringr::str_replace(string,"professor","Rafael")
    string <- stringr::str_replace(string,"Professor","Rafael")
  }

  # Substituindo aluno por nome
  if(
    stringr::str_detect(string, "aluno") |
    stringr::str_detect(string, "Aluno")
  ){
    string <- stringr::str_replace(string,"aluno","Victor")
    string <- stringr::str_replace(string,"Aluno","Victor")
  }

  # Substituindo aluna por nome
  if(
    stringr::str_detect(string, "aluna") |
    stringr::str_detect(string, "Aluna")
  ){
    string <- stringr::str_replace(string,"aluna","Samantha")
    string <- stringr::str_replace(string,"Aluna","Samantha")
  }

  # Retornando resultado

  return(string)
}

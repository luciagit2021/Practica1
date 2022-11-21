


#' Agencia Tributaria
#' Tablas: "Asalariados, percepciones salariales y salarios por Comunidad Aut?noma"
#' Autor: Lucía Fernández González
#' output: csv
#' ---


rm(list = ls())


getwd()

###cargo las librerías
pacman::p_load(glue, openxlsx, rvest, httr, tidyverse, tidyselect)


UserAgent <- paste(
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/107.0.0.0 Safari/537.36")

#### dirección web de las estadísticas de la AEAT

base_url <- glue(
  "https://www.agenciatributaria.es/AEAT/Contenidos_Comunes/La_Agencia_Tributaria/Estadisticas/Publicaciones/sites/mercado/2021/"
)
pag_url <- "jrubikf556e330817d393a43709315680b99c80da2c25b9.html"

nombre_tabla <- "Asalariados, percepciones salariales y salarios"



# función que obtiene todos los enlaces que se pueden extraer a partir de una pÃ¡gina, que serían 17

obtener_enlaces <- function(url_base, pagina) {
  cat(paste(Sys.time(), "- abriendo pagina:", pagina,"\n"))
  page_url <- glue(base_url, pagina)
  home_page <- session(url = page_url, encoding = "UTF-8", user_agent(UserAgent), timeout(10))####abrir una sesión, hasta 10 segundos
  
  lista_nombres <- home_page %>%
      html_nodes(css = "#menubar1 .m_scroll a") %>%
      html_text()    ###cojo el texto que figura en los selectores de tipo "#menubar1 .m_scroll a" y lo guardo en lista_nombres
  lista_urls <- home_page %>%
      html_nodes(css = "#menubar1 .m_scroll a") %>%
      html_attr(name = "href")  ##### cojo los enlaces que figuran en los selectores de tipo "#menubar1 .m_scroll a" y los guardo en lista_urls
  lista_enlaces <- tibble(
      nombres = lista_nombres,
      enlaces = lista_urls
  )                 #####creo una tabla con el texto y el enlace
  
  return(lista_enlaces)
}


# extrae la tabla de una determinada página y el título donde se indica el cruce realizado

obtener_tablas <- function(url_base, pagina) {
  cat( paste(Sys.time(), "- abriendo pagina:", pagina, "\n"))
  # obtener los enlaces
  page_url <- glue(base_url, pagina)
  home_page <-session(url = page_url, encoding = "UTF-8", user_agent(UserAgent), timeout(10))
  
 
    tabla_datos <- home_page %>%
      html_nodes(css = "#table01") %>%   ###extraigo la tabla marcada con el selector
      html_table(convert = FALSE)
    tabla_asalariados <- as.data.frame(tabla_datos[[1]])
    names(tabla_asalariados)[1]<- "Espacio"
    
    tabla_asalariados <- tabla_asalariados %>%
      
      mutate(across(
        .cols = c(Asalariados, Salarios, `Salario  Medio Anual`),
        ~ as.numeric(str_remove_all(string = .x, pattern = "\\."))
      )) %>%
      mutate(across(
        .cols = `Percepciones  por persona`,
        ~ as.numeric(str_replace(string = .x, pattern = ",", replacement = "\\."))
      ))
    grupo <- home_page %>%
      html_nodes(css = "h2") %>%   
      html_text() %>%           #####extraigo el título de la tabla, le doy formato y me quedo con la parte donde se indica el cruce realizado
      str_squish() %>%
      str_split(pattern = ",") %>%
      unlist() %>%
      str_squish() %>%
      str_remove_all(pattern = ".*: ")
    
    tabla_asalariados$sexo = grupo[1] 
    tabla_asalariados$edad = grupo[2] 
    tabla_asalariados$nacionalidad = grupo[3] 
    tabla_asalariados$erte= grupo[4]
  
  return(tabla_asalariados)
}


# obtener los enlaces dentro de la página inicial
enlaces <- obtener_enlaces(url_base = base_url, pagina = pag_url)

sexo <- enlaces$nombres[1:3]
edad <- enlaces$nombres[4:11]
nacionalidad <- enlaces$nombres[12:14]
erte<-enlaces$nombres[15:17]

# todos los cruces posibles, 216 tablas posibles
total <- expand_grid(sexo, edad, nacionalidad, erte)


####aplico a cada uno de los 17 enlaces de la página original la función obtener enlaces para obtener los enlaces que habría dentro de cada una de ellas
######y así recursivamente hasta obtener todos los enlaces a todas las tablas posibles

while (nrow(enlaces) < nrow(total)) {
  enlaces <- map_dfr(.x = enlaces$enlaces, .f = ~ obtener_enlaces(url_base = base_url, pagina = .x))
  enlaces <- enlaces %>% distinct(enlaces, .keep_all = TRUE)
}

# obtener todas las tablas de datos, aplicando a los 216 enlaces anteriores la función obtener_tablas
##se almacenan en un dataframe una debajo de otra

tablas <- map_dfr(.x = enlaces$enlaces, .f = ~ obtener_tablas(url_base = base_url, pagina = .x))


# guardar el resultado --------------------------------------------------------------------------------------------
write.csv2(tablas, "tablas_aeat.csv", row.names = FALSE)





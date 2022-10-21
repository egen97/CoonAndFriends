library(googleLanguageR)

vignette("googleLanguageR")
gl_auth("Secure/secure-site-328710-ada7added035.json")

?gl_translate

russian_text <- "Почему у меня такие сухие глаза, меня это начинает раздражать!"

hello_world <- gl_translate(
  russian_text,
  target = "en"
)



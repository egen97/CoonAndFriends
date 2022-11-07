# Welcome to OSINT Telegram!

With OSINT: Telegram, we have automatically collected, translated and analysed more than 300,000 telegram posts from 19 of Russia’s biggest pro-War telegram channels to make them accessible to researchers, journalists and the interested public.

While some of these war blogs are already used as valuable sources by a handful of analysists and journalists, the information is spread out, only available in Russian, and not easily available for systematic analysis. We want to change this, and make the data available for the interested public! With this solution, we aim to:

 - Create transparency and awareness in the international community around war propaganda.
 - Make research data available that could be used for analytic purposes, including predicting war actions.
 - Offer a tool to follow and understand the development of the war through Telegram bloggers.

This repository includes the code we made during the Hack4Peace event 2022. Here, we used the telegram API to automatically download all posts on the selected channels from 01.01.2022 – 21.10.2022, translated them to English using the Google translate API, and ran sentiment analyses using Google’s natural language processing algorithm and Vader’s unsupervised classification algorithm. Using entity recognition, we further identified all Ukrainian cities mentioned in the posts and used the Google Maps API to geolocate them.

The package with the function used in this project can be found here: https://github.com/egen97/peacePHDs 

Please see https://solveig.shinyapps.io/prototype/ to view the prototype.

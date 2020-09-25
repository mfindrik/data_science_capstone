Coursera Data Science Capstone Project
========================================================
author: Mislav F.
date: 23.09.2020
autosize: true

- Pitch slide deck for NLP word prediction app

Overview
========================================================

- The objective of this capstone was to develop a web app that can predict the next word, like that used by mobile keyboards applications implemented by the Swiftkey.

- There are many tasks to be realized such as: (1) Understanding the problem, (2) Cleaning and exploring text data used for model learning; (3) Preprocessing the text data and extracting feature; (4) Learning the NLP model; (5) Developing a web application and writing the slides.

- The data for model learning came from three files (Blogs, News and Twitter). This data has been collected by crawling blog, news, and twits on the Internet. The data was cleaned, processed, tokenized, and n-gram features are created. 

Methods and approach
========================================================

- Firstly, the provided datasets have been explored, cleaned and normalized. Cleaning was performed by removing the puntuations, whitespaces, numbers, etc. while normalization was done by casting words to lower case.

- Secondly, so-called n-grams (i.e. bigrams and trigrams) features where created to construct the predictive models.

- Finally, from n-gram features word frequncy table was created for predicting most likely next words in the text sequence, given the previous one or two words.


The web-app user interface
========================================================

- Screenshot of the user interface with the box for inputing the text and list of top three suggested words.

<center>
![Web application](presentation-figure/web-app.png)<-
</center>

Remarks and links
========================================================

- Preformance could be improved by increasing the sample size.

- Disk usage could also be improved by keeping only top three predictions.

- The web-applicatoin can be found under this link: [web app](https://mfindrik.shinyapps.io/final_project_app/)

- Github repository with code for the web-app can be found here: [code repo](https://github.com/mfindrik/data_science_capstone)


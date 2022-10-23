# CoonAndFriends

## The idea

We aim to create a continuous content analysis using quantitative text analysis methods such as topic modelling to map how the Russian people perceive the war in Ukraine. We'll do this using blog posts from Telegram, VK and other relevant sources. Understanding different perspectives is fundamental to converse, negotiate and prevent conflict. It can also be used to target the most effective or pressing areas for internal change.

## General setup

	1. Data fetching
	
	a. MVP
		a. Identify channels we'd like to scrape (20-30 channels?) X
		b. Run scraper on the texts X
		c. Run the texts through a translator X
	
		d. Post translated version back to telegram -  Solveig
	
	b. Automate:
		a. Script runs every hour
		b. Send to translator 
		c. Upload translated file to dropbox
	
	2. Analyze
		a. Priority: Entity recognition 
			1. Dictionary with fuzzy search
			2. Spacy
		Output: Unit = date_place, variable = the channels mentioning the place at that date
		 - Starting from 1. january 2022
		
		b. If time: 
			i. Sentiment analysis (dictonary methods)
			ii. Topic modelling
	
	3. Present
		a. Dashboard/shiny app
		b. With a map
		c. And telegram channels
		d. The "Putin-meter"
		

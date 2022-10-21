# CoonAndFriends

## The idea

We aim to create a continuous content analysis using quantitative text analysis methods such as topic modelling to map how the Russian people perceive the war in Ukraine. We'll do this using blog posts from Telegram, VK and other relevant sources. Understanding different perspectives is fundamental to converse, negotiate and prevent conflict. It can also be used to target the most effective or pressing areas for internal change.

## General setup

	1. Data fetching
	
	a. MVP
		a. Identify channels we'd like to scrape (20-30 channels?)
		b. Run scraper on the texts
		c. Run the texts through a translator
		d. Post translated version back to telegram?
	
	b. Automate:
		a. Script runs every hour
		b. Send to translator 
		c. Upload translated file to dropbox
	
	2. Analyze
		a. Priority: Entity recognition
		
		b. If time: 
			i. Sentiment analysis (dictonary methods)
			ii. Topic modelling
	
	3. Present
		a. Dashboard
		b. With a map
		c. And telegram channels
		d. The "Putin-meter"
		

# Mining the Twitter Network of US Congressmen

The project scrapes lists of friends and followers of US Congressmen (as of 2017) and calculates their ideological positions using Correspondence Analysis.

See https://twitter.com/cspan/lists/members-of-congress/members for an updated list.

The design of this project is based on the work of Pablo Barbera: https://github.com/pablobarbera/twitter_ideology.
I develop new modules to organize and store scraped data and revise some error-handling functions. The changes serve to fits the particular purpose of our project.

This may not work with the current version of Twitter API, as Twitter's API changed a lot since this scraper was developed.

outputs/uk/default-default.zip: data/EHS_2012/r_sedbuk.csv *.R ehcs/ scotland/* wales/*
	Rscript main.R

data/EHS_2012/r_sedbuk.csv:
	git clone git@bitbucket.org:cse-bristol/survey-data.git
	cp -R survey-data/* data
	rm -rf survey-data

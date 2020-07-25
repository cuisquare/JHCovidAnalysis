IF NOT EXIST Data (
	mkdir Data
	cd Data
	git clone https://github.com/CSSEGISandData/COVID-19.git JH_Data_GitHubClone
	cd ..
	)
ELSE (
	Rem assumes the git directoy exists as defined above
	cd Data/JH*
	git pull
	cd ../..
	)
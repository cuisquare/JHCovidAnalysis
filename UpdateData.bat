IF NOT EXIST Data (
	mkdir Data
)
cd Data
IF NOT EXIST JH_Data_GitHubClone (
  git clone https://github.com/CSSEGISandData/COVID-19.git JH_Data_GitHubClone
)
Rem assumes the git directoy exists as defined above
cd JH*
git pull
cd ../..
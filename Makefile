all : README.txt

README.txt : README.md
	pandoc README.md -o README.txt

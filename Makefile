all : README.txt codebook.pdf

codebook.pdf : codebook.md
	pandoc codebook.md -o codebook.pdf

README.txt : README.md
	pandoc README.md -o README.txt

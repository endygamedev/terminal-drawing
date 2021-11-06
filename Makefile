PROJECT_NAME = terminal-drawing


all:
	cabal build
	clear
	cabal exec ${PROJECT_NAME} 

distDir := $(shell stack path --dist-dir)
outDir := $(distDir)/build/Rechner

ifeq ($(OS),Windows_NT)
    ext := .exe
endif

rechner := $(outDir)/Rechner$(ext)

$(rechner): app/Main.hs src/Formel.hs src/Parser.hs
	stack build

dist: $(rechner)
	cp $(rechner) .

clean:
	-rm -rf .stack-work/
	-rm ./Rechner$(ext)

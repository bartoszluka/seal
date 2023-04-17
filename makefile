OUT_DIR = ./out
NAME = main
MAIN = ${NAME}.seal
DOT_S = ${OUT_DIR}/${NAME}.s
DOT_LL = ${OUT_DIR}/${NAME}.ll
DOT_O = ${OUT_DIR}/${NAME}.o
EXE = ${OUT_DIR}/${NAME}

build: executable

run: executable
	@${EXE}

executable: dot-o
	gcc -no-pie ${DOT_O} -o ${EXE}

dot-o: dot-s
	gcc -c -no-pie ${DOT_S} -o ${DOT_O}

dot-s: dot-ll
	llc -filetype=asm ${DOT_LL} -o ${DOT_S}

dot-ll: compiler
	@mkdir -p ${OUT_DIR}
	stack run compile ${MAIN} > ${DOT_LL}

compiler:
	stack build

clean:
	rm -rf ${OUT_DIR}


.PHONY: build run executable dot-o dot-s dot-ll compiler clean

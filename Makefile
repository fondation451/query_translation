# MAKEFILE SQUALL PARSER

SHELL      := bash
TARGET     := main.native
JOUJOU     := squallc
OCAMLBUILD :=\
  ocamlbuild \
    -classic-display \
    -j 4 \
    -use-ocamlfind \
    -use-menhir \
    -menhir "menhir -lg 1 -la 1 --explain" \

.PHONY: all test clean

all:
	@ $(OCAMLBUILD) -quiet $(TARGET)
	@ ln -sf $(TARGET) $(JOUJOU)

clean:
	rm -f *~
	rm -f tests/*.c tests/*.out
	$(OCAMLBUILD) -clean
	rm -f $(TARGET) $(JOUJOU)

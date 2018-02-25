SHELL      := bash
TARGET     := main.native
SQUALLC    := squallc
OCAMLBUILD :=\
  ocamlbuild \
    -classic-display \
    -j 4 \
    -use-ocamlfind \
    -use-menhir \
    -menhir "menhir -lg 1 -la 1 --explain" \

.PHONY: all clean

all:
	@ $(OCAMLBUILD) -quiet $(TARGET)
	@ ln -sf $(TARGET) $(SQUALLC)

clean:
	rm -f *~
	rm -f tests/*.c tests/*.out
	$(OCAMLBUILD) -clean
	rm -f $(TARGET) $(SQUALLC)

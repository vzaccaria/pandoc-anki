
test: 
	stack run pandoc-anki -- Algebra.org -i

Algebra.pdf: Algebra.org
	org2pdf $< --fast -v -k

.PHONY = clean
clean:
	rm -f tmp-*

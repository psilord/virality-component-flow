all: graphs

z3:
	@echo Z3 computed...
	@cat flow.z3 | z3 -in

graphs: actor-data-flow.pdf component-data-flow.pdf flow-state-machine.pdf

actor-data-flow.pdf: actor-data-flow.dot
	dot -Tpdf actor-data-flow.dot -o actor-data-flow.pdf

component-data-flow.pdf: component-data-flow.dot
	dot -Tpdf component-data-flow.dot -o component-data-flow.pdf

flow-state-machine.pdf: flow-state-machine.dot
	dot -Tpdf flow-state-machine.dot -o flow-state-machine.pdf

clean:
	rm *.pdf

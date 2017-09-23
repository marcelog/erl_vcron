CWD=$(shell pwd)
ROOT?=${CWD}
REBAR?=${ROOT}/rebar3.1

all: clean dialyzer xref tests cover

run: get_deps shell

shell:
	${REBAR} shell

clean:
	${REBAR} clean

dialyzer:
	${REBAR} dialyzer

xref:
	${REBAR} xref

get_deps:
	${REBAR} get-deps

tests:
	${REBAR} eunit

cover:
	${REBAR} cover

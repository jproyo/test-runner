all: setup build

build: build-srv build-cl 

clean: clean-cl clean-srv

clean-cl:
	(cd client; make clean)

build-cl:
	(cd client ; make)

clean-srv:
	stack clean 

build-srv:
	stack build

setup:
	stack setup
	stack test --only-dependencies

start: build
	stack exec test-runner -- -f server/app/conf.yaml

test:
	stack test

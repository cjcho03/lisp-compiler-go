all: run

build/out.s: *.go
	mkdir -p build
	echo "(+ 1 (* 2 3))" | go run . > build/out.s

build/a.out: build/out.s
	gcc -no-pie build/out.s -o build/a.out

run: build/a.out
	./build/a.out

clean:
	rm -rf build

[ -e "output.txt" ] && rm output.txt

log() {
	echo $1 >> output.txt
}

log "Compiling files.." 
ocamlbuild -r 'parse.byte' >> output.txt 2>&1 || { echo "Failed to compile."; exit 1; }
log "Compiled successfully"

negative_tests() {
	echo "../testcases/test49.tig"
}
negative_tests=$(negative_tests)

positive_tests=$(comm -23 <(ls ../testcases/*) <(negative_tests))

log "Running tests.."
for testfile in $positive_tests; do
	log "Testing $testfile"
	./parse.byte $testfile >> output.txt || { echo "Failed to parse file $testfile." ; exit 1; }
	log "Success"
	log "----"
done

for testfile in $negative_tests; do
	log "Testing $testfile"
	./parse.byte $testfile >> output.txt && { echo "Parse of $testfile succeeded when failure was expected."; exit 1; }
	log "Success"
	log "----"
done
log "Tests run successfully"

echo "Build and test successful."
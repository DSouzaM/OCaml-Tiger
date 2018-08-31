[ -e "output.txt" ] && rm output.txt

log() {
	echo $1 >> output.txt
}

log "Compiling files.." 
ocamlbuild -r 'main.byte' >> output.txt 2>&1 || { echo "Failed to compile."; exit 1; }
log "Compiled successfully"

negative_tests (){
	echo '../testcases/test9.tig'
 	echo '../testcases/test10.tig'
	echo '../testcases/test11.tig'
	echo '../testcases/test13.tig'
	echo '../testcases/test14.tig'
	echo '../testcases/test15.tig'
	echo '../testcases/test16.tig'
	echo '../testcases/test17.tig'
	echo '../testcases/test18.tig'
	echo '../testcases/test19.tig'
	echo '../testcases/test20.tig'
	echo '../testcases/test21.tig'
	echo '../testcases/test22.tig'
	echo '../testcases/test23.tig'
	echo '../testcases/test24.tig'
	echo '../testcases/test25.tig'
	echo '../testcases/test26.tig'
	echo '../testcases/test28.tig'
	echo '../testcases/test29.tig'
	echo '../testcases/test31.tig'
	echo '../testcases/test32.tig'
	echo '../testcases/test33.tig'
	echo '../testcases/test34.tig'
	echo '../testcases/test35.tig'
	echo '../testcases/test36.tig'
	echo '../testcases/test38.tig'
	echo '../testcases/test39.tig'
	echo '../testcases/test40.tig'
	echo '../testcases/test43.tig'
	echo '../testcases/test45.tig'
	echo '../testcases/test49.tig'
}
negative_tests=$(negative_tests)
positive_tests=$(comm -23 <(ls ../testcases/*) <(negative_tests | sort))

log "Running tests.."
for testfile in $positive_tests; do
	log "Testing $testfile"
	./main.byte $testfile >> output.txt || { echo "Failed to compile file $testfile." ; exit 1; }
	log "Success"
	log "----"
done

for testfile in $negative_tests; do
	log "Testing $testfile"
	./main.byte $testfile >> output.txt && { echo "Compilation of $testfile succeeded when failure was expected."; exit 1; }
	log "Success"
	log "----"
done
log "Tests run successfully"

echo "Build and test successful."
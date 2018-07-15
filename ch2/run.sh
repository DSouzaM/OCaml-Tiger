ocamlbuild 'parse.byte' || { echo "Failed to compile."; exit 1; }
[ -e "textoutput.txt" ] && rm testoutput.txt

for testfile in ../testcases/*; do
	echo "testing $testfile" >> testoutput.txt
	./parse.byte $testfile >> testoutput.txt || { echo "Failed to parse file $testfile." ; exit 1; }
	echo "----" >> testoutput.txt
done

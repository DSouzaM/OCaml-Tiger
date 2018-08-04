ocamlbuild 'parse.byte' || { echo "Failed to compile."; exit 1; }
[ -e "output.txt" ] && rm output.txt

for testfile in ../testcases/*; do
	echo "testing $testfile" >> output.txt
	./parse.byte $testfile >> output.txt || { echo "Failed to parse file $testfile." ; exit 1; }
	echo "----" >> output.txt
done

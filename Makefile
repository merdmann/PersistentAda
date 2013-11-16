
all :  build lib
	gnatmake -gnateDVersion=\"1.0\" -P odl
	gnatmake -gnateDVersion=\"1.0\" -A./lib -P rt


./build ./lib :
	mkdir -p $@





clean::
	gnatclean -P odl
	gnatclean -P rt
	rm -rf build



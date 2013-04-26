
test: libguidoc.dylib
	ghci -framework GUIDOEngine -lguidoc src/Guido.hs

libguidoc.dylib:
	c++ -Isrc -Iinclude -c src/guido-c.cpp
	c++ -dynamiclib \
	-current_version 1.0 -compatibility_version 1.0 -fvisibility=default \
	-framework GUIDOEngine -o libguidoc.dylib guido-c.o 

clean:
	rm -f libguidoc.dylib

test: lib
	ghci -framework GUIDOEngine -lguidoc src/Guido.hs

lib: libguidoc.dylib

libguidoc.dylib:
	c++ -Isrc -Iinclude \
	-dynamiclib -current_version 1.0 -compatibility_version 1.0 -fvisibility=default \
	-framework GUIDOEngine -o libguidoc.dylib src/guido-c.cpp

clean:
	rm -f libguidoc.dylib
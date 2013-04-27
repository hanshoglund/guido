
test: lib
	ghci -framework GUIDOEngine -lguidoc src/Guido.hs

lib: libguidoc.dylib

libguidoc.dylib:
	c++ -Isrc -Iinclude \
	-dynamiclib -current_version 1.0 -compatibility_version 1.0 -fvisibility=default \
	-framework GUIDOEngine \
	-framework QuartzCore \
	-o libguidoc.dylib src/guido-c.cpp src/guido-c-osx.m

clean:
	rm -f libguidoc.dylib
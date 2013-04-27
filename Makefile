
test: lib
	ghci -framework GUIDOEngine -lguidoc src/Guido.hs

lib: libguidoc.dylib

libguidoc.dylib:
	c++ -Isrc -Iinclude \
	`wx-config --cppflags` \
	-dynamiclib -current_version 1.0 -compatibility_version 1.0 -fvisibility=default \
	-framework GUIDOEngine \
	-framework QuartzCore \
	`wx-config --libs` \
	-o libguidoc.dylib src/guido-c.cpp src/guido-c-osx.m

clean:
	rm -f libguidoc.dylib
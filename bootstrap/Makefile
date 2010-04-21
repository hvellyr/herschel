heather: heather.o str.o port.o option.o properties.o
	$(CXX) -o $@ $^

str.o: str.cpp str.h common.h refcountable.h
	$(CXX) -g -c -Wall -o $@ $<

port.o: port.cpp port.h common.h refcountable.h ptr.h
	$(CXX) -g -c -Wall -o $@ $<

option.o: option.cpp option.h common.h str.h
	$(CXX) -g -c -Wall -o $@ $<

heather.o: heather.cpp
	$(CXX) -g -c -Wall -o $@ $<

properties.o: properties.cpp properties.h str.h
	$(CXX) -g -c -Wall -o $@ $<


heather: heather.o str.o port.o option.o
	$(CXX) -o $@ $^

str.o: str.cpp str.h common.h refcountable.h
	$(CXX) -g -c -Wall -o $@ $<

port.o: port.cpp port.h common.h refcountable.h
	$(CXX) -g -c -Wall -o $@ $<

option.o: option.cpp option.h common.h str.h
	$(CXX) -g -c -Wall -o $@ $<

heather.o: heather.cpp
	$(CXX) -g -c -Wall -o $@ $<


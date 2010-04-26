CXXFLAGS = -DUNITTESTS

heather: heather.o str.o port.o option.o properties.o tokenizer.o
	$(CXX) -o $@ $^

str.o: str.cpp str.h common.h refcountable.h
	$(CXX) $(CXXFLAGS) -g -c -Wall -o $@ $<

port.o: port.cpp port.h common.h refcountable.h ptr.h
	$(CXX) $(CXXFLAGS) -g -c -Wall -o $@ $<

option.o: option.cpp option.h common.h str.h
	$(CXX) $(CXXFLAGS) -g -c -Wall -o $@ $<

heather.o: heather.cpp
	$(CXX) $(CXXFLAGS) -g -c -Wall -o $@ $<

properties.o: properties.cpp properties.h str.h
	$(CXX) $(CXXFLAGS) -g -c -Wall -o $@ $<

tokenizer.o: tokenizer.cpp tokenizer.h port.h str.h
	$(CXX) $(CXXFLAGS) -g -c -Wall -o $@ $<



clean:
	rm *.o heather
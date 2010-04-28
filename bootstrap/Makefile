CXXFLAGS = -DUNITTESTS

heather: heather.o str.o port.o option.o properties.o tokenizer.o unittests.o tokenport.o
	$(CXX) -o $@ $^

str.o: str.cpp str.h common.h refcountable.h unittests.h
	$(CXX) $(CXXFLAGS) -g -c -Wall -o $@ $<

port.o: port.cpp port.h common.h refcountable.h ptr.h unittests.h
	$(CXX) $(CXXFLAGS) -g -c -Wall -o $@ $<

option.o: option.cpp option.h common.h str.h unittests.h
	$(CXX) $(CXXFLAGS) -g -c -Wall -o $@ $<

heather.o: heather.cpp unittests.h
	$(CXX) $(CXXFLAGS) -g -c -Wall -o $@ $<

properties.o: properties.cpp properties.h str.h
	$(CXX) $(CXXFLAGS) -g -c -Wall -o $@ $<

tokenizer.o: tokenizer.cpp tokenizer.h port.h str.h unittests.h
	$(CXX) $(CXXFLAGS) -g -c -Wall -o $@ $<

unittests.o: unittests.cpp unittests.h str.h
	$(CXX) $(CXXFLAGS) -g -c -Wall -o $@ $<

tokenport.o: tokenport.cpp tokenport.h unittests.h str.h exception.h
	$(CXX) $(CXXFLAGS) -g -c -Wall -o $@ $<



clean:
	rm *.o heather
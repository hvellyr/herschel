# Quick start

This is a lightweight C++ option parser library, supporting the standard GNU
style syntax for options.

Options can be given as:

    --long
    --long=argument
    --long argument
    -a
    -ab
    -abc argument

where c takes an argument, but a and b do not.

## Basics

    #include <cxxopts.hpp>

Create a cxxopts::Options instance.

    cxxopts::Options options;

Then use add_options.

    options.add_options()
      ("d,debug", "Enable debugging")
      ("f,file", "File name", cxxopts::value<std::string>())

Options can be declared with a short and/or long option. A description must be
provided. The third argument is the value, if omitted it is boolean. Any type
can be given as long as it can be parsed, with operator>>.

To parse the command line do:

    options.parse(argc, argv);

To retrieve an option use `options.count("option")` to get the number of times
it appeared, and

    options["opt"].as<type>()

to get its value. If "opt" doesn't exist, or isn't of the right type, then an
exception will be thrown.

## Positional Arguments

Positional arguments can be optionally parsed into one or more options. To set up positional arguments, call 

    options.parse_positional({"first", "second", "last"})

where "last" should be the name of an option with a container type, and the others should have a single value.

# Linking

This is a header only library.

# Requirements

The only build requirement is a C++ compiler that supports C++11 regular 
expressions. For example GCC >= 4.9 or clang with libc++.


# TODO list

* Allow unrecognised options.
* Various help strings.
* Unicode aware for help strings.

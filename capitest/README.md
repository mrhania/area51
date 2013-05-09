Ruby C API test
===============

Nothing but simple exercise involving writing C (C++ here) extensions in Ruby.

Contains:

* simple class `CAPITester` with `do_i_know_how_to_define_methods?` method
* an `stl_sort` extension method for `Array` which uses C++ `std::sort`

Installation
------------

    gem build capitest.gemspec
    gem install capitest

Usage
-----

    require 'capitest'
    CAPITester.do_i_know_how_to_define_methods? # => true
    [3, 10, 20, 50, 6, -1].stl_sort # => [-1, 3, 6, 10, 20, 50]

Funny fact: `Array#stl_sort` is twice as fast as `Array#sort` (even with my
dumb comparison implementation) so this gem is not as useless as it should be.

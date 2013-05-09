require 'test/unit'
require 'capitest'

class CAPIGemTest < Test::Unit::TestCase

  def test_method_defining
    assert CAPITester.do_i_know_how_to_define_methods?
  end

  def test_stl_sort
    # Test STL sort against "manual" sort.
    assert_equal [5, 10, 2, 30, 100].stl_sort, [2, 5, 10, 30, 100]
    
    # Test STL sort agains Array#sort.
    10.times do
      ary = (1..1000).to_a.shuffle
      assert_equal ary.stl_sort, ary.sort
    end

    # Only Fixnum allowed.
    assert_raise(TypeError) { ['c', 'b', 'a'].stl_sort }
  end

end

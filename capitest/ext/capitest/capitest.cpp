#include "capitest.h"

static VALUE cgem_do_i_know_how_to_define_methods_p(VALUE self)
{
	return Qtrue; /* Sure I do! */
}

static bool cgem_fixnum_cmp(const VALUE &a, const VALUE &b)
{
	if (!(FIXNUM_P(a) && FIXNUM_P(b))) {
		rb_raise(rb_eTypeError, "Only Fixnum allowed.");
		return false;
	}

	return FIX2INT(a) < FIX2INT(b);
}

static VALUE cgem_ary_stl_sort(VALUE ary)
{
	const VALUE *ptr = RARRAY_PTR(ary);
	const long len = RARRAY_LEN(ary);

	/* Create C-style copy of Ruby array to not mess up. */
	VALUE *cpy = new VALUE [len];
	memcpy(cpy, ptr, sizeof(VALUE) * len);
	
	std::sort(cpy, cpy + len, cgem_fixnum_cmp);

	/* Create Ruby array out of C-style array. */
	const VALUE sorted = rb_ary_new4(len, cpy);
	delete[] cpy;

	return sorted;
}

extern "C" {

void Init_capitest(void)
{
	VALUE cTester = rb_define_class("CAPITester", rb_cObject);

	rb_define_singleton_method(
		cTester,
		"do_i_know_how_to_define_methods?",
		(METHOD)cgem_do_i_know_how_to_define_methods_p, 0
	);

	rb_define_method(rb_cArray, "stl_sort", (METHOD)cgem_ary_stl_sort, 0);
}

}

#include "types.h"

static const char* basic_type_to_str[] {
    "i8", "i16", "i32", "float"
};

static const char* type_var_to_str[] {
    "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m",
    "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z",
};

bool is_basic_type(const Type* t) {
    return 0 <= t->type_id && t->type_id < 4;
}

bool is_type_variable(const Type* t) {
    return t->type_id < 0;
}

#ifdef _DEBUG
static char get_type_variable_letter(const Type* t) {
    assert(is_type_variable(t));
    return 'a' - t->type_id;
}

static const char* get_type_variable_str(const Type* t) {
    assert(is_type_variable(t));
    return type_var_to_str[-t->type_id - 1];
}

static const char* get_custom_type_str(const Type* t) {
    if (is_basic_type(t))
        return basic_type_to_str[t->type_id];
    assert(false /* FIXME: lookup in Type Databank! */);
}

const char* get_type_str(const Type* t) {
    if (is_type_variable(t))
        return get_type_variable_str(t);
    else
        return get_type_str(t);
}

#endif


#include "types.h"

const CustomType* get_type_for_id(int id, const TypeBank* bank) {
    assert(id <= bank->nb_types);
    return &bank->types[id];
}

const Constructor* get_constructor_for_id(int ctor, const CustomType* ty) {
    assert(ctor <= ty->nb_ctors);
    return &tu->ctors[ctor];
}

const Constructor*
get_constructor_for_ids(int type, int ctor, const TypeBank* bank) {
    assert(type <= bank->nb_types);
    return get_constructor_id(ctor, get_type_for_id(type, bank));
}

#ifdef _DEBUG
const char* get_type_str_for_id(int id, const TypeBank* bank) {
    assert(id <= bank->nb_types);
    return &bank->types[id].type_name;
}

const char* get_constructor_str_for_id(int ctor, const CustomType* ty) {
    assert(ctor <= ty->nb_ctors);
    return &tu->ctors[ctor].ctor_name;
}
#endif

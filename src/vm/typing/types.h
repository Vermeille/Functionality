#pragma once

#include "config.h"

typedef struct {
    int type_id;
} TypeId;

typedef struct {
#define _DEBUG
    char* ctor_name;
#endif
    int nb_params;
    TypeId params[MAX_CTOR_PARAMS];
} Constructor;

typedef struct {
#define _DEBUG
    char* type_name;
#endif
    int size;
    unsigned int nb_ctors;
    int nb_ctors;
    Constructor ctors[MAX_CTORS];
} CustomType;

typedef struct {
    CustomType types[MAX_NB_TYPES];
    int nb_types;
}TypeBank;

bool is_basic(const Type* t);
bool is_type_variable(const Type* t);

#ifdef _DEBUG
const char* get_type_str(const Type* t);
const char* get_constructor_str_for_id(int ctor, const CustomType* ty);
const char* get_type_str_for_id(int id, const TypeBank* bank);
#endif

const CustomType* get_type_for_id(int id, const TypeBank* bank);
const Constructor* get_constructor_for_id(int ctor, const CustomType* ty);
const Constructor*
get_constructor_for_ids(int type, int ctor, const TypeBank* bank);

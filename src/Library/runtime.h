#ifndef __RUNTIME_H
#define __RUNTIME_H

#include <inttypes.h>

typedef struct Str {
  int32_t len;
  uint8_t *data;
} Str;

typedef struct Array {
  int32_t itemSize;
  void *items;
  int32_t len;
} Array;

typedef struct __attribute__((__packed__)) Type {
  struct Type *inhParent;
  int32_t size;
  void *methods;
  int32_t *referenceOffsets;
  int32_t *referenceOffsetsSize;
} Type;

typedef struct __attribute__((__packed__)) ObjReference {
  Type *type;
  void *data;
} *obj;

obj _new(Type *t);

obj _cast(obj o, Type *t);

void *_getarritemptr(obj arrObj, int32_t ind);
obj _new_int_arr(int32_t len);
obj _new_byte_arr(int32_t len);
obj _new_obj_arr(int32_t len);
obj _new_arr(int32_t len, int32_t size);
obj _new_string(char *c);

void _null_err();

int8_t _printInt(int32_t i);
int8_t _printString(obj str);
int32_t _readInt();
obj _readString();
int8_t _error();

int32_t _String_length(obj o);
int8_t _String_equals(obj o1, obj o2);
obj _String_concat(obj o1, obj o2);
int8_t _Object_equals(obj o1, obj o2);
obj _Object_toString(obj o);

#endif
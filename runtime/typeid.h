/* -*-c-*-

   This file is part of the herschel package

   Copyright (c) 2010-2012 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#ifndef runtime_typeid_h
#define runtime_typeid_h

#include "runtime/herschel.h"

#define TYPE_TAG_ANY     0x01
#define TYPE_TAG_BOOL    0x02
#define TYPE_TAG_CHAR    0x03
#define TYPE_TAG_INT32   0x04
#define TYPE_TAG_UINT32  0x05
#define TYPE_TAG_KEYW    0x06
#define TYPE_TAG_INT16   0x07
#define TYPE_TAG_UINT16  0x08
#define TYPE_TAG_INT8    0x09
#define TYPE_TAG_UINT8   0x0a
#define TYPE_TAG_INT64   0x0b
#define TYPE_TAG_UINT64  0x0c

#define TYPE_TAG_FLOAT32 0x0d
#define TYPE_TAG_FLOAT64 0x0e

#define TYPE_TAG_STRING 0x10
#define TYPE_TAG_STRINGIMPL 0x11

#define TYPE_ARRAY_OFFSET 0x40

void h7_type_init();

size_t h7_type_slot_get(H7_Type* ty, const char* slot_name);

H7_Type* h7_type_lookup_by_name(const char* nm);
H7_Type* h7_type_lookup_by_tag(int tag_id);

/* checks whether one isa two */
int h7_type_isa(H7_Type* one, H7_Type* two);

#endif

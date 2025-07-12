#ifndef LIBRETRO_TRACE_EXTENSIONS_H
#define LIBRETRO_TRACE_EXTENSIONS_H
#include "libretro.h"

#define RETRO_ENVIRONMENT_SET_TRACE_CONTEXT     (1 | RETRO_ENVIRONMENT_PRIVATE)

struct retro_trace_ctx_t;
struct retro_trace_elem_header_t;

typedef void (*retro_trace_callback_t)(struct retro_trace_ctx_t *ctx, const void *data, size_t len);

struct retro_trace_ctx_t {
    retro_trace_callback_t callback;
    void *ctx;

    int64_t breakpoint;
    const struct retro_trace_descriptor_t *fields;
};

struct retro_trace_elem_header_t {
    size_t elem_size;
    size_t num_memory_effects;
    uint64_t cycle;
};

typedef enum {
    RETRO_TRACE_FIELD_TYPE_REG,
    RETRO_TRACE_FIELD_TYPE_PC,
    RETRO_TRACE_FIELD_TYPE_SP,
    RETRO_TRACE_FIELD_TYPE_TIMING,
} retro_trace_field_type_t;

typedef enum {
    RETRO_TRACE_FIELD_TYPE_MASK = 0x3,
} retro_trace_field_flags_t;


struct retro_trace_descriptor_bit_t {
    const char *name;
    const char *desc;
    uint64_t mask;
};

struct retro_trace_descriptor_t {
    const char *name;
    size_t len;
    size_t offset;
    retro_trace_field_flags_t flags;

    struct retro_trace_descriptor_bit_t *bits;
};

struct retro_trace_memory_effect_t {
    uint64_t address;
    uint64_t data;
    uint64_t mask;
};

#endif /*LIBRETRO_TRACE_EXTENSIONS_H*/

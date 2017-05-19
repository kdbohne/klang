#pragma once

#include "common.h"
#include "string.h"

// TODO: what size?
#define HASH_MAP_CAPACITY 4096
#define HASH_MAP_EMPTY_KEY UINT64_MAX

template<typename T>
struct HashMap
{
    u64 keys[HASH_MAP_CAPACITY];
    T values[HASH_MAP_CAPACITY];

    HashMap()
    {
        for (int i = 0; i < HASH_MAP_CAPACITY; ++i)
            keys[i] = HASH_MAP_EMPTY_KEY;
    }

    T *get_(u64 key)
    {
        i32 i = (i32)(key % HASH_MAP_CAPACITY);
        i32 i_start = i;

        while (true)
        {
            if (keys[i] == HASH_MAP_EMPTY_KEY)
                break;

            if (keys[i] == key)
                return &values[i];

            i = (i + 1) % HASH_MAP_CAPACITY;
            if (i == i_start)
            {
                // TODO: error message
                assert(false);
                break;
            }
        }

        return NULL;
    }

    T *get(const char *str)
    {
        u64 hash = hash_djb2(str);
        return get_(hash);
    }

    void set_(u64 key, T &value)
    {
        i32 i = (i32)(key % HASH_MAP_CAPACITY);
        i32 i_start = i;

        while (true)
        {
            if (keys[i] == HASH_MAP_EMPTY_KEY)
            {
                keys[i] = key;
                values[i] = value;
                break;
            }

            i = (i + 1) % HASH_MAP_CAPACITY;
            if (i == i_start)
            {
                // TODO: error message
                assert(false);
                break;
            }
        }
    }

    void set(const char *str, T &value)
    {
        u64 hash = hash_djb2(str);
        set_(hash, value);
    }
};

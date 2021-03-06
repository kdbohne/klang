#pragma once

#include "common.h"

extern "C"
{
    void *malloc(u64 size);
    void free(void *ptr);
    void *memcpy(void *dest, const void *src, u64 n);
}

#define ARRAY_INITIAL_CAPACITY 8
#define ARRAY_GROWTH_RATE      1.5

template<typename T>
struct Array
{
    T *data = NULL;
    i64 count = 0;
    i64 capacity = 0;

    Array() : data(NULL), count(0), capacity(0)
    {
    }

    ~Array()
    {
        if (data)
        {
            // FIXME
//            free(data);
//            delete[] data;
        }
    }

    void grow_if_needed()
    {
        if (count < capacity)
            return;

        if (capacity == 0)
        {
            capacity = ARRAY_INITIAL_CAPACITY;

//            data = (T *)malloc(sizeof(T) * capacity);
            data = new T[capacity];
        }
        else
        {
            capacity *= ARRAY_GROWTH_RATE;

//            T *new_data = (T *)malloc(sizeof(T) * capacity);
            T *new_data = new T[capacity];
            memcpy(new_data, data, sizeof(T) * count);

//            free(data);
            delete[] data;

            data = new_data;
        }
    }

    void add(T value)
    {
        grow_if_needed();

        data[count++] = value;
    }

    T *next()
    {
        grow_if_needed();

        return &data[count++];
    }

    // NOTE: This does not free any memory. It is only used when duplicating
    // an existing array in src/type.cpp:duplicate_node().
    void reset()
    {
        data = NULL;
        count = 0;
        capacity = 0;
    }

    T *begin()
    {
        return &data[0];
    }

    T *end()
    {
        return &data[count];
    }

    T &operator[](i64 i)
    {
        // TODO: bounds checking?
        return data[i];
    }
};

#ifndef VSHARP_COVERAGEINSTRUMENTER_SERIALIZATION_H
#define VSHARP_COVERAGEINSTRUMENTER_SERIALIZATION_H

#include <vector>

template <typename T> void serializePrimitive(const T obj, std::vector<char>& v) {
    static_assert(std::is_fundamental<T>::value || std::is_enum<T>::value,"Can only serialize primitive objects.");
    auto size = v.size();
    v.resize(size + sizeof(T));
    std::memcpy(&v[size], &obj, sizeof(T));
}

template <typename T> void serializePrimitiveArray(const T *obj, size_t len, std::vector<char>& v) {
    static_assert(std::is_fundamental<T>::value || std::is_enum<T>::value,"Can only serialize primitive objects.");
    auto size = v.size();
    v.resize(size + sizeof(T) * len);
    std::memcpy(&v[size], obj, sizeof(T) * len);
}

#endif //VSHARP_COVERAGEINSTRUMENTER_SERIALIZATION_H

// Copyright (c) 2016 Gregor Klinke

#pragma once

#if defined(HAVE_STD_OPTIONAL)
#  include <optional>
#endif

#include <exception>
#include <utility>


namespace herschel { namespace estd {

#if !defined(HAVE_STD_OPTIONAL)

  class bad_optional_access : public std::exception {
  public:
    const char* what() const noexcept override { return "bad access to optional"; }
  };


  template <typename T>
  class optional {
  public:
    optional() = default;
    optional(T val)
        : _value(std::move(val))
        , _is_set(true)
    {
    }

    optional(const optional<T>& rhs) = default;

#  if defined(IS_VS2013)
    optional(optional<T>&& rhs)
        : _value(std::move(rhs._value))
        , _is_set(rhs._is_set)
    {
    }
#  else
    optional(optional<T>&&) = default;
#  endif

    ~optional() { reset(); }

    optional<T>& operator=(const optional<T>& rhs) = default;

#  if defined(IS_VS2013)
    optional<T>& operator=(optional<T>&& rhs)
    {
      _value = std::move(rhs._value);
      _is_set = rhs._is_set;
      return *this;
    }
#  else
    optional<T>& operator=(optional<T>&&) = default;
#  endif

    explicit operator bool() const { return _is_set; }

    const T& value() const
    {
      if (!_is_set)
        throw bad_optional_access();
      return _value;
    }
    T& value()
    {
      if (!_is_set)
        throw bad_optional_access();
      return _value;
    }

    void reset()
    {
      if (_is_set) {
        _is_set = false;
        _value.T::~T();
      }
    }

    const T& operator*() const { return _value; }
    T& operator*() { return _value; }

    const T* operator->() const { return &_value; }
    T* operator->() { return &_value; }

  private:
    T _value;
    bool _is_set = false;
  };

#else

  using std::optional;

#endif

}}  // namespace herschel::estd

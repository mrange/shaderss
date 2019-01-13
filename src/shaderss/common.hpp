#pragma once

#include <windows.h>

#include <exception>
#include <stdexcept>
#include <string>

#define STRINGIFY_(x) #x
#define STRINGIFY(x) STRINGIFY_(x)

#define CHECK(expr) check (expr, (__FILE__ "(" STRINGIFY(__LINE__) "): Check failed for - " #expr))
#define CHECK_HR(expr) check_hr (expr, (__FILE__ "(" STRINGIFY(__LINE__) "): Check hr failed for - " #expr))

struct unit_t
{
};
extern unit_t unit;

struct empty_t
{
};
extern empty_t empty;

std::string utf8_encode (const std::wstring &wstr);

template<typename T>
auto check (T && v, char const * msg)
{
  if (!(v))
  {
    OutputDebugStringA (msg);
    OutputDebugStringA ("\n");
    throw std::runtime_error (msg);
  }

  return std::forward<T> (v);
}

HRESULT check_hr (HRESULT hr, char const * msg);

template<typename T>
struct com_out_ptr;

template<typename TExit>
struct on_exit
{
  on_exit ()                            = delete;
  on_exit (on_exit const &)             = delete;

  on_exit& operator= (on_exit const &)  = delete;
  on_exit& operator= (on_exit &&)       = delete;

  on_exit (TExit && e) noexcept
    : exit (std::move (e))
  {
  }

  // Enables move .ctor eliding
  on_exit (on_exit && o) noexcept;

  ~on_exit () noexcept
  {
    exit ();
  }
private:
  TExit exit    ;
};

template<typename TExit>
auto on_exit_do (TExit && e) noexcept
{
  return on_exit<std::decay_t<TExit>> (std::forward<TExit> (e));
}

template<typename T>
struct com_ptr
{
  com_ptr () noexcept
    : ptr (nullptr)
  {
  }

  com_ptr (empty_t) noexcept
    : ptr (nullptr)
  {
  }

  ~com_ptr () noexcept
  {
    if (ptr)
    {
      ptr->Release ();
      ptr = nullptr;
    }
  }

  explicit com_ptr (T* p) noexcept
    : ptr (p)
  {
    if (ptr)
    {
      ptr->AddRef ();
    }
  }

  com_ptr (com_ptr const & cp) noexcept
    : ptr (cp.ptr)
  {
    if (ptr)
    {
      ptr->AddRef ();
    }
  }

  com_ptr (com_ptr && cp) noexcept
    : ptr (cp.ptr)
  {
    cp.ptr = nullptr;
  }

  com_ptr& operator= (com_ptr const & cp) noexcept
  {
    com_ptr that (cp);

    swap (that);

    return *this;
  }

  com_ptr& operator= (com_ptr && cp) noexcept
  {
    com_ptr that (std::move (cp));

    swap (that);

    return *this;
  }

  bool operator! () const noexcept
  {
    return !ptr;
  }

  void swap (com_ptr & cp) noexcept
  {
    std::swap (ptr, cp.ptr);
  }

  void attach_no_addref (T* p) noexcept
  {
    if (ptr)
    {
      ptr->Release ();
      ptr = nullptr;
    }

    ptr = p;
  }


  T* get () noexcept
  {
    return ptr;
  }

  T* get () const noexcept
  {
    return ptr;
  }

  T* operator-> () noexcept
  {
    return get ();
  }

  T* operator-> () const noexcept
  {
    return get ();
  }

  com_out_ptr<T> out () noexcept
  {
    return com_out_ptr<T> (this);
  }

private:
  T* ptr;
};

template<typename T>
struct com_out_ptr
{
  com_out_ptr (com_ptr<T> * v) noexcept
    : receiver  (v)
    , ptr       (nullptr)
  {
  }

  com_out_ptr (com_out_ptr<T> const &)            = delete;
  com_out_ptr& operator= (com_out_ptr<T> const &) = delete;
  com_out_ptr& operator= (com_out_ptr<T> &&)      = delete;

  // Enables move .ctor eliding
  com_out_ptr (com_out_ptr<T> &&) noexcept;

  operator void** () noexcept
  {
    return reinterpret_cast<void**> (&ptr);
  }

  operator T** () noexcept
  {
    return &ptr;
  }

  ~com_out_ptr () noexcept
  {
    if (receiver)
    {
      receiver->attach_no_addref (ptr);
      receiver  = nullptr;
      ptr       = nullptr;
    }
  }

private:
  com_ptr<T>*   receiver;
  T*            ptr     ;
};

template<typename T>
com_ptr<T> cocreate_instance (IID const & clsid)
{
  com_ptr<T> ptr;

  CHECK_HR (CoCreateInstance(
      clsid
    , nullptr
    , CLSCTX_INPROC_SERVER
    , __uuidof (T)
    , ptr.out ()
    ));

  return ptr;
}

void inplace_rtrim (std::wstring& s);

void inplace_ltrim (std::wstring& s);

void inplace_trim (std::wstring& s);

float to_float (std::wstring const & s, float default_to);

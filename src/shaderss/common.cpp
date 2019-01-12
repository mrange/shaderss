#include "stdafx.h"

#include <type_traits>

#include "common.hpp"

namespace
{

  wchar_t const raw__whitespaces[] = L" \t\n\r\f\v";
  // We want the trailing \0 as well
  std::wstring const whitespaces (raw__whitespaces, std::extent<decltype(raw__whitespaces)>::value);
}

unit_t unit;
empty_t empty;

HRESULT check_hr (HRESULT hr, char const * msg)
{
  if (FAILED(hr))
  {
    throw std::runtime_error (msg);
  }

  return hr;
}


std::string utf8_encode (const std::wstring &wstr)
{
  if (wstr.empty())
  {
    return std::string();
  }

  auto size_needed = WideCharToMultiByte (
      CP_UTF8
    , 0
    , wstr.c_str ()
    , (int)wstr.size()
    , nullptr
    , 0
    , nullptr
    , nullptr
    );

  std::string strTo (size_needed, 0);
  WideCharToMultiByte (
      CP_UTF8
    , 0
    , wstr.c_str ()
    , (int)wstr.size()
    , &strTo.front ()
    , size_needed
    , nullptr
    , nullptr
    );
  return strTo;
}


void inplace_rtrim (std::wstring& s)
{
  s.erase (s.find_last_not_of(whitespaces) + 1);
}

void inplace_ltrim (std::wstring& s)
{
  s.erase (0, s.find_first_not_of(whitespaces));
}

void inplace_trim (std::wstring& s)
{
  inplace_ltrim (s);
  inplace_rtrim (s);
}

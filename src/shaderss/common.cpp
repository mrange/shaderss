#include "stdafx.h"

#include "common.hpp"

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


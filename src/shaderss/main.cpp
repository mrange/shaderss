#include "stdafx.h"
#include "resource.h"

#include <windows.h>
#include <commctrl.h>
#include <wincodec.h>
#include <GL/gl.h>

#include <algorithm>
#include <cassert>
#include <memory>
#include <regex>
#include <stdexcept>
#include <string>
#include <utility>

#include "glext.h"

#include "common.hpp"

#pragma comment(lib, "Comctl32")

#pragma comment(linker, "\"/manifestdependency:type='win32' name='Microsoft.Windows.Common-Controls' version='6.0.0.0' processorArchitecture='*' publicKeyToken='6595b64144ccf1df' language='*'\"")

extern int show__screen_saver (int nCmdShow, bool screen_saver_mode);
extern int show__config_dialog (HWND parent);

namespace
{
  HINSTANCE hinst;
}

HINSTANCE get__hinstance () noexcept
{
  return hinst;
}

extern "C"
{
int APIENTRY wWinMain (
    HINSTANCE hInstance
  , HINSTANCE hPrevInstance
  , LPWSTR    lpCmdLine
  , int       nCmdShow
  )
{
  try
  {
    hinst = hInstance; // Store instance handle in our global variable

    CHECK (SetProcessDPIAware ());

    CHECK_HR (CoInitialize (0));
    auto on_exit__co_unitialize = on_exit_do ([] { CoUninitialize (); });

    InitCommonControls ();

    std::wstring command_line (lpCmdLine);
    std::wregex re_commands (LR"*(^\s*(()|(/dev)|(/c)|(/s)|/p (\d+)|/c:(\d+))\s*$)*", std::regex_constants::ECMAScript | std::regex_constants::icase);

    auto invalid_command_line_msg = std::string ("Invalid argument, expecting /dev, /c, /c:<HWND>, /s or /p <HWND>\r\n") + utf8_encode (command_line);

    std::wcmatch match;
    if (!std::regex_match (command_line.c_str (), match, re_commands))
    {
      throw std::runtime_error (invalid_command_line_msg.c_str ());
    }

    assert (match.size () == 8);

    if (match[2].matched)
    {
      // No arg - Show config
      return 1;
    }
    else if (match[3].matched)
    {
      // /dev - Show screen saver in window
      show__screen_saver (nCmdShow, false);
      return 0;
    }
    else if (match[4].matched)
    {
      // /c - Show config modal

      show__config_dialog (nullptr);

      return 1;
    }
    else if (match[5].matched)
    {
      // /s - Show screen saver in full screen
      show__screen_saver (nCmdShow, true);
      return 0;
    }
    else if (match[6].matched)
    {
      // /p <HWND> - Show screen saver attached to HWND
      return 1;
    }
    else if (match[7].matched)
    {
      // /c:<HWND> - Show config modal attached to HWND

      // TODO: Parse HWND
      show__config_dialog (nullptr);

      return 1;
    }
    else
    {
      throw std::runtime_error (invalid_command_line_msg.c_str ());
    }
  }
  catch (std::exception const & e)
  {
    MessageBoxA (nullptr, e.what (), "Shader Screen Saver Crashed", MB_OK|MB_ICONERROR);
    return 98;
  }
  catch (...)
  {
    MessageBoxW (nullptr, L"Unrecognized exception caught", L"Shader Screen Saver Crashed", MB_OK|MB_ICONERROR);
    return 99;
  }
}
}

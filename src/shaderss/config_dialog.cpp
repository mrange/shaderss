#include "stdafx.h"

#include <Windows.h>

#include "resource.h"

#include "config.hpp"

HINSTANCE get__hinstance () noexcept;

namespace
{
  HWND                dlg                                                 ;
  std::wstring  const view__shader  = L"https://www.shadertoy.com/view/"  ;

  void set__text (int id, std::wstring const & t)
  {
    HWND hwnd = CHECK (GetDlgItem (dlg, id));
    CHECK (SetWindowTextW (hwnd, t.c_str ()));
  }

  void set__shader_info (shader_info const & si)
  {
    set__text (IDC_SHADER_LINK          , view__shader + si.id);
    set__text (IDC_SHADER_AUTHOR        , si.author           );
    set__text (IDC_SHADER_LICENSE       , si.license          );
    set__text (IDC_SHADER_MODIFICATIONS , si.modifications    );
    set__text (IDC_SHADER_NOTES         , si.notes            );
  }

  void set__configuration (shader_configuration const & sc)
  {
    set__shader_info (sc.shader_info);
    set__text (IDC_START_TIME           , std::to_wstring (sc.start_time) );
    set__text (IDC_SPEED                , std::to_wstring (sc.speed)      );
    set__text (IDC_IMAGE_PATH           , sc.image_path                   );
  }

  LRESULT CALLBACK WndProc(HWND hwnd, UINT message, WPARAM wParam, LPARAM lParam)
  {
    switch (message)
    {
    case WM_INITDIALOG:
      {
        dlg = hwnd;
        auto cfg = get__current_configuration ();
        set__configuration (cfg);
        return (LRESULT)TRUE;
      }
    case WM_COMMAND:
      {
        if (LOWORD(wParam) == IDOK || LOWORD(wParam) == IDCANCEL)
        {
            EndDialog(hwnd, LOWORD(wParam));
            return (LRESULT)TRUE;
        }
        break;
      }
    }
    return (LRESULT)FALSE;
  }
}

int show__config_dialog (HWND parent)
{
  DialogBoxW (get__hinstance (), MAKEINTRESOURCE(IDD_CONFIGDIALOG), parent, &WndProc);

  return 0;
}


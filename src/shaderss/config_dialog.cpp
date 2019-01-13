#include "stdafx.h"

#include <Windows.h>
#include <windowsx.h>
#include <shobjidl_core.h>

#include "resource.h"

#include "config.hpp"

HINSTANCE get__hinstance () noexcept;

namespace
{
  HWND                dlg                                                 ;
  std::wstring  const view__shader  = L"https://www.shadertoy.com/view/"  ;

  std::wstring get__text (int id)
  {
    auto hwnd         = CHECK (GetDlgItem (dlg, id));
    auto text_length  = Edit_GetTextLength (hwnd);
    if (text_length > 0)
    {
      std::wstring text;
      // +1 for '\0'
      text.resize (text_length + 1);

      CHECK (Edit_GetText (hwnd, &text.front (), text_length + 1));

      // Remove '\0'
      text.erase (text_length);

      return text;
    }
    else
    {
      return std::wstring ();
    }
  }

  void set__text (int id, std::wstring const & t)
  {
    auto hwnd = CHECK (GetDlgItem (dlg, id));
    CHECK (Edit_SetText (hwnd, t.c_str ()));
  }

  void set__shader_infos (shader_infos const & sis)
  {
    auto hwnd = CHECK (GetDlgItem (dlg, IDC_SELECT_SHADER));

    for (auto const & si : sis)
    {
      CHECK (CB_ERR != ComboBox_AddString (hwnd, si.short_description.c_str ()));
    }

    CHECK (CB_ERR != ComboBox_SetCurSel (hwnd, 0));
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

  COMDLG_FILTERSPEC file_types[] =
  {
    { L"Image files" , L"*.jpg;*.jpeg;*.png" },
  };

  std::wstring find__image_file (HWND parent, std::wstring const & current_file)
  {
    auto file_dialog = cocreate_instance<IFileDialog> (CLSID_FileOpenDialog);

    FILEOPENDIALOGOPTIONS options {};
    CHECK_HR (file_dialog->GetOptions (&options));
    options |= FOS_STRICTFILETYPES | FOS_FORCEFILESYSTEM;
    CHECK_HR (file_dialog->SetOptions (options));

    CHECK_HR (file_dialog->SetFileTypes (std::extent <decltype(file_types)>::value, file_types));
    CHECK_HR (file_dialog->SetFileTypeIndex (0));
    CHECK_HR (file_dialog->SetDefaultExtension (file_types[0].pszSpec));

    CHECK_HR (file_dialog->SetTitle (L"Select an image file"));
    CHECK_HR (file_dialog->SetFileName (current_file.c_str ()));

    auto show_result = file_dialog->Show (parent);

    if (SUCCEEDED (show_result))
    {
      com_ptr<IShellItem> shell_item;
      CHECK_HR (file_dialog->GetResult (shell_item.out ()));

      PWSTR file_name {};
      CHECK_HR (shell_item->GetDisplayName (SIGDN_FILESYSPATH, &file_name));
      auto on_exit__free_file_name = on_exit_do ([file_name] {CoTaskMemFree (file_name); });

      return std::wstring (file_name);
    }
    else
    {
      return std::wstring ();
    }
  }

  LRESULT CALLBACK WndProc(HWND hwnd, UINT message, WPARAM wParam, LPARAM lParam)
  {
    LRESULT good = 1;
    LRESULT bad  = 0;
    switch (message)
    {
    case WM_INITDIALOG:
      {
        dlg = hwnd;

        set__shader_infos (get__shader_infos ());

        auto cfg = get__current_configuration ();
        set__configuration (cfg);
        return good;
      }
    case WM_COMMAND:
      {
        auto wlow   = LOWORD(wParam);
        auto whigh  = HIWORD(wParam);
        if (wlow == IDC_SELECT_SHADER && whigh == CBN_SELCHANGE)
        {
          auto hwnd = CHECK (GetDlgItem (dlg, wlow));
          auto i = ComboBox_GetCurSel (hwnd);
          auto const & sis = get__shader_infos ();
          auto const & si = sis.at (i);
          set__shader_info (si);
          return good;
        }
        else if (wlow == IDC_FIND_IMAGE && whigh == BN_CLICKED)
        {
          auto current_file = get__text (IDC_IMAGE_PATH);
          auto new_file     = find__image_file (dlg, current_file);
          if (!new_file.empty ())
          {
            set__text (IDC_IMAGE_PATH, new_file);
          }
          return good;
        }
        else if ((wlow == IDC_START_TIME || wlow == IDC_SPEED) && whigh == EN_KILLFOCUS)
        {
          auto hwnd = CHECK (GetDlgItem (dlg, wlow));
          auto text = get__text (wlow);

          if (std::isnan (to_float (text, NAN)))
          {
            std::wstring msg = text + L" is not a valid number. Excepted numbers like 2 or 3.5";
            MessageBoxW (dlg, msg.c_str (), L"Expected valid number", MB_OK|MB_ICONERROR);
          }

          return good;
        }
        else if (wlow == IDOK || wlow == IDCANCEL)
        {
          EndDialog(hwnd, wlow);
          return good;
        }
        break;
      }
    }
    return bad;
  }
}

int show__config_dialog (HWND parent)
{
  DialogBoxW (get__hinstance (), MAKEINTRESOURCE(IDD_CONFIGDIALOG), parent, &WndProc);

  return 0;
}


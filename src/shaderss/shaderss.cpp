#include "stdafx.h"
#include "resource.h"

#include <windows.h>
#include <GL/gl.h>

#include <cassert>
#include <memory>
#include <regex>
#include <stdexcept>
#include <string>
#include <utility>

#include "glext.h"

#pragma comment(lib, "Opengl32")

char const * const get_vertex_shader ();
char const * const get_fragment_shader ();

namespace
{
#define MAX_LOADSTRING 100

#define STRINGIFY_(x) #x
#define STRINGIFY(x) STRINGIFY_(x)

HINSTANCE   hinst             ;
HWND        hwnd              ;
HDC         hdc               ;
HGLRC       hrc               ;
bool        done              ;
LONG        width             ;
LONG        height            ;
ULONGLONG   start             ;
bool        screen_saver_mode ;

PIXELFORMATDESCRIPTOR pfd =
{
  sizeof(PIXELFORMATDESCRIPTOR)                         ,
  1                                                     ,
  PFD_DRAW_TO_WINDOW|PFD_SUPPORT_OPENGL|PFD_DOUBLEBUFFER,
  PFD_TYPE_RGBA                                         ,
  32                                                    ,
  0, 0, 0, 0, 0, 0, 8, 0                                ,
  0, 0, 0, 0, 0                                         , // accum
  32                                                    , // zbuffer
  0                                                     ,  // stencil!
  0                                                     ,  // aux
  PFD_MAIN_PLANE                                        ,
  0, 0, 0, 0                                            ,
};


WCHAR       szTitle[MAX_LOADSTRING]             ; // The title bar text
WCHAR const window_class_name[] = L"SHADER_SS"  ; // the main window class name

GLuint pid  ;
GLuint fsid ;
GLuint vsid ;

constexpr int gl_functions_count = 7;

char const * const gl_names[gl_functions_count] =
{
  "glCreateShaderProgramv",
  "glGenProgramPipelines" ,
  "glBindProgramPipeline" ,
  "glUseProgramStages"    ,
  "glProgramUniform4fv"   ,
  "glGetProgramiv"        ,
  "glGetProgramInfoLog"   ,
};

void * gl_functions[gl_functions_count];

#define oglCreateShaderProgramv         ((PFNGLCREATESHADERPROGRAMVPROC)  gl_functions[0])
#define oglGenProgramPipelines          ((PFNGLGENPROGRAMPIPELINESPROC)   gl_functions[1])
#define oglBindProgramPipeline          ((PFNGLBINDPROGRAMPIPELINEPROC)   gl_functions[2])
#define oglUseProgramStages             ((PFNGLUSEPROGRAMSTAGESPROC)      gl_functions[3])
#define oglProgramUniform4fv            ((PFNGLPROGRAMUNIFORM4FVPROC)     gl_functions[4])
#define oglGetProgramiv                 ((PFNGLGETPROGRAMIVPROC)          gl_functions[5])
#define oglGetProgramInfoLog            ((PFNGLGETPROGRAMINFOLOGPROC)     gl_functions[6])

template<typename T>
auto check (T && v, char const * msg)
{
  if (!(v))
  {
    OutputDebugStringA (msg);
    throw std::runtime_error (msg);
  }

  return std::forward<T> (v);
}

int check_link_status (int id, char const * msg)
{
  int result;
  oglGetProgramiv (id, GL_LINK_STATUS, &result);
  if (!result)
  {
    char    info[1536];

    oglGetProgramInfoLog (id, 1024, nullptr, info);
    OutputDebugStringA (msg);
    OutputDebugStringA (info);
    throw std::runtime_error (msg);
  }

  return id;
}

#define CHECK(expr) check (expr, (__FILE__ "(" STRINGIFY(__LINE__) "): Check failed for - " #expr))

#define CHECK_LINK_STATUS(expr) check_link_status (expr, (__FILE__ "(" STRINGIFY(__LINE__) "): Check link status failed for - " #expr))

LRESULT CALLBACK window_proc (HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
  auto kill = [] ()
  {
    done = true;
    PostQuitMessage (0);
  };

  auto now = GetTickCount64 () - start;
  auto screen_saver_check = screen_saver_mode && (now > 1000);

  switch (message)
  {
  case WM_COMMAND:
    {
      int wmId = LOWORD (wParam);
      // Parse the menu selections:
      switch (wmId)
      {
      default:
        return DefWindowProc (hWnd, message, wParam, lParam);
      }
    }
    return 0;
  case WM_SIZE:
    width  = LOWORD (lParam);
    height = HIWORD (lParam);
    glViewport (0, 0, width, height);
    return 0;
  case WM_PAINT:
    {
      PAINTSTRUCT ps;
      HDC hdc = BeginPaint (hWnd, &ps);
      // TODO: Add any drawing code that uses hdc here...
      EndPaint (hWnd, &ps);
    }
    return 0;
  case WM_DESTROY:
    kill ();
    return 0;
  case WM_ACTIVATE:
  case WM_ACTIVATEAPP:
  case WM_NCACTIVATE:
    if (screen_saver_check && !wParam)
    {
      kill ();
      return 0;
    }
    else
    {
      return DefWindowProc (hWnd, message, wParam, lParam);
    }
  case WM_LBUTTONDOWN:
  case WM_RBUTTONDOWN:
  case WM_MBUTTONDOWN:
  case WM_KEYDOWN:
  case WM_KEYUP:
  case WM_MOUSEMOVE:
    if (screen_saver_check)
    {
      kill ();
      return 0;
    }
    else
    {
      return DefWindowProc (hWnd, message, wParam, lParam);
    }
  case WM_SETCURSOR:
    if (screen_saver_check)
    {
      return DefWindowProc (hWnd, message, 0, lParam);  // Clears wParam
    }
    else
    {
      return DefWindowProc (hWnd, message, wParam, lParam);
    }
  case WM_SYSCOMMAND:
    if (screen_saver_check && (wParam == SC_CLOSE || wParam == SC_SCREENSAVE))
    {
      return false;
    }
    else
    {
      return DefWindowProc (hWnd, message, wParam, lParam);
    }
  default:
    return DefWindowProc (hWnd, message, wParam, lParam);
  }
}

ATOM register_class ()
{
  WNDCLASSEXW wcex;

  wcex.cbSize = sizeof (WNDCLASSEX);

  wcex.style          = CS_HREDRAW | CS_VREDRAW;
  wcex.lpfnWndProc    = window_proc;
  wcex.cbClsExtra     = 0;
  wcex.cbWndExtra     = 0;
  wcex.hInstance      = hinst;
  wcex.hIcon          = LoadIcon (hinst, MAKEINTRESOURCE (IDI_SHADERSS));
  wcex.hCursor        = LoadCursor (nullptr, IDC_ARROW);
  wcex.hbrBackground  = (HBRUSH) (COLOR_WINDOW+1);
  wcex.lpszMenuName   = nullptr;
  wcex.lpszClassName  = window_class_name;
  wcex.hIconSm        = LoadIcon (wcex.hInstance, MAKEINTRESOURCE (IDI_SMALL));

  return CHECK (RegisterClassExW (&wcex));
}

void init_window (int nCmdShow)
{
  hwnd = CHECK (CreateWindowExW (
      0
    , window_class_name
    , szTitle
    , WS_VISIBLE | WS_OVERLAPPEDWINDOW
    , CW_USEDEFAULT
    , CW_USEDEFAULT
    , CW_USEDEFAULT
    , CW_USEDEFAULT
    , nullptr
    , nullptr
    , hinst
    , nullptr
    ));

  ShowWindow (hwnd, nCmdShow);
  CHECK (UpdateWindow (hwnd));

  if (screen_saver_mode)
  {
    auto cx = GetSystemMetrics (SM_CXSCREEN);
    auto cy = GetSystemMetrics (SM_CYSCREEN);

    auto style = GetWindowLongW (hwnd, GWL_STYLE);
    style &= ~(WS_CAPTION | WS_THICKFRAME | WS_MINIMIZEBOX | WS_MAXIMIZEBOX | WS_SYSMENU);
    SetWindowLongW (hwnd, GWL_STYLE, style);

    auto ex_style = GetWindowLongW (hwnd, GWL_EXSTYLE);
    ex_style &= ~(WS_EX_DLGMODALFRAME | WS_EX_CLIENTEDGE | WS_EX_STATICEDGE);
    SetWindowLongW (hwnd, GWL_EXSTYLE, ex_style);
    SetWindowPos (
        hwnd
      , nullptr
      , 0
      , 0
      , cx
      , cy
      , SWP_FRAMECHANGED | SWP_NOZORDER | SWP_NOOWNERZORDER
      );
  }
}


void init_opengl ()
{
  hdc = CHECK (GetDC(hwnd));

  auto pf = CHECK (ChoosePixelFormat (hdc,&pfd));

  CHECK (SetPixelFormat (hdc,pf,&pfd));

  hrc = CHECK (wglCreateContext (hdc));

  CHECK (wglMakeCurrent(hdc, hrc));

  for (auto i = 0; i < gl_functions_count; ++i)
  {
    gl_functions[i] = CHECK (wglGetProcAddress(gl_names[i]));
  }

  auto vsh = get_vertex_shader ();
  auto fsh = get_fragment_shader ();

  vsid = oglCreateShaderProgramv (GL_VERTEX_SHADER, 1, &vsh);
  fsid = oglCreateShaderProgramv (GL_FRAGMENT_SHADER, 1, &fsh);

  oglGenProgramPipelines (1, &pid);
  oglBindProgramPipeline (pid);
  oglUseProgramStages (pid, GL_VERTEX_SHADER_BIT, vsid);
  oglUseProgramStages (pid, GL_FRAGMENT_SHADER_BIT, fsid);

  CHECK_LINK_STATUS (vsid);
  CHECK_LINK_STATUS (fsid);
  CHECK_LINK_STATUS (pid);
}

void draw_gl (std::uint64_t now)
{
  auto t = 0.001f*now;

  float fparams[4]
  {
    t         ,
    width*1.f ,
    height*1.f,
    0         ,
  };

  oglProgramUniform4fv (fsid, 0, 1, fparams);

  glRects (-1, -1, 1, 1);
}

int show_screen_saver (int nCmdShow)
{
  register_class ();

  init_window (nCmdShow);

  init_opengl ();

  HACCEL hAccelTable = LoadAccelerators (hinst, MAKEINTRESOURCE (IDC_SHADERSS));

  MSG msg;

  {
    RECT client;

    CHECK (GetClientRect(hwnd, &client));
    width  = client.right - client.left;
    height = client.bottom - client.top;
  }

  start = GetTickCount64 ();

  // Main message loop:
  while (true)
  {
    while (!done && PeekMessage (&msg, 0, 0, 0, PM_REMOVE))
    {
      if (!TranslateAccelerator (msg.hwnd, hAccelTable, &msg))
      {
        TranslateMessage (&msg);
        DispatchMessage (&msg);
      }
    }

    if (done) break;

    auto now = GetTickCount64 () - start;

    draw_gl (now);

    SwapBuffers (hdc);
    Sleep (1);
  }

  return msg.wParam;
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

    CHECK (LoadStringW (hInstance, IDS_APP_TITLE, szTitle, MAX_LOADSTRING));

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
      show_screen_saver (nCmdShow);
      return 0;
    }
    else if (match[4].matched)
    {
      // /c - Show config modal
      return 1;
    }
    else if (match[5].matched)
    {
      // /s - Show screen saver in full screen
      screen_saver_mode = true;
      show_screen_saver (nCmdShow);
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
